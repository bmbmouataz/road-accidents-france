# Part 6 - Machine learning: predicting who is killed or hospitalised.
#
# Chronological split, so the models are judged on years they have never seen:
#   train 2019-2022, validation 2023 (tuning, early stopping, thresholds), test 2024-2025.
# The final models are refitted on 2019-2023 with the tuned settings and scored on 2024-2025.
#
# Models: two baselines, penalised logistic regression (glmnet), random forest (ranger),
# XGBoost. Two feature sets: PRE_IMPACT (what is known before the crash) and
# PRE_IMPACT + CRASH (collision type, obstacle, impact point, manoeuvre).

source("R/00_utils.R")
source("R/00_model_data.R")
suppressPackageStartupMessages({
  library(Matrix)
  library(glmnet)
  library(ranger)
  library(xgboost)
})
set.seed(2026)
THREADS <- max(1, parallel::detectCores() - 1)

df <- model_data()
split <- case_when(df$year_num <= 2022 ~ "train", df$year_num == 2023 ~ "valid", .default = "test")
idx <- split(seq_len(nrow(df)), split)
idx$trainval <- c(idx$train, idx$valid)
y <- df$severe
yn <- as.numeric(y)
message(sprintf("train %s, valid %s, test %s rows; severe rate %.1f%% / %.1f%% / %.1f%%",
                format(length(idx$train), big.mark = ","), format(length(idx$valid), big.mark = ","),
                format(length(idx$test), big.mark = ","), 100 * mean(y[idx$train]),
                100 * mean(y[idx$valid]), 100 * mean(y[idx$test])))

metrics <- function(p, y) {
  tibble(roc_auc = auc(p, y), pr_auc = pr_auc(p, y), log_loss = log_loss(p, y), brier = brier(p, y))
}

design <- function(features) {
  f <- reformulate(features, intercept = FALSE)
  sparse.model.matrix(f, data = df)
}
X <- list(pre = design(PRE_IMPACT), crash = design(c(PRE_IMPACT, CRASH)))
message("Design matrices: ", ncol(X$pre), " and ", ncol(X$crash), " columns")

tuning <- list()
log_tuning <- function(model, features, setting, p) {
  tuning[[length(tuning) + 1]] <<- bind_cols(tibble(model, features, setting), metrics(p, y[idx$valid]))
  message(sprintf("  %-22s %-6s %-32s AUC %.4f  PR-AUC %.4f", model, features, setting,
                  auc(p, y[idx$valid]), pr_auc(p, y[idx$valid])))
}

valid_pred <- tibble(row = idx$valid)
test_pred <- tibble(row = idx$test)

# Baselines ---------------------------------------------------------------------
# 1. Everyone gets the training prevalence. 2. Each person gets the severe rate of
# their mode of transport in the training years: the "obvious" rule to beat.
prev <- mean(y[idx$train])
mode_rate <- tibble(mode = df$mode[idx$train], y = y[idx$train]) |>
  group_by(mode) |>
  summarise(rate = mean(y))
by_mode <- function(rows) mode_rate$rate[match(df$mode[rows], mode_rate$mode)]
log_tuning("Baseline: prevalence", "-", "-", rep(prev, length(idx$valid)))
log_tuning("Baseline: mode rate", "-", "-", by_mode(idx$valid))
valid_pred$baseline_mode <- by_mode(idx$valid)
prev_tv <- mean(y[idx$trainval])
mode_rate <- tibble(mode = df$mode[idx$trainval], y = y[idx$trainval]) |>
  group_by(mode) |>
  summarise(rate = mean(y))
test_pred$baseline_prevalence <- prev_tv
test_pred$baseline_mode <- by_mode(idx$test)

# Penalised logistic regression ------------------------------------------------------
# Lambda chosen on the 2023 log loss (not by random cross-validation, which would mix years)
fit_glmnet <- function(features, alpha) {
  m <- glmnet(X[[features]][idx$train, ], yn[idx$train], family = "binomial", alpha = alpha,
              nlambda = 40, lambda.min.ratio = 1e-4)
  p <- predict(m, X[[features]][idx$valid, ], type = "response")
  ll <- apply(p, 2, log_loss, y = y[idx$valid])
  list(alpha = alpha, lambda = m$lambda[which.min(ll)], p = p[, which.min(ll)],
       n_coef = m$df[which.min(ll)])
}
glm_fits <- map(c(0, 0.5, 1), \(a) fit_glmnet("pre", a))
for (g in glm_fits) {
  log_tuning("Penalised logistic", "pre", sprintf("alpha=%.1f lambda=%.2g", g$alpha, g$lambda), g$p)
}
best_glm <- glm_fits[[which.min(map_dbl(glm_fits, \(g) log_loss(g$p, y[idx$valid])))]]
glm_crash <- fit_glmnet("crash", best_glm$alpha)
log_tuning("Penalised logistic", "crash", sprintf("alpha=%.1f lambda=%.2g", glm_crash$alpha, glm_crash$lambda),
           glm_crash$p)
valid_pred$logistic <- best_glm$p
valid_pred$logistic_crash <- glm_crash$p

refit_glmnet <- function(features, g) {
  m <- glmnet(X[[features]][idx$trainval, ], yn[idx$trainval], family = "binomial",
              alpha = g$alpha, lambda = g$lambda)
  as.numeric(predict(m, X[[features]][idx$test, ], type = "response"))
}
test_pred$logistic <- refit_glmnet("pre", best_glm)
test_pred$logistic_crash <- refit_glmnet("crash", glm_crash)

# Random forest -----------------------------------------------------------------
# Probability forest on the factor columns (ranger orders factor levels by outcome,
# which keeps splits on 10+ level factors cheap). Each tree sees 30% of the rows.
rf_data <- df |>
  select(all_of(PRE_IMPACT)) |>
  mutate(across(where(is.logical), factor), severe = factor(y))
rf_grid <- expand_grid(mtry = c(4, 7), min.node.size = c(50, 300))
rf_fit <- function(rows, mtry, min.node.size) {
  ranger(severe ~ ., data = rf_data[rows, ], probability = TRUE, num.trees = 300, mtry = mtry,
         min.node.size = min.node.size, sample.fraction = 0.3, respect.unordered.factors = "order",
         num.threads = THREADS, seed = 2026)
}
rf_valid <- pmap(rf_grid, \(mtry, min.node.size) {
  m <- rf_fit(idx$train, mtry, min.node.size)
  p <- predict(m, rf_data[idx$valid, ], num.threads = THREADS)$predictions[, "TRUE"]
  log_tuning("Random forest", "pre", sprintf("mtry=%d min.node.size=%d", mtry, min.node.size), p)
  p
})
best_rf <- which.min(map_dbl(rf_valid, log_loss, y = y[idx$valid]))
valid_pred$random_forest <- rf_valid[[best_rf]]
rf_final <- rf_fit(idx$trainval, rf_grid$mtry[best_rf], rf_grid$min.node.size[best_rf])
test_pred$random_forest <- predict(rf_final, rf_data[idx$test, ], num.threads = THREADS)$predictions[, "TRUE"]
rm(rf_final)

# XGBoost -----------------------------------------------------------------------
xgb_fit <- function(features, max_depth, train_rows = idx$train, nrounds = 3000, valid = TRUE) {
  dtrain <- xgb.DMatrix(X[[features]][train_rows, ], label = yn[train_rows], nthread = THREADS)
  evals <- if (valid) list(valid = xgb.DMatrix(X[[features]][idx$valid, ], label = yn[idx$valid])) else list()
  xgb.train(params = list(objective = "binary:logistic", eval_metric = "logloss", eta = 0.05,
                          max_depth = max_depth, subsample = 0.8, colsample_bytree = 0.8,
                          min_child_weight = 20, nthread = THREADS),
            data = dtrain, nrounds = nrounds, evals = evals,
            early_stopping_rounds = if (valid) 50 else NULL, verbose = 0)
}
best_round <- function(m) which.min(attributes(m)$evaluation_log$valid_logloss)

xgb_valid <- map(c(4, 6, 8), \(d) {
  m <- xgb_fit("pre", d)
  n <- best_round(m)
  p <- predict(m, X$pre[idx$valid, ], iterationrange = c(1, n + 1))
  log_tuning("XGBoost", "pre", sprintf("max_depth=%d rounds=%d", d, n), p)
  list(depth = d, rounds = n, p = p)
})
best_xgb <- xgb_valid[[which.min(map_dbl(xgb_valid, \(r) log_loss(r$p, y[idx$valid])))]]
valid_pred$xgboost <- best_xgb$p

m <- xgb_fit("crash", best_xgb$depth)
n_crash <- best_round(m)
p <- predict(m, X$crash[idx$valid, ], iterationrange = c(1, n_crash + 1))
log_tuning("XGBoost", "crash", sprintf("max_depth=%d rounds=%d", best_xgb$depth, n_crash), p)
valid_pred$xgboost_crash <- p

# Refit on 2019-2023 with 5% more rounds for the 25% larger training set
xgb_final <- xgb_fit("pre", best_xgb$depth, idx$trainval, round(best_xgb$rounds * 1.05), valid = FALSE)
test_pred$xgboost <- predict(xgb_final, X$pre[idx$test, ])
xgb_final_crash <- xgb_fit("crash", best_xgb$depth, idx$trainval, round(n_crash * 1.05), valid = FALSE)
test_pred$xgboost_crash <- predict(xgb_final_crash, X$crash[idx$test, ])

# Save -------------------------------------------------------------------------------
dir.create("models", showWarnings = FALSE)
invisible(xgb.save(xgb_final, "models/xgboost_pre_impact.ubj"))
shap_rows <- sample(idx$test, 5000)
# Kept sparse: the model treats unstored zeros as missing values
saveRDS(list(X = X$pre[shap_rows, ], row = shap_rows), file.path(DIR_INTERIM, "shap_sample.rds"))

keep <- c("acc_id", "year_num", "severe", "killed", "mode", "age_group", "built_up", "protection")
write_pq(bind_cols(df[valid_pred$row, keep], select(valid_pred, -row)), "predictions_valid")
write_pq(bind_cols(df[test_pred$row, keep], select(test_pred, -row)), "predictions_test")

tuning <- list_rbind(tuning)
save_table(tuning, "ml_tuning")
save_table(tibble(model = c("Penalised logistic", "Random forest", "XGBoost"),
                  chosen = c(sprintf("alpha=%.1f, lambda=%.2g, %d non-zero coefficients", best_glm$alpha,
                                     best_glm$lambda, best_glm$n_coef),
                             sprintf("mtry=%d, min.node.size=%d, 300 trees", rf_grid$mtry[best_rf],
                                     rf_grid$min.node.size[best_rf]),
                             sprintf("max_depth=%d, eta=0.05, %d rounds", best_xgb$depth, best_xgb$rounds))),
           "ml_chosen_settings")
message("Predictions written to data/interim/predictions_{valid,test}.parquet")
