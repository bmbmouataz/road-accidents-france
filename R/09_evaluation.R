# Part 7 - Evaluation and interpretability on the 2024-2025 test years.
#
# Inputs: predictions from R/08 (validation 2023 and test 2024-2025) and the final XGBoost.
# Confidence intervals come from a bootstrap that resamples whole accidents, since people
# in the same accident are not independent.

source("R/00_utils.R")
source("R/00_model_data.R")
suppressPackageStartupMessages({
  library(xgboost)
  library(shapviz)
})
set.seed(2026)
PART <- "07_evaluation"

valid <- read_pq("predictions_valid")
test <- read_pq("predictions_test")

LABELS <- c(baseline_prevalence = "Baseline: prevalence", baseline_mode = "Baseline: rate by mode",
            logistic = "Penalised logistic", random_forest = "Random forest", xgboost = "XGBoost",
            logistic_crash = "Penalised logistic + crash variables",
            xgboost_crash = "XGBoost + crash variables")
MAIN <- c("baseline_mode", "logistic", "random_forest", "xgboost")
MODEL_COL <- setNames(unname(COL[c("yellow", "aqua", "orange", "blue")]), LABELS[MAIN])
models <- intersect(names(LABELS), names(test))
y <- test$severe

# Metrics with accident-level bootstrap intervals --------------------------------------
point <- map(models, \(m) tibble(model = m, roc_auc = auc(test[[m]], y),
                                 pr_auc = pr_auc(test[[m]], y),
                                 log_loss = log_loss(test[[m]], y), brier = brier(test[[m]], y))) |>
  list_rbind()

rows_by_acc <- split(seq_len(nrow(test)), test$acc_id)
boot <- map(1:200, \(b) {
  rows <- unlist(rows_by_acc[sample.int(length(rows_by_acc), replace = TRUE)], use.names = FALSE)
  yb <- y[rows]
  tibble(b, model = models,
         roc_auc = map_dbl(models, \(m) auc(test[[m]][rows], yb)),
         pr_auc = map_dbl(models, \(m) pr_auc(test[[m]][rows], yb)))
}) |> list_rbind()

test_metrics <- boot |>
  group_by(model) |>
  summarise(roc_auc_low = quantile(roc_auc, 0.025), roc_auc_high = quantile(roc_auc, 0.975),
            pr_auc_low = quantile(pr_auc, 0.025), pr_auc_high = quantile(pr_auc, 0.975)) |>
  right_join(point, by = "model") |>
  mutate(label = LABELS[model], prevalence = mean(y)) |>
  arrange(match(model, names(LABELS))) |>
  relocate(label, roc_auc, roc_auc_low, roc_auc_high, pr_auc, pr_auc_low, pr_auc_high)
save_table(test_metrics, "ml_test_metrics")

# Is XGBoost really better than the logistic regression? Paired bootstrap difference
wide <- boot |>
  select(b, model, roc_auc, pr_auc) |>
  pivot_wider(names_from = model, values_from = c(roc_auc, pr_auc))
paired_ci <- function(label, d) {
  q <- quantile(d, c(0.025, 0.5, 0.975))
  tibble(comparison = label, low = q[[1]], median = q[[2]], high = q[[3]])
}
paired <- bind_rows(
  paired_ci("XGBoost - logistic, ROC-AUC", wide$roc_auc_xgboost - wide$roc_auc_logistic),
  paired_ci("XGBoost - logistic, PR-AUC", wide$pr_auc_xgboost - wide$pr_auc_logistic),
  paired_ci("XGBoost - random forest, ROC-AUC", wide$roc_auc_xgboost - wide$roc_auc_random_forest),
  paired_ci("XGBoost + crash - XGBoost, ROC-AUC", wide$roc_auc_xgboost_crash - wide$roc_auc_xgboost)
)
save_table(paired, "ml_paired_differences")

# ROC and precision-recall curves -----------------------------------------------------
curve_points <- function(score, y, n = 300) {
  o <- order(score, decreasing = TRUE)
  ys <- y[o]
  keep <- unique(round(seq(1, length(ys), length.out = n)))
  tp <- cumsum(ys)[keep]
  fp <- cumsum(!ys)[keep]
  tibble(tpr = tp / sum(ys), fpr = fp / sum(!ys), precision = tp / keep)
}
curves <- map(MAIN, \(m) curve_points(test[[m]], y) |> mutate(model = LABELS[[m]])) |> list_rbind() |>
  mutate(model = factor(model, LABELS[MAIN]))
auc_lab <- test_metrics |> filter(model %in% MAIN) |>
  mutate(txt = sprintf("%s: ROC-AUC %.3f, PR-AUC %.3f", label, roc_auc, pr_auc))

p1 <- ggplot(curves, aes(fpr, tpr, colour = model)) +
  geom_abline(linetype = "dotted", colour = INK_2) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = MODEL_COL, labels = auc_lab$txt) +
  coord_equal() +
  labs(title = "ROC curve", x = "False positive rate", y = "True positive rate (recall)", colour = NULL) +
  theme(legend.position = "none")
p2 <- ggplot(curves, aes(tpr, precision, colour = model)) +
  geom_hline(yintercept = mean(y), linetype = "dotted", colour = INK_2) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = MODEL_COL, labels = auc_lab$txt) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Precision-recall curve", x = "Recall", y = "Precision", colour = NULL) +
  theme(legend.position = "right")
p <- patchwork::wrap_plots(p1, p2, widths = c(1, 1.6)) +
  patchwork::plot_annotation(
    title = "XGBoost ranks people best, 2 points of ROC-AUC above the penalised logistic regression",
    subtitle = sprintf("Test years 2024-2025 (%s people, %.1f%% killed or hospitalised). Dotted: random model.",
                       format(nrow(test), big.mark = ","), 100 * mean(y)))
save_fig(p, "01_roc_pr_curves", width = 13, height = 5.2, part = PART)

# Calibration ------------------------------------------------------------------------
calib <- map(MAIN, \(m) {
  tibble(p = test[[m]], y = y) |>
    mutate(bin = ntile(p, 20)) |>
    group_by(bin) |>
    summarise(predicted = mean(p), observed = mean(y), n = n()) |>
    mutate(model = LABELS[[m]])
}) |> list_rbind() |>
  mutate(model = factor(model, LABELS[MAIN]))
ece <- calib |> group_by(model) |> summarise(ece = weighted.mean(abs(predicted - observed), n))
save_table(left_join(calib, ece, by = "model"), "ml_calibration")
p <- ggplot(calib, aes(predicted, observed, colour = model)) +
  geom_abline(linetype = "dotted", colour = INK_2) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.8) +
  scale_colour_manual(values = MODEL_COL) +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_continuous(labels = scales::label_percent()) +
  coord_equal() +
  labs(title = "All models are well calibrated (mean gap below one point)",
       subtitle = "Test years 2024-2025, people grouped in 20 bins of predicted probability. Dotted: perfect calibration.",
       x = "Predicted probability", y = "Observed share killed or hospitalised", colour = NULL)
save_fig(p, "02_calibration", width = 8.5, height = 7, part = PART)

# Thresholds and confusion matrices ---------------------------------------------------
# The decision threshold of each model maximises F1 on the 2023 validation year,
# then it is applied unchanged to 2024-2025.
best_threshold <- function(score, y) {
  o <- order(score, decreasing = TRUE)
  ys <- y[o]
  tp <- cumsum(ys)
  k <- seq_along(ys)
  f1 <- 2 * tp / (k + sum(ys))
  score[o][which.max(f1)]
}
classif <- map(intersect(c(MAIN, "logistic_crash", "xgboost_crash"), names(valid)), \(m) {
  t <- best_threshold(valid[[m]], valid$severe)
  pred <- test[[m]] >= t
  tp <- sum(pred & y); fp <- sum(pred & !y); fn <- sum(!pred & y); tn <- sum(!pred & !y)
  tibble(model = m, label = LABELS[[m]], threshold = t, tp, fp, fn, tn,
         precision = tp / (tp + fp), recall = tp / (tp + fn), f1 = 2 * tp / (2 * tp + fp + fn),
         specificity = tn / (tn + fp), flagged = mean(pred))
}) |> list_rbind()
save_table(classif, "ml_confusion")

cm <- classif |>
  filter(model %in% c("logistic", "xgboost")) |>
  select(label, tp, fp, fn, tn) |>
  pivot_longer(-label) |>
  mutate(truth = if_else(name %in% c("tp", "fn"), "Killed or hospitalised", "Not severe"),
         predicted = if_else(name %in% c("tp", "fp"), "Predicted severe", "Predicted not severe"),
         correct = name %in% c("tp", "tn")) |>
  group_by(label, truth) |>
  mutate(share = value / sum(value)) |>
  ungroup()
p <- ggplot(cm, aes(predicted, truth, fill = correct)) +
  geom_tile(colour = SURFACE, linewidth = 1.5, alpha = 0.85) +
  geom_text(aes(label = sprintf("%s\n%.0f%% of row", scales::comma(value), 100 * share)), size = 3.6,
            colour = INK) +
  scale_fill_manual(values = c(`TRUE` = "#9ec5f4", `FALSE` = "#f6c3ad"), guide = "none") +
  facet_wrap(~label) +
  labs(title = "Confusion matrices on 2024-2025, threshold chosen on 2023 to maximise F1",
       x = NULL, y = NULL) +
  theme(panel.grid = element_blank())
save_fig(p, "03_confusion_matrices", width = 11, height = 4.2, part = PART)

# Performance by group and by year -------------------------------------------------------
groups <- bind_rows(
  test |> mutate(group = as.character(mode), dimension = "Mode"),
  test |> mutate(group = as.character(year_num), dimension = "Year"),
  test |> mutate(group = as.character(built_up), dimension = "Location")
) |>
  filter(!is.na(group)) |>
  group_by(dimension, group) |>
  filter(n() >= 2000, sum(severe) >= 100) |>
  summarise(n = n(), observed = mean(severe),
            xgb_auc = auc(xgboost, severe), logit_auc = auc(logistic, severe),
            xgb_mean_pred = mean(xgboost), logit_mean_pred = mean(logistic), .groups = "drop")
save_table(groups, "ml_by_group")
p <- groups |>
  filter(dimension == "Mode") |>
  mutate(group = fct_reorder(group, xgb_auc)) |>
  pivot_longer(c(xgb_auc, logit_auc), names_to = "model", values_to = "auc") |>
  mutate(model = recode(model, xgb_auc = "XGBoost", logit_auc = "Penalised logistic")) |>
  ggplot(aes(auc, group, colour = model)) +
  geom_point(size = 3) +
  scale_colour_manual(values = unname(COL[c("aqua", "blue")])) +
  labs(title = "Within a mode, severity is much harder to predict for pedestrians and two-wheelers than for car occupants",
       subtitle = "ROC-AUC within each mode of transport, test years 2024-2025",
       x = "ROC-AUC", y = NULL, colour = NULL)
save_fig(p, "04_auc_by_mode", width = 9, height = 4.8, part = PART)

# Error analysis: who does the model miss? --------------------------------------------
t_xgb <- classif$threshold[classif$model == "xgboost"]
errors <- test |>
  filter(severe) |>
  mutate(missed = xgboost < t_xgb) |>
  group_by(mode) |>
  summarise(severe_cases = n(), missed = mean(missed), median_score = median(xgboost)) |>
  arrange(desc(severe_cases))
save_table(errors, "ml_missed_by_mode")

# Interpretability: SHAP values for XGBoost ----------------------------------------------
sh <- readRDS(file.path(DIR_INTERIM, "shap_sample.rds"))
xgb <- xgb.load("models/xgboost_pre_impact.ubj")
# The model was trained on a sparse matrix, where unstored zeros count as missing values.
# SHAP values must be computed on the same sparse representation: on a dense matrix a 0
# becomes a real value and the contributions are wrong.
X_sparse <- if (inherits(sh$X, "dgCMatrix")) sh$X else Matrix::Matrix(sh$X, sparse = TRUE)
X_dense <- as.matrix(X_sparse)
S <- predict(xgb, xgb.DMatrix(X_sparse), predcontrib = TRUE)
margin <- predict(xgb, xgb.DMatrix(X_sparse), outputmargin = TRUE)
stopifnot(max(abs(rowSums(S) - margin)) < 1e-3)
contrib <- as_tibble(S[, colnames(X_dense)])

# One-hot columns are summed back to their original variable: the SHAP value of a
# variable is the sum of the contributions of its dummies.
var_of <- function(col) {
  hit <- PRE_IMPACT[startsWith(col, PRE_IMPACT)]
  hit[which.max(nchar(hit))]
}
by_var <- contrib |>
  mutate(id = row_number()) |>
  pivot_longer(-id, names_to = "column", values_to = "shap") |>
  mutate(variable = map_chr(column, var_of)) |>
  group_by(id, variable) |>
  summarise(shap = sum(shap), .groups = "drop")
importance <- by_var |>
  group_by(variable) |>
  summarise(mean_abs_shap = mean(abs(shap))) |>
  arrange(desc(mean_abs_shap))
save_table(importance, "ml_shap_importance")

# Level of a factor for each person, read back from the one-hot columns
level_of <- function(prefix, reference) {
  cols <- grep(paste0("^", prefix), colnames(X_dense), value = TRUE)
  hit <- X_dense[, cols, drop = FALSE]
  lv <- sub(prefix, "", cols)
  if_else(rowSums(hit) == 0, reference, lv[max.col(hit, ties.method = "first")])
}
by_level <- bind_rows(
  tibble(id = seq_len(nrow(X_dense)), variable = "mode", level = level_of("mode", "Car")),
  tibble(id = seq_len(nrow(X_dense)), variable = "age_group", level = level_of("age_group", "35-44"))
) |>
  left_join(by_var, by = c("id", "variable"))
save_table(by_level |> group_by(variable, level) |>
             summarise(n = n(), mean_shap = mean(shap), .groups = "drop"), "ml_shap_by_level")

VAR_LABELS <- c(mode = "Mode of transport", passenger = "Passenger", sex = "Sex", age_group = "Age",
                protection = "Belt or helmet", opponent = "Heaviest other party", road = "Road category",
                built_up = "Built-up area", speed = "Speed limit", intersection = "Intersection",
                lighting = "Lighting", weather = "Weather", surface = "Road surface",
                period = "Time of day", weekend = "Weekend", area = "Overseas")
p1 <- importance |>
  mutate(variable = fct_reorder(VAR_LABELS[variable], mean_abs_shap)) |>
  ggplot(aes(mean_abs_shap, variable)) +
  geom_col(fill = COL["blue"], width = 0.7) +
  labs(title = "Mean |SHAP| by variable", x = "Mean absolute contribution (log-odds)", y = NULL)
AGES <- c("0-13", "14-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
p2 <- by_level |>
  filter(variable == "mode") |>
  group_by(level) |> filter(n() >= 30) |> ungroup() |>
  mutate(level = fct_reorder(level, shap)) |>
  ggplot(aes(shap, level)) +
  geom_vline(xintercept = 0, colour = INK_2) +
  geom_boxplot(fill = "#9ec5f4", colour = COL["blue"], outlier.size = 0.4, width = 0.6) +
  labs(title = "Contribution of the mode, by mode", x = "SHAP value (log-odds)", y = NULL)
p3 <- by_level |>
  filter(variable == "age_group") |>
  mutate(level = factor(level, AGES)) |>
  ggplot(aes(shap, level)) +
  geom_vline(xintercept = 0, colour = INK_2) +
  geom_boxplot(fill = "#9ec5f4", colour = COL["blue"], outlier.size = 0.4, width = 0.6) +
  labs(title = "Contribution of age, by age group", x = "SHAP value (log-odds)", y = NULL)
p <- patchwork::wrap_plots(p1, p2, p3, nrow = 1, widths = c(1, 1, 0.9)) +
  patchwork::plot_annotation(title = "The mode of transport dominates the predictions, then the other party, the road and the location",
                             subtitle = "SHAP values of the final XGBoost on 5,000 people from the test years. Positive values push towards a severe outcome.")
save_fig(p, "05_shap", width = 15, height = 6, part = PART)

message(paste(capture.output(print(select(test_metrics, label, roc_auc, roc_auc_low, roc_auc_high, pr_auc,
                                          log_loss, brier), width = 200)), collapse = "\n"))
message(paste(capture.output(print(paired)), collapse = "\n"))
message(paste(capture.output(print(select(classif, label, threshold, precision, recall, f1, flagged))),
              collapse = "\n"))
message(paste(capture.output(print(ece)), collapse = "\n"))
message(paste(capture.output(print(head(importance, 8))), collapse = "\n"))
message(paste(capture.output(print(errors)), collapse = "\n"))
message(paste(capture.output(print(filter(groups, dimension != "Mode"))), collapse = "\n"))
