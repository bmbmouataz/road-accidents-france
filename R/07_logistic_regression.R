# Part 5 - Logistic regression for the probability of being killed or hospitalised.
#
# Population: people involved in an injury accident, 2019-2025, with known severity.
# Standard errors are clustered by accident: people in the same crash share the same
# road, speed and impact, so treating them as independent overstates the precision.
# Only variables known before the impact are used (no collision type, obstacle or impact
# point): the model describes who is at risk in which situation, not how the crash unfolded.

source("R/00_utils.R")
source("R/00_model_data.R")
suppressPackageStartupMessages(library(fixest))

PART <- "05_model"
set.seed(2026)

df <- model_data()
message(sprintf("Model data: %s people, %s accidents, %.1f%% killed or hospitalised",
                format(nrow(df), big.mark = ","), format(n_distinct(df$acc_id), big.mark = ","),
                100 * mean(df$severe)))

f_main <- severe ~ mode + passenger + sex + age_group + protection + opponent + road + built_up +
  speed + intersection + lighting + weather + surface + period + weekend + area | year
m_main <- feglm(f_main, data = df, family = binomial, cluster = ~acc_id)
m_iid <- summary(m_main, vcov = "iid")

fit <- tibble(
  model = "Main effects",
  n = nobs(m_main), n_coef = length(coef(m_main)),
  mcfadden_r2 = r2(m_main, "pr2"),
  auc_in_sample = auc(predict(m_main), df$severe),
  se_ratio_cluster_vs_iid = median(se(m_main) / se(m_iid))
)

# Odds ratios with cluster-robust 95% intervals ---------------------------------------
VARS <- c(mode = "Mode of transport", passenger = "Passenger (vs driver)", sex = "Sex",
          age_group = "Age", protection = "Belt or helmet", opponent = "Heaviest other party",
          road = "Road category", built_up = "Location", speed = "Speed limit (km/h)",
          intersection = "Intersection", lighting = "Lighting", weather = "Weather",
          surface = "Road surface", period = "Time of day", weekend = "Weekend", area = "Area")
REFS <- c(mode = "Car", sex = "Female", age_group = "35-44", protection = "Worn",
          opponent = "Car", road = "Municipal road", built_up = "Built-up area",
          speed = "40-50", intersection = "No intersection", lighting = "Daylight",
          weather = "Normal", surface = "Normal", period = "Midday (10h-16h)", area = "Mainland")

ci <- confint(m_main)
odds <- tibble(term = names(coef(m_main)), estimate = coef(m_main), low = ci[, 1], high = ci[, 2],
               p_value = pvalue(m_main)) |>
  mutate(variable = map_chr(term, \(t) names(VARS)[which(startsWith(t, names(VARS)))[1]]),
         level = str_remove(term, paste0("^", variable)),
         level = if_else(level %in% c("TRUE", ""), "Yes", level),
         across(c(estimate, low, high), exp, .names = "or_{.col}"),
         label = VARS[variable])
save_table(select(odds, variable, level, odds_ratio = or_estimate, ci_low = or_low,
                  ci_high = or_high, p_value), "logit_odds_ratios")

ref_rows <- tibble(variable = names(REFS), level = paste0(REFS, " (ref.)"), or_estimate = 1,
                   or_low = 1, or_high = 1, label = VARS[names(REFS)])
plot_df <- bind_rows(odds, ref_rows) |>
  filter(!variable %in% c("area", "weekend")) |>
  mutate(label = factor(label, unique(VARS)),
         signif = or_low > 1 | or_high < 1) |>
  group_by(variable) |>
  mutate(level = fct_reorder(level, or_estimate)) |>
  ungroup()

p <- ggplot(plot_df, aes(or_estimate, level)) +
  geom_vline(xintercept = 1, colour = INK_2, linewidth = 0.4) +
  geom_linerange(aes(xmin = or_low, xmax = or_high), colour = COL["blue"], linewidth = 0.8) +
  geom_point(aes(colour = or_estimate > 1), size = 2) +
  scale_colour_manual(values = c(`TRUE` = COL[["orange"]], `FALSE` = COL[["blue"]]), guide = "none") +
  scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 8, 16), labels = \(x) format(x, drop0trailing = TRUE)) +
  facet_grid(label ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(title = "Odds ratios of being killed or hospitalised, all factors adjusted",
       subtitle = sprintf("Logistic regression on %s people involved in injury accidents, 2019-2025, with year fixed effects. Bars: 95%% intervals clustered by accident.",
                          format(nobs(m_main), big.mark = ",")),
       x = "Odds ratio (log scale; 1 = same odds as the reference level)", y = NULL) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
        panel.spacing.y = unit(0.25, "lines"), axis.text.y = element_text(size = 8.5))
save_fig(p, "01_odds_ratios", width = 11, height = 15, part = PART)

# Interactions ---------------------------------------------------------------------------
# Does being outside a built-up area hurt every mode equally? Are older pedestrians
# more vulnerable than older car occupants beyond the main effects of age and mode?
df_int <- mutate(df, pedestrian = mode == "Pedestrian")
f_int <- severe ~ mode + passenger + sex + age_group + protection + opponent + road + built_up +
  speed + intersection + lighting + weather + surface + period + weekend + area +
  mode:built_up + age_group:pedestrian | year
m_int <- feglm(f_int, data = df_int, family = binomial, cluster = ~acc_id)
w1 <- wald(m_int, keep = "^mode.*:built_up", print = FALSE)
w2 <- wald(m_int, keep = "^age_group.*:pedestrian", print = FALSE)
interactions <- tibble(interaction = c("Mode x built-up area", "Age x pedestrian"),
                       wald_stat = c(w1$stat, w2$stat), df = c(w1$df1, w2$df1),
                       p_value = c(w1$p, w2$p))
save_table(interactions, "logit_interactions")
fit <- bind_rows(fit, tibble(model = "With interactions", n = nobs(m_int),
                             n_coef = length(coef(m_int)), mcfadden_r2 = r2(m_int, "pr2"),
                             auc_in_sample = auc(predict(m_int), df_int$severe),
                             se_ratio_cluster_vs_iid = NA_real_))
save_table(fit, "logit_fit")

# Average predicted probabilities: every person of a 100,000-person sample is given a
# level in turn, the rest of their characteristics unchanged, and predictions are averaged.
# Pedestrians have no belt or helmet and are always hit by a vehicle, so these two
# variables are set consistently with the mode being imposed.
sample_df <- slice_sample(df_int, n = 100000)
AGES <- c("0-13", "14-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")

impose <- function(data, new_mode, ...) {
  ped <- new_mode == "Pedestrian"
  data |>
    mutate(mode = factor(new_mode, levels(df$mode)), pedestrian = ped,
           protection = factor(if (ped) "Not applicable" else "Worn", levels(df$protection)),
           opponent = if (ped) {
             factor(if_else(opponent == "No other vehicle", "Car", as.character(opponent)),
                    levels(df$opponent))
           } else opponent,
           ...)
}
avg_prob <- function(data) mean(predict(m_int, newdata = data, type = "response"), na.rm = TRUE)

p_mode <- expand_grid(mode = levels(df$mode), built_up = levels(df$built_up)) |>
  mutate(prob = map2_dbl(mode, built_up, \(m, b) {
    avg_prob(impose(sample_df, m, built_up = factor(b, levels(df$built_up))))
  }))
save_table(p_mode, "logit_predicted_mode_builtup")

p <- p_mode |>
  mutate(mode = fct_reorder(mode, prob, .fun = max)) |>
  ggplot(aes(prob, mode)) +
  geom_line(aes(group = mode), colour = GRID, linewidth = 2.2) +
  geom_point(aes(colour = built_up), size = 3.4) +
  scale_colour_manual(values = unname(COL[c("blue", "orange")])) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0, NA)) +
  labs(title = "Outside built-up areas, 50% of pedestrians, 37% of cyclists and 15% of car occupants are severely hurt",
       subtitle = "Average predicted probability of being killed or hospitalised (belt or helmet worn), other characteristics as observed",
       x = NULL, y = NULL, colour = NULL)
save_fig(p, "02_mode_by_location", width = 11, height = 5, part = PART)

p_age <- expand_grid(age_group = AGES, who = c("Car occupant (belted)", "Pedestrian")) |>
  mutate(prob = map2_dbl(age_group, who, \(a, w) {
    mode <- if (w == "Pedestrian") "Pedestrian" else "Car"
    avg_prob(impose(sample_df, mode, age_group = factor(a, levels(df$age_group))))
  }), age_group = factor(age_group, AGES))
save_table(p_age, "logit_predicted_age")

p <- ggplot(p_age, aes(age_group, prob, colour = who, group = who)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.6) +
  scale_colour_manual(values = unname(COL[c("blue", "orange")])) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, NA)) +
  labs(title = "From age 35-44 to 75+, the risk rises by 24 points for pedestrians and 12 points for belted car occupants",
       subtitle = "Average predicted probability of being killed or hospitalised, other characteristics as observed",
       x = "Age group", y = NULL, colour = NULL)
save_fig(p, "03_age_pedestrian", width = 9, height = 4.8, part = PART)

message(paste(capture.output(print(fit)), collapse = "\n"))
message(paste(capture.output(print(interactions)), collapse = "\n"))
