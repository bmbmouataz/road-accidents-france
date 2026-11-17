# Modelling table shared by the logistic regression (R/07) and machine learning (R/08).
# Sourced after R/00_utils.R.
#
# Population: people involved in an injury accident in 2019-2025 with known severity.
# Target: severe = killed or hospitalised.
# PRE_IMPACT variables are known before the crash; CRASH variables describe the crash
# itself (collision type, obstacle hit, impact point, manoeuvre) and are only used to
# measure how much they add.

PRE_IMPACT <- c("mode", "passenger", "sex", "age_group", "protection", "opponent", "road",
                "built_up", "speed", "intersection", "lighting", "weather", "surface", "period",
                "weekend", "area")
CRASH <- c("collision", "obstacle", "impact", "manoeuvre")

speed_class <- function(v) {
  cut(v, c(0, 30, 50, 80, 90, 110, 130),
      labels = c("30 or less", "40-50", "60-80", "90", "110", "130")) |>
    fct_na_value_to_level("Unknown")
}

model_data <- function() {
  read_pq("users", DIR_FINAL) |>
    filter(year >= 2019, !is.na(severity), mode != "Other") |>
    transmute(
      acc_id, severe, killed, year_num = year, year = factor(year),
      mode = fct_relevel(fct_drop(mode), "Car"),
      passenger = role == "Passenger",
      sex = fct_relevel(sex, "Female"),
      age_group = fct_relevel(age_group, "35-44"),
      protection = fct_relevel(fct_drop(protection), "Worn"),
      opponent = fct_relevel(factor(opponent), "Car"),
      road = fct_relevel(fct_lump_min(fct_drop(road), 3000, other_level = "Other"), "Municipal road"),
      built_up = fct_relevel(built_up, "Built-up area"),
      speed = fct_relevel(speed_class(speed_limit), "40-50"),
      intersection = fct_relevel(fct_lump_min(intersection, 5000, other_level = "Other"),
                                 "No intersection"),
      lighting = fct_relevel(lighting, "Daylight"),
      weather = fct_relevel(fct_lump_min(weather, 5000, other_level = "Other"), "Normal"),
      surface = fct_relevel(surface, "Normal"),
      period = fct_relevel(period, "Midday (10h-16h)"),
      weekend, area = factor(area),
      collision = fct_na_value_to_level(collision, "Unknown"),
      obstacle = factor(case_when(fixed_obstacle %in% 1:17 ~ "Fixed obstacle",
                                  mobile_obstacle %in% c(1L, 2L, 4L) ~ "Pedestrian or vehicle",
                                  mobile_obstacle %in% 5:6 ~ "Animal",
                                  .default = "None or unknown")),
      impact = factor(case_when(impact_point %in% 1:3 ~ "Front", impact_point %in% 4:6 ~ "Rear",
                                impact_point %in% 7:8 ~ "Side", impact_point == 9 ~ "Multiple",
                                .default = "None or unknown")),
      # ONISR codes: 1-2 straight on, 3/11/12/17/18 changing lane or overtaking, 13-14
      # drifting off course, 10/15/16 turning, 5-6 wrong way, 9/19 entering or crossing,
      # 4/20/22-24 reversing, parking or stopped, 21 avoiding
      manoeuvre = factor(case_when(manoeuvre %in% 1:2 ~ "Straight on",
                                   manoeuvre %in% c(3, 11, 12, 17, 18) ~ "Changing lane or overtaking",
                                   manoeuvre %in% 13:14 ~ "Drifting off course",
                                   manoeuvre %in% c(10, 15, 16) ~ "Turning",
                                   manoeuvre %in% 5:6 ~ "Wrong way",
                                   manoeuvre %in% c(9, 19) ~ "Entering or crossing",
                                   manoeuvre %in% c(4, 20, 22, 23, 24) ~ "Reversing, parking or stopped",
                                   manoeuvre == 21 ~ "Avoiding",
                                   .default = "Other or unknown"))
    ) |>
    drop_na(all_of(PRE_IMPACT))
}

# Metrics -------------------------------------------------------------------------
# ROC-AUC through the Mann-Whitney statistic: fast on hundreds of thousands of rows
auc <- function(score, y) {
  r <- rank(score)
  n1 <- as.numeric(sum(y))
  n0 <- length(y) - n1
  (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Area under the precision-recall curve (average precision)
pr_auc <- function(score, y) {
  o <- order(score, decreasing = TRUE)
  y <- y[o]
  tp <- cumsum(y)
  precision <- tp / seq_along(y)
  sum(precision[y]) / sum(y)
}

log_loss <- function(p, y) {
  p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

brier <- function(p, y) mean((p - y)^2)
