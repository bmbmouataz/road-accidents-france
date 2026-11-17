# Part 3 - Exploratory analysis. Figures in figures/03_eda, tables in results/eda_*.csv
#
# Counts are shown over 2005-2025. Severity rates are computed on 2019-2025 only:
# "hospitalised" is not comparable before 2018 and vehicle codes changed in 2019.

source("R/00_utils.R")

acc <- read_pq("accidents", DIR_FINAL)
usr <- read_pq("users", DIR_FINAL)
recent <- filter(usr, year >= 2019, !is.na(severity))
PART <- "03_eda"
pct <- scales::label_percent(accuracy = 1)
comma <- scales::label_comma()

severe_rate <- function(df, ...) {
  df |>
    group_by(...) |>
    summarise(users = n(), severe = mean(severe), killed = mean(killed), .groups = "drop")
}

# 1. Yearly trend ----------------------------------------------------------------
yearly <- usr |>
  group_by(year) |>
  summarise(Killed = sum(killed, na.rm = TRUE),
            Hospitalised = sum(severity == "Hospitalised", na.rm = TRUE)) |>
  left_join(count(acc, year, name = "Accidents"), by = "year")
save_table(yearly, "eda_yearly")

p <- yearly |>
  pivot_longer(-year) |>
  mutate(name = factor(name, c("Accidents", "Killed", "Hospitalised"))) |>
  ggplot(aes(year, value)) +
  geom_vline(data = tibble(name = factor("Hospitalised", c("Accidents", "Killed", "Hospitalised"))),
             aes(xintercept = 2017.5), linetype = "dashed", colour = COL["orange"]) +
  geom_vline(xintercept = COVID_YEAR, linetype = "dotted", colour = INK_2) +
  geom_line(colour = COL["blue"], linewidth = 1) +
  geom_point(colour = COL["blue"], size = 1.8) +
  facet_wrap(~name, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  labs(title = "Injury accidents and casualties in France, 2005-2025",
       subtitle = "Dotted line: 2020 lockdowns. Dashed: 'hospitalised' recording changed in 2018 and is not comparable with earlier years",
       x = NULL, y = NULL, caption = "Source: BAAC (ONISR), mainland France and overseas departments.")
save_fig(p, "01_yearly_trend", width = 11, height = 4.2, part = PART)

# 2. Month -------------------------------------------------------------------------
monthly <- acc |>
  filter(year >= 2019, year != COVID_YEAR) |>
  group_by(month) |>
  summarise(accidents = n() / n_distinct(year), killed = sum(n_killed) / n_distinct(year),
            killed_per_100 = 100 * sum(n_killed) / n())
save_table(monthly, "eda_monthly")

p <- monthly |>
  mutate(month = factor(month.abb[month], month.abb)) |>
  pivot_longer(c(accidents, killed_per_100)) |>
  mutate(name = recode(name, accidents = "Accidents per month (average)",
                       killed_per_100 = "Killed per 100 accidents")) |>
  ggplot(aes(month, value)) +
  geom_col(fill = COL["blue"], width = 0.7) +
  facet_wrap(~name, scales = "free_y") +
  labs(title = "August has fewer accidents than June or October, but they are the deadliest of the year",
       subtitle = "2019-2025 excluding 2020", x = NULL, y = NULL)
save_fig(p, "02_month", width = 10, height = 4.2, part = PART)

# 3. Hour x weekday ------------------------------------------------------------------
hourly <- acc |>
  filter(year >= 2019, !is.na(hour)) |>
  count(weekday, hour)
p <- ggplot(hourly, aes(hour, fct_rev(weekday), fill = n / n_distinct(acc$year[acc$year >= 2019]))) +
  geom_tile(colour = SURFACE, linewidth = 0.5) +
  scale_fill_gradientn(colours = BLUES, labels = comma) +
  scale_x_continuous(breaks = seq(0, 23, 2), expand = c(0, 0)) +
  labs(title = "Accidents follow commuting hours on weekdays and shift to nights at weekends",
       subtitle = "Average number of injury accidents per year, 2019-2025", x = "Hour of day",
       y = NULL, fill = "Accidents per year") +
  theme(panel.grid = element_blank(), legend.key.width = unit(1.4, "cm"))
save_fig(p, "03_hour_weekday", width = 10, height = 4.2, part = PART)

by_hour <- acc |>
  filter(year >= 2019, !is.na(hour)) |>
  group_by(hour, weekend = if_else(weekend, "Weekend", "Weekday")) |>
  summarise(killed_per_100 = 100 * sum(n_killed) / n(), accidents = n(), .groups = "drop")
save_table(by_hour, "eda_hour")
p <- ggplot(by_hour, aes(hour, killed_per_100, colour = weekend)) +
  geom_line(linewidth = 1) + geom_point(size = 1.6) +
  scale_colour_manual(values = unname(COL[c("blue", "orange")])) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(title = "Between 3 and 5 a.m., an accident kills 2 to 3 times as often as at 5 p.m.",
       subtitle = "Killed per 100 injury accidents, 2019-2025", x = "Hour of day", y = NULL,
       colour = NULL)
save_fig(p, "04_fatality_by_hour", width = 9, height = 4.5, part = PART)

# 4. Lighting and weather ---------------------------------------------------------------
cond <- bind_rows(
  severe_rate(filter(recent, !is.na(lighting)), level = lighting) |> mutate(factor = "Lighting"),
  severe_rate(filter(recent, !is.na(weather)), level = weather) |> mutate(factor = "Weather"),
  severe_rate(filter(recent, !is.na(surface)), level = surface) |> mutate(factor = "Road surface")
)
save_table(cond, "eda_conditions")
p <- cond |>
  filter(users > 2000) |>
  mutate(level = reorder(paste(factor, level, sep = "__"), severe)) |>
  ggplot(aes(severe, level)) +
  geom_col(fill = COL["blue"], width = 0.7) +
  geom_text(aes(label = pct(severe)), hjust = -0.15, size = 3.2, colour = INK_2) +
  facet_wrap(~factor, scales = "free_y", ncol = 1) +
  scale_y_discrete(labels = \(x) str_remove(x, ".*__")) +
  scale_x_continuous(labels = pct, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "At night on unlit roads, the share of people killed is 2.7 times the daylight level",
       subtitle = "Share of people involved who were killed or hospitalised, 2019-2025",
       x = NULL, y = NULL,
       caption = "Light rain lowers severity (slower driving, more accidents in towns); fog and strong wind raise it.")
save_fig(p, "05_conditions", width = 9, height = 8.5, part = PART)

# 5. Road category and built-up area ----------------------------------------------------
roads <- acc |>
  filter(year >= 2019, !is.na(road), !is.na(built_up), road != "Other") |>
  group_by(road, built_up) |>
  summarise(accidents = n(), killed_per_100 = 100 * sum(n_killed) / n(), .groups = "drop")
save_table(roads, "eda_roads")
p <- roads |>
  filter(accidents > 500) |>
  ggplot(aes(killed_per_100, reorder(road, killed_per_100), fill = built_up)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = unname(COL[c("orange", "blue")])) +
  labs(title = "Outside built-up areas, an accident kills 3 to 4 times as often; departmental roads are the deadliest",
       subtitle = "Killed per 100 injury accidents by road category, 2019-2025",
       x = NULL, y = NULL, fill = NULL)
save_fig(p, "06_roads", width = 9, height = 4.8, part = PART)

# 6. Mode of transport --------------------------------------------------------------
modes <- recent |>
  filter(mode != "Other", !is.na(mode)) |>
  count(mode, severity) |>
  group_by(mode) |>
  mutate(share = n / sum(n), total = sum(n)) |>
  ungroup()
save_table(modes, "eda_modes")
order_modes <- modes |>
  filter(severity %in% c("Killed", "Hospitalised")) |>
  group_by(mode) |>
  summarise(s = sum(share)) |>
  arrange(s) |>
  pull(mode)
p <- modes |>
  filter(total > 5000) |>
  mutate(mode = factor(mode, order_modes)) |>
  ggplot(aes(share, mode, fill = fct_rev(severity))) +
  geom_col(width = 0.72, colour = SURFACE, linewidth = 0.4) +
  scale_fill_manual(values = SEVERITY_COL, breaks = names(SEVERITY_COL)) +
  scale_x_continuous(labels = pct, expand = c(0, 0)) +
  labs(title = "About one motorcyclist, pedestrian or cyclist in three is killed or hospitalised, against one car occupant in eight",
       subtitle = "Severity of injury by mode of transport of the person, 2019-2025",
       x = NULL, y = NULL, fill = NULL)
save_fig(p, "07_mode_severity", width = 10, height = 5, part = PART)

killed_mode_year <- usr |>
  filter(killed, mode != "Other") |>
  mutate(mode = fct_lump_n(fct_drop(mode), 6, other_level = "Other")) |>
  count(year, mode)
save_table(killed_mode_year, "eda_killed_mode_year")
p <- ggplot(killed_mode_year, aes(year, n)) +
  geom_vline(xintercept = COVID_YEAR, linetype = "dotted", colour = INK_2) +
  geom_line(colour = COL["blue"], linewidth = 1) +
  facet_wrap(~mode, scales = "free_y", ncol = 4) +
  scale_y_continuous(limits = c(0, NA), labels = comma) +
  labs(title = "Car deaths halved since 2005, motorcyclist deaths fell by a third, cyclist deaths rose by 30%",
       subtitle = "People killed per year by mode of transport",
       x = NULL, y = NULL)
save_fig(p, "08_killed_by_mode_year", width = 11, height = 5, part = PART)

# 7. Age and sex --------------------------------------------------------------------
age_sex <- recent |>
  filter(!is.na(age_group), !is.na(sex)) |>
  group_by(age_group, sex) |>
  summarise(involved = n(), killed = sum(killed), severe_rate = mean(severe), .groups = "drop")
save_table(age_sex, "eda_age_sex")
p1 <- age_sex |>
  mutate(killed = killed / n_distinct(recent$year) * if_else(sex == "Male", 1, -1)) |>
  ggplot(aes(killed, age_group, fill = sex)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = unname(COL[c("orange", "blue")])) +
  scale_x_continuous(labels = \(x) comma(abs(x))) +
  labs(title = "People killed per year", x = NULL, y = NULL, fill = NULL)
p2 <- ggplot(age_sex, aes(severe_rate, age_group, colour = sex)) +
  geom_point(size = 3) +
  scale_colour_manual(values = unname(COL[c("orange", "blue")]), guide = "none") +
  scale_x_continuous(labels = pct, limits = c(0, NA)) +
  labs(title = "Share killed or hospitalised", x = NULL, y = NULL)
p <- patchwork::wrap_plots(p1, p2) +
  patchwork::plot_annotation(
    title = "Nearly four road deaths in five are men; people over 75 are the most fragile",
    subtitle = "2019-2025 average")
save_fig(p, "09_age_sex", width = 11, height = 5, part = PART)

# 8. Who hits whom --------------------------------------------------------------------
opp <- recent |>
  filter(mode %in% c("Pedestrian", "Bicycle", "E-scooter / PMD", "Moped", "Motorcycle", "Car"),
         !is.na(opponent), opponent != "Other") |>
  severe_rate(mode, opponent) |>
  filter(users >= 300)
save_table(opp, "eda_opponent")
p <- opp |>
  mutate(opponent = factor(opponent, c("No other vehicle", "Bicycle or PMD", "Moped",
                                       "Motorcycle", "Car", "Van", "Bus or coach",
                                       "Heavy goods vehicle", "Tram or train"))) |>
  ggplot(aes(opponent, fct_rev(mode), fill = severe)) +
  geom_tile(colour = SURFACE, linewidth = 0.8) +
  geom_text(aes(label = pct(severe), colour = severe > 0.35), size = 3.3) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = INK), guide = "none") +
  scale_fill_gradientn(colours = BLUES, labels = pct) +
  labs(title = "Hit by a heavy goods vehicle, 64% of pedestrians and 55% of cyclists are killed or hospitalised",
       subtitle = "Share killed or hospitalised, by the person's mode (rows) and the heaviest other party (columns), 2019-2025",
       x = "Heaviest other vehicle involved", y = NULL, fill = NULL) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1))
save_fig(p, "10_opponent", width = 11, height = 5, part = PART)

message("EDA figures written to ", file.path(DIR_FIG, PART))
