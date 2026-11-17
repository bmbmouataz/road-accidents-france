# Data quality: missing values, duplicates, orphans and invalid values.
# Writes cleaned tables to data/interim/*_clean.parquet and a log of every check.

source("R/00_utils.R")

characteristics <- read_pq("characteristics")
places <- read_pq("places")
vehicles <- read_pq("vehicles")
users <- read_pq("users")

log <- list()
note <- function(check, n, total, action) {
  log[[length(log) + 1]] <<- tibble(check, n, share = n / total, action)
  message(sprintf("%-60s %9s  (%.2f%%)", check, format(n, big.mark = ","), 100 * n / total))
}

# Scope -----------------------------------------------------------------------
# Overseas collectivities (Polynesia, New Caledonia, Saint-Martin...) only appear
# from 2019: keeping them would create an artificial jump in the series.
MAINLAND_OVERSEAS <- c(sprintf("%02d", c(1:19, 21:95)), "2A", "2B",
                       "971", "972", "973", "974", "976")
out_scope <- characteristics |> filter(!department %in% MAINLAND_OVERSEAS)
note("Accidents outside mainland France + 5 overseas departments", nrow(out_scope),
     nrow(characteristics), "removed (only recorded from 2019)")
characteristics <- characteristics |> filter(department %in% MAINLAND_OVERSEAS)
keep <- characteristics$acc_id
places <- filter(places, acc_id %in% keep)
vehicles <- filter(vehicles, acc_id %in% keep)
users <- filter(users, acc_id %in% keep)

# Missing values ----------------------------------------------------------------
# 0 and empty cells mean "not filled" for categorical codes (ONISR documentation)
zero_is_missing <- list(
  characteristics = c("lighting", "urban", "intersection", "weather", "collision"),
  places = c("road_cat", "traffic_regime", "slope", "layout", "surface", "situation"),
  users = c("user_cat", "severity", "sex", "trip")
)
characteristics <- mutate(characteristics, across(all_of(zero_is_missing$characteristics), \(x) na_if(x, 0L)))
places <- mutate(places, across(all_of(zero_is_missing$places), \(x) na_if(x, 0L)))
users <- mutate(users, across(all_of(zero_is_missing$users), \(x) na_if(x, 0L)))
vehicles <- mutate(vehicles, veh_cat = na_if(veh_cat, 0L))

missing_by_year <- function(df, table) {
  df |>
    select(-any_of(c("acc_id", "veh_id", "veh_uid"))) |>
    group_by(year) |>
    summarise(across(everything(), \(x) mean(is.na(x)))) |>
    pivot_longer(-year, names_to = "variable", values_to = "missing") |>
    mutate(table = table)
}
missing <- bind_rows(missing_by_year(characteristics, "characteristics"),
                     missing_by_year(places, "places"),
                     missing_by_year(vehicles, "vehicles"),
                     missing_by_year(users, "users"))
save_table(missing, "quality_missing_by_year")

p <- missing |>
  mutate(variable = paste(table, variable, sep = " / ")) |>
  filter(!str_detect(variable, "safety_2005|safety_[123]|ped_|seat")) |>
  ggplot(aes(year, fct_rev(variable), fill = missing)) +
  geom_tile(colour = SURFACE, linewidth = 0.4) +
  scale_fill_gradientn(colours = BLUES, labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(2005, 2025, 2), expand = c(0, 0)) +
  labs(title = "Share of missing values by variable and year",
       subtitle = "Coordinates are only reliable from 2019; speed limit and motor type were added in 2019",
       x = NULL, y = NULL, fill = "Missing") +
  theme(panel.grid = element_blank(), legend.key.width = unit(1.5, "cm"))
save_fig(p, "missing_values", width = 10, height = 9, part = "02_data_engineering")

# Duplicates ------------------------------------------------------------------
note("Duplicated accident ids in characteristics", sum(duplicated(characteristics$acc_id)),
     nrow(characteristics), "none")

# From 2023 an accident can have several place rows (one per road at an
# intersection). Keep the main road: lowest road category code, 9 (other) last.
dup_places <- sum(duplicated(places$acc_id))
note("Extra place rows for the same accident (2023 onwards)", dup_places, nrow(places),
     "kept the row of the main road")
places <- places |>
  arrange(acc_id, if_else(road_cat == 9L | is.na(road_cat), 99L, road_cat)) |>
  distinct(acc_id, .keep_all = TRUE)

dup_veh <- vehicles |>
  filter(year < 2019) |>
  count(acc_id, veh_id) |>
  filter(n > 1)
note("Duplicated (accident, vehicle letter) keys before 2019", nrow(dup_veh), nrow(vehicles),
     "none; from 2019 the unique id_vehicule is used instead")
note("Duplicated (accident, vehicle letter) keys from 2019",
     vehicles |> filter(year >= 2019) |> count(acc_id, veh_id) |> filter(n > 1) |> nrow(),
     nrow(vehicles), "solved by joining on id_vehicule")

# Orphans ---------------------------------------------------------------------
orphans <- users |>
  mutate(key = if_else(year >= 2019, veh_uid, veh_id)) |>
  anti_join(vehicles |> mutate(key = if_else(year >= 2019, veh_uid, veh_id)),
            by = c("acc_id", "key"))
note("Users whose vehicle is missing from the vehicle table", nrow(orphans), nrow(users),
     "kept, vehicle variables set to missing")
note("Accidents without any user", nrow(anti_join(characteristics, users, by = "acc_id")),
     nrow(characteristics), "none")
note("Accidents without a place row", nrow(anti_join(characteristics, places, by = "acc_id")),
     nrow(characteristics), "kept, place variables missing")

# Invalid values --------------------------------------------------------------
age <- users$year - users$birth_year
bad_age <- !is.na(age) & (age < 0 | age > 105)
note("Birth year giving an age below 0 or above 105", sum(bad_age), nrow(users),
     "birth year set to missing")
users$birth_year[bad_age] <- NA

note("Users with unknown severity", sum(is.na(users$severity)), nrow(users),
     "excluded from severity analyses (mostly hit-and-run users added from 2021)")

bad_time <- with(characteristics, is.na(hour) | hour > 23 | is.na(minute) | minute > 59)
note("Invalid time of day", sum(bad_time), nrow(characteristics), "set to missing")
characteristics$hour[bad_time] <- NA

bad_date <- with(characteristics, is.na(make_date(year, month, day)))
note("Invalid calendar date", sum(bad_date), nrow(characteristics), "none")

mainland <- !str_starts(characteristics$department, "97")
bad_coord <- with(characteristics, mainland & !is.na(lat) &
                    (lat < 41 | lat > 51.5 | lon < -5.5 | lon > 10))
note("Mainland accidents with coordinates outside mainland France", sum(bad_coord),
     sum(mainland), "coordinates set to missing")
characteristics$lat[bad_coord] <- NA
characteristics$lon[bad_coord] <- NA

bad_speed <- with(places, !is.na(speed_limit) & (speed_limit < 5 | speed_limit > 130))
note("Speed limit outside 5-130 km/h", sum(bad_speed), sum(!is.na(places$speed_limit)),
     "set to missing")
places$speed_limit[bad_speed] <- NA

# Consistency between tables ---------------------------------------------------
killed_mismatch <- users |>
  group_by(acc_id) |>
  summarise(any_killed = any(severity == 2L, na.rm = TRUE)) |>
  filter(is.na(any_killed))
note("Accidents where the worst severity cannot be computed", nrow(killed_mismatch),
     nrow(characteristics), "none")

write_pq(characteristics, "characteristics_clean")
write_pq(places, "places_clean")
write_pq(vehicles, "vehicles_clean")
write_pq(users, "users_clean")

quality_log <- list_rbind(log)
save_table(quality_log, "quality_checks")
