# Builds the two analysis tables used by every later script:
#   data/final/accidents.parquet  one row per accident
#   data/final/users.parquet      one row per person involved, with vehicle and context
#
# Users are linked to their own vehicle (pedestrians to the vehicle that hit them):
# on the unique id_vehicule from 2019, on (accident, vehicle letter) before.
# Joining on the accident id alone, as in the first version of this project,
# multiplies every user by the number of vehicles in the accident.

source("R/00_utils.R")

characteristics <- read_pq("characteristics_clean")
places <- read_pq("places_clean")
vehicles <- read_pq("vehicles_clean")
users <- read_pq("users_clean")

lab <- function(x, labels) factor(unname(labels[as.character(x)]), levels = unique(labels))

SEVERITY <- c(`1` = "Unharmed", `4` = "Slightly injured", `3` = "Hospitalised", `2` = "Killed")
LIGHTING <- c(`1` = "Daylight", `2` = "Dusk or dawn", `5` = "Night, street lights on",
              `4` = "Night, street lights off", `3` = "Night, no street lights")
WEATHER <- c(`1` = "Normal", `8` = "Overcast", `2` = "Light rain", `3` = "Heavy rain",
             `4` = "Snow or hail", `5` = "Fog or smoke", `6` = "Strong wind",
             `7` = "Dazzling", `9` = "Other")
COLLISION <- c(`1` = "Two vehicles, head-on", `2` = "Two vehicles, rear-end",
               `3` = "Two vehicles, side", `4` = "3+ vehicles, chain",
               `5` = "3+ vehicles, multiple", `6` = "Other collision", `7` = "No collision")
ROAD <- c(`1` = "Motorway", `2` = "National road", `3` = "Departmental road",
          `4` = "Municipal road", `7` = "Metropolitan road", `5` = "Off public network",
          `6` = "Car park", `9` = "Other")
INTERSECTION <- c(`1` = "No intersection", `2` = "X intersection", `3` = "T intersection",
                  `4` = "Y intersection", `5` = "5+ branches", `6` = "Roundabout",
                  `7` = "Square", `8` = "Level crossing", `9` = "Other")
SURFACE <- c(`1` = "Normal", `2` = "Wet", `3` = "Wet", `4` = "Wet", `5` = "Snow or ice",
             `7` = "Snow or ice", `6` = "Other", `8` = "Other", `9` = "Other")
USER_CAT <- c(`1` = "Driver", `2` = "Passenger", `3` = "Pedestrian", `4` = "Pedestrian")
TRIP <- c(`1` = "Home-work", `2` = "Home-school", `3` = "Shopping", `4` = "Professional",
          `5` = "Leisure", `9` = "Other")

# Vehicle groups across the 2005-2018 and 2019+ nomenclatures (codes 4-6, 8-9,
# 11-12, 18-19 were retired in 2006; 41-43, 50, 60 and 80 were created in 2019)
veh_group <- function(catv) {
  case_when(
    catv %in% c(1, 80) ~ "Bicycle",
    catv %in% c(50, 60) ~ "E-scooter / PMD",
    catv %in% c(2, 30, 41) ~ "Moped",
    catv %in% c(4, 5, 6, 31, 32, 33, 34, 42, 43) ~ "Motorcycle",
    catv %in% c(3, 7, 8, 9) ~ "Car",
    catv %in% c(10, 11, 12) ~ "Van",
    catv %in% c(13, 14, 15, 16, 17) ~ "Heavy goods vehicle",
    catv %in% c(18, 37, 38) ~ "Bus or coach",
    catv %in% c(19, 39, 40) ~ "Tram or train",
    .default = "Other"
  )
}
VEH_GROUPS <- c("Car", "Van", "Motorcycle", "Moped", "Bicycle", "E-scooter / PMD",
                "Heavy goods vehicle", "Bus or coach", "Tram or train", "Other")
# Rough mass ranking, used to find the heaviest other party in the accident
MASS_RANK <- c(Bicycle = 1, `E-scooter / PMD` = 1, Moped = 2, Motorcycle = 3, Car = 4,
               Van = 5, `Bus or coach` = 6, `Heavy goods vehicle` = 7, `Tram or train` = 8)

# Vehicles ----------------------------------------------------------------------
vehicles <- vehicles |>
  mutate(key = if_else(year >= 2019, veh_uid, veh_id),
         veh_group = veh_group(veh_cat),
         rank = unname(MASS_RANK[veh_group]))

# Heaviest other vehicle in the same accident, for each vehicle
top_two <- vehicles |>
  filter(!is.na(rank)) |>
  group_by(acc_id) |>
  summarise(max1 = max(rank), n_max1 = sum(rank == max(rank)),
            max2 = suppressWarnings(max(rank[rank < max(rank)])), n_veh = n())
group_of_rank <- c("Bicycle or PMD", "Moped", "Motorcycle", "Car", "Van", "Bus or coach",
                   "Heavy goods vehicle", "Tram or train")
vehicles <- vehicles |>
  left_join(top_two, by = "acc_id") |>
  mutate(opp_rank = case_when(is.na(rank) | n_veh == 1 ~ NA_real_,
                              rank < max1 | n_max1 > 1 ~ max1,
                              is.finite(max2) ~ max2),
         opponent = case_when(n_veh == 1 | is.na(n_veh) ~ "No other vehicle",
                              !is.na(opp_rank) ~ group_of_rank[opp_rank],
                              .default = "Other"))

# Accidents ---------------------------------------------------------------------
severity_counts <- users |>
  group_by(acc_id) |>
  summarise(n_users = n(),
            n_killed = sum(severity == 2L, na.rm = TRUE),
            n_hospitalised = sum(severity == 3L, na.rm = TRUE),
            n_slight = sum(severity == 4L, na.rm = TRUE),
            n_pedestrians = sum(user_cat %in% c(3L, 4L)))
n_vehicles <- count(vehicles, acc_id, name = "n_vehicles")

period_of_day <- function(h) {
  case_when(h %in% c(22:23, 0:5) ~ "Night (22h-6h)", h %in% 6:9 ~ "Morning (6h-10h)",
            h %in% 10:15 ~ "Midday (10h-16h)", h %in% 16:21 ~ "Evening (16h-22h)") |>
    factor(levels = c("Morning (6h-10h)", "Midday (10h-16h)", "Evening (16h-22h)",
                      "Night (22h-6h)"))
}

accidents <- characteristics |>
  left_join(places |> select(-year), by = "acc_id") |>
  left_join(severity_counts, by = "acc_id") |>
  left_join(n_vehicles, by = "acc_id") |>
  mutate(
    date = make_date(year, month, day),
    weekday = factor(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")[wday(date, week_start = 1)],
                     levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    weekend = weekday %in% c("Sat", "Sun"),
    period = period_of_day(hour),
    area = if_else(str_starts(department, "97"), "Overseas", "Mainland"),
    n_vehicles = replace_na(n_vehicles, 0L),
    fatal = n_killed > 0,
    lighting = lab(lighting, LIGHTING), weather = lab(weather, WEATHER),
    collision = lab(collision, COLLISION), road = lab(road_cat, ROAD),
    intersection = lab(intersection, INTERSECTION), surface = lab(surface, SURFACE),
    built_up = factor(c(`1` = "Outside built-up area", `2` = "Built-up area")[as.character(urban)])
  ) |>
  select(acc_id, year, date, month, weekday, weekend, hour, period, area, department,
         commune, lat, lon, built_up, road, intersection, lighting, weather, surface,
         collision, speed_limit, n_lanes, n_vehicles, n_users, n_pedestrians, n_killed,
         n_hospitalised, n_slight, fatal)

# Users -------------------------------------------------------------------------
age_group <- function(age) {
  cut(age, c(-Inf, 13, 17, 24, 34, 44, 54, 64, 74, Inf),
      labels = c("0-13", "14-17", "18-24", "25-34", "35-44", "45-54", "55-64",
                 "65-74", "75+"))
}

users_full <- users |>
  mutate(key = if_else(year >= 2019, veh_uid, veh_id)) |>
  left_join(vehicles |> select(acc_id, key, veh_group, veh_cat, opponent, manoeuvre,
                               fixed_obstacle, mobile_obstacle, impact_point, motor),
            by = c("acc_id", "key")) |>
  left_join(accidents |> select(-year), by = "acc_id") |>
  mutate(
    severity = factor(unname(SEVERITY[as.character(severity)]), levels = SEVERITY),
    killed = severity == "Killed",
    severe = severity %in% c("Killed", "Hospitalised"),
    role = lab(user_cat, USER_CAT),
    # Pedestrians get their own mode; the vehicle linked to them is the one that hit them
    mode = factor(if_else(role == "Pedestrian", "Pedestrian", veh_group),
                  levels = c(VEH_GROUPS, "Pedestrian")),
    opponent = if_else(role == "Pedestrian",
                       if_else(veh_group %in% c("Bicycle", "E-scooter / PMD"), "Bicycle or PMD",
                               veh_group),
                       opponent),
    # Seat belt (or child seat) for enclosed vehicles, helmet for two-wheelers and PMDs.
    # Coded this way from 2019 only (secu1-secu3); earlier years use another scheme.
    two_wheel = veh_group %in% c("Motorcycle", "Moped", "Bicycle", "E-scooter / PMD"),
    worn = if_else(two_wheel,
                   safety_1 %in% 2L | safety_2 %in% 2L | safety_3 %in% 2L,
                   safety_1 %in% c(1L, 3L) | safety_2 %in% c(1L, 3L) | safety_3 %in% c(1L, 3L)),
    none = coalesce(safety_1 == 0L, FALSE),
    protection = case_when(year < 2019 ~ NA_character_,
                           role == "Pedestrian" ~ "Not applicable",
                           worn ~ "Worn", none ~ "Not worn", .default = "Unknown") |>
      factor(levels = c("Worn", "Not worn", "Unknown", "Not applicable")),
    sex = factor(c(`1` = "Male", `2` = "Female")[as.character(sex)]),
    age = year - birth_year,
    age_group = age_group(age),
    trip = lab(trip, TRIP),
    veh_group = factor(veh_group, levels = VEH_GROUPS)
  ) |>
  select(acc_id, year, veh_id, role, mode, veh_group, veh_cat, opponent, severity, killed,
         severe, sex, age, age_group, birth_year, trip, seat, protection, safety_2005, safety_1,
         safety_2, safety_3, manoeuvre, fixed_obstacle, mobile_obstacle, impact_point,
         motor, date, month, weekday, weekend, hour, period, area, department, built_up,
         road, intersection, lighting, weather, surface, collision, speed_limit, n_lanes,
         n_vehicles, n_users, lat, lon)

# Consistency checks -------------------------------------------------------------
checks <- tribble(
  ~check, ~ok,
  "Every user kept exactly once after the joins", nrow(users_full) == nrow(users),
  "Every accident kept exactly once", nrow(accidents) == nrow(characteristics) &&
    !anyDuplicated(accidents$acc_id),
  "Users per accident match n_users", sum(accidents$n_users) == nrow(users_full),
  "Killed users match the accident totals", sum(users_full$killed, na.rm = TRUE) ==
    sum(accidents$n_killed),
  "All users have a mode", !anyNA(users_full$mode[!is.na(users_full$veh_group) |
                                                    users_full$role == "Pedestrian"]),
  "Ages within 0-105", all(between(users_full$age, 0, 105), na.rm = TRUE)
)
# Size of the original join (users x vehicles on the accident id only), for the README
naive_rows <- accidents |> summarise(n = sum(as.numeric(n_users) * pmax(n_vehicles, 1))) |> pull(n)
checks <- add_row(checks, check = sprintf(
  "Info: joining users and vehicles on the accident id alone gives %s rows instead of %s (+%.0f%%)",
  format(naive_rows, big.mark = ","), format(nrow(users_full), big.mark = ","),
  100 * (naive_rows / nrow(users_full) - 1)), ok = TRUE)
save_table(checks, "consistency_checks")
for (i in seq_len(nrow(checks))) message(if (checks$ok[i]) "OK   " else "FAIL ", checks$check[i])
stopifnot(all(checks$ok))

write_pq(accidents, "accidents", DIR_FINAL)
write_pq(users_full, "users", DIR_FINAL)
message(sprintf("accidents: %s rows, users: %s rows",
                format(nrow(accidents), big.mark = ","), format(nrow(users_full), big.mark = ",")))
