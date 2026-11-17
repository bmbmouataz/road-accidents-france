# Reads the 84 raw files and stacks each table over 2005-2025 with one schema.
#
# Format changes handled here (see data/README.md):
#   - separator: "," until 2018 (tab for characteristics 2009), ";" from 2019
#   - encoding: latin1 for most characteristics files until 2018, UTF-8 otherwise
#   - accident key named Accident_Id in characteristics 2022
#   - year on two digits and time as an integer ("1930") until 2018, "19:30" after
#   - department on three digits until 2018 ("590" = Nord, "201" = Corse-du-Sud)
#   - coordinates in degrees x 100000 until 2018, decimal comma after

source("R/00_utils.R")

read_raw <- function(table, year) {
  path <- file.path(DIR_RAW, sprintf("%s_%d.csv", table, year))
  header <- read_lines(path, n_max = 1)
  delim <- c(",", ";", "\t")[which.max(str_count(header, c(",", ";", "\t")))]
  enc <- if (all(validUTF8(read_lines(path, n_max = 50000)))) "UTF-8" else "latin1"
  read_delim(path, delim = delim, col_types = cols(.default = col_character()),
             locale = locale(encoding = enc), na = c("", "NA", "N/A", "-1", " -1"),
             trim_ws = TRUE, progress = FALSE, show_col_types = FALSE) |>
    rename_with(\(n) str_squish(str_replace_all(n, "[\r\n\"]", ""))) |>
    rename(any_of(c(Num_Acc = "Accident_Id", Num_Acc = "num_acc"))) |>
    mutate(year = year)
}

stack_years <- function(table) map(YEARS, \(y) read_raw(table, y)) |> list_rbind()

int <- function(x) suppressWarnings(as.integer(x))
# id_vehicule exists from 2019 and contains (non-breaking) spaces: "155 680 557"
clean_id <- function(x) if (is.null(x)) NA_character_ else str_remove_all(x, "[\\s ]")
num_comma <- function(x) suppressWarnings(as.numeric(str_replace(x, ",", ".")))

department_code <- function(dep, year) {
  dep <- str_squish(dep)
  old <- year <= 2018
  case_when(
    !old ~ str_pad(dep, 2, pad = "0"),
    dep == "201" ~ "2A",
    dep == "202" ~ "2B",
    str_starts(dep, "97") ~ dep,   # overseas departments keep three digits
    TRUE ~ str_sub(str_pad(dep, 3, pad = "0"), 1, 2)
  )
}

characteristics <- stack_years("characteristics") |>
  transmute(
    acc_id = Num_Acc, year,
    month = int(mois), day = int(jour),
    hour = if_else(year <= 2018, int(hrmn) %/% 100L, int(str_sub(hrmn, 1, 2))),
    minute = if_else(year <= 2018, int(hrmn) %% 100L, int(str_sub(hrmn, 4, 5))),
    lighting = int(lum), urban = int(agg), intersection = int(int), weather = int(atm),
    collision = int(col), department = department_code(dep, year),
    commune = com,
    lat = if_else(year <= 2018, num_comma(lat) / 1e5, num_comma(lat)),
    lon = if_else(year <= 2018, num_comma(long) / 1e5, num_comma(long)),
    # Before 2019, coordinates are only usable for mainland France (gps == "M"),
    # "0000000" means missing, and the 2009 file holds no valid coordinates at all
    no_coord = year <= 2018 & (is.na(gps) | gps != "M" | lat == 0 | lon == 0 | year == 2009),
    lat = if_else(no_coord, NA_real_, lat),
    lon = if_else(no_coord, NA_real_, lon),
    no_coord = NULL
  )

places <- stack_years("places") |>
  transmute(
    acc_id = Num_Acc, year,
    road_cat = int(catr), traffic_regime = int(circ), n_lanes = int(nbv),
    reserved_lane = int(vosp), slope = int(prof), layout = int(plan),
    surface = int(surf), infrastructure = int(infra), situation = int(situ),
    speed_limit = int(vma)
  )

vehicles <- stack_years("vehicles") |>
  transmute(
    acc_id = Num_Acc, year, veh_id = num_veh, veh_uid = clean_id(id_vehicule),
    direction = int(senc), veh_cat = int(catv), fixed_obstacle = int(obs),
    mobile_obstacle = int(obsm), impact_point = int(choc), manoeuvre = int(manv),
    motor = int(motor)
  )

users <- stack_years("users") |>
  transmute(
    acc_id = Num_Acc, year, veh_id = num_veh, veh_uid = clean_id(id_vehicule),
    seat = int(place), user_cat = int(catu), severity = int(grav), sex = int(sexe),
    trip = int(trajet), birth_year = int(an_nais),
    safety_2005 = secu, safety_1 = int(secu1), safety_2 = int(secu2), safety_3 = int(secu3),
    ped_location = int(locp), ped_action = actp, ped_alone = int(etatp)
  )

write_pq(characteristics, "characteristics")
write_pq(places, "places")
write_pq(vehicles, "vehicles")
write_pq(users, "users")

rows <- tibble(table = c("characteristics", "places", "vehicles", "users"),
               rows = c(nrow(characteristics), nrow(places), nrow(vehicles), nrow(users)))
print(rows)
