# Part 8 - Aggregated data for the Quarto dashboard (site/dashboard.qmd).
# The dashboard filters in the browser, so it only receives small pre-aggregated tables.

source("R/00_utils.R")

OUT <- "site/data"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

usr <- read_pq("users", DIR_FINAL)
acc <- read_pq("accidents", DIR_FINAL)
geo <- read_csv(file.path(DIR_RES, "geo_departments.csv"), show_col_types = FALSE)

# People by year x department x mode, with severity counts
people <- usr |>
  filter(!is.na(severity), !is.na(mode)) |>
  group_by(year, department, mode) |>
  summarise(people = n(), killed = sum(severity == "Killed"),
            hospitalised = sum(severity == "Hospitalised"),
            slight = sum(severity == "Slightly injured"), .groups = "drop")
write_csv(people, file.path(OUT, "people.csv"))

# Accidents by year x department, with population for per-capita rates
pop <- map(2005:2023, \(y) {
  readxl::read_excel(file.path(DIR_RAW, "insee_population_departments.xls"), sheet = as.character(y),
                     skip = 5, col_names = FALSE, .name_repair = "minimal")[, c(1, 8)] |>
    set_names(c("department", "population")) |>
    filter(str_detect(department, "^(\\d{2}|2A|2B|97\\d)$")) |>
    mutate(year = y, population = as.numeric(population))
}) |> list_rbind()
pop <- bind_rows(pop, map(2024:2025, \(y) filter(pop, year == 2023) |> mutate(year = y)))
accidents <- acc |>
  group_by(year, department) |>
  summarise(accidents = n(), fatal_accidents = sum(fatal), .groups = "drop") |>
  left_join(pop, by = c("year", "department"))
write_csv(accidents, file.path(OUT, "accidents.csv"))

write_csv(select(geo, department, name), file.path(OUT, "departments.csv"))

# Mainland boundaries (already simplified, 570 KB)
invisible(file.copy(file.path(DIR_RAW, "departments.geojson"), OUT, overwrite = TRUE))

# Model results shown on the "Models" page
invisible(file.copy(file.path(DIR_RES, c("logit_odds_ratios.csv", "ml_test_metrics.csv", "ml_shap_importance.csv")),
          OUT, overwrite = TRUE))

sizes <- file.info(list.files(OUT, full.names = TRUE))$size
message(sprintf("Dashboard data: %d files, %.1f MB", length(sizes), sum(sizes) / 1e6))
