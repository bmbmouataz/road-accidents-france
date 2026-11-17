# Downloads the official BAAC files (road injury accidents, ONISR) from data.gouv.fr.
# Files are cached in data/raw/ and skipped if already present.

source("R/00_utils.R")

DATASET_ID <- "53698f4ca3a729239d2036df"
TABLES <- c(characteristics = "^(caracteristiques|carcteristiques|caract)",
            places = "^lieux", vehicles = "^vehicules[-_]\\d{4}", users = "^usagers")

resources <- jsonlite::fromJSON(
  paste0("https://www.data.gouv.fr/api/1/datasets/", DATASET_ID, "/")
)$resources

files <- resources |>
  as_tibble() |>
  transmute(title = tolower(title), url) |>
  mutate(year = as.integer(str_extract(title, "(?<=[-_])\\d{4}(?=\\.csv$)")),
         table = map_chr(title, \(t) {
           hit <- names(TABLES)[str_detect(t, TABLES)]
           if (length(hit) == 1) hit else NA_character_
         })) |>
  filter(!is.na(table), year %in% YEARS) |>
  distinct(table, year, .keep_all = TRUE) |>
  mutate(dest = file.path(DIR_RAW, sprintf("%s_%d.csv", table, year)))

stopifnot(nrow(files) == 4 * length(YEARS))

dir.create(DIR_RAW, recursive = TRUE, showWarnings = FALSE)
todo <- filter(files, !file.exists(dest))
for (i in seq_len(nrow(todo))) {
  message(sprintf("[%d/%d] %s %d", i, nrow(todo), todo$table[i], todo$year[i]))
  download.file(todo$url[i], todo$dest[i], mode = "wb", quiet = TRUE)
}

# Department populations (INSEE estimates, 1975-2023), used for per-capita rates in the maps
pop_url <- "https://www.insee.fr/fr/statistiques/fichier/1893198/estim-pop-dep-sexe-gca-1975-2023.xls"
pop_dest <- file.path(DIR_RAW, "insee_population_departments.xls")
if (!file.exists(pop_dest)) download.file(pop_url, pop_dest, mode = "wb", quiet = TRUE)

# Simplified department boundaries (derived from IGN ADMIN EXPRESS, open licence)
geo_url <- paste0("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/",
                  "departements-version-simplifiee.geojson")
geo_dest <- file.path(DIR_RAW, "departments.geojson")
if (!file.exists(geo_dest)) download.file(geo_url, geo_dest, mode = "wb", quiet = TRUE)

write_csv(select(files, table, year, url), file.path(DIR_RAW, "sources.csv"))
message("Raw files: ", nrow(files), " BAAC tables + INSEE populations in ", DIR_RAW)
