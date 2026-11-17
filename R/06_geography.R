# Part 4 - Geography: per-department rates, maps and an interactive leaflet map.
#
# Raw counts mostly map population, so every indicator is a rate:
#   - accidents per 100,000 inhabitants and killed per million inhabitants (INSEE populations)
#   - killed per 100 accidents, which needs no population and measures how severe accidents are
# Recent period: 2021-2025 (2020 excluded). Reference period: 2010-2014.

source("R/00_utils.R")
suppressPackageStartupMessages({
  library(sf)
  library(leaflet)
})
sf_use_s2(FALSE)

PART <- "04_geography"
RECENT <- 2021:2025
REFERENCE <- 2010:2014

acc <- read_pq("accidents", DIR_FINAL)

# Populations: one sheet per year, 1975-2023. Years after 2023 reuse 2023.
pop_file <- file.path(DIR_RAW, "insee_population_departments.xls")
population <- map(2005:2023, \(y) {
  readxl::read_excel(pop_file, sheet = as.character(y), skip = 5, col_names = FALSE,
                     .name_repair = "minimal")[, c(1, 2, 8)] |>
    set_names(c("department", "name", "population")) |>
    filter(str_detect(department, "^(\\d{2}|2A|2B|97\\d)$")) |>
    mutate(year = y, population = as.numeric(population),
           # readxl returns latin1 bytes for this old .xls file
           name = if_else(validUTF8(name), name, iconv(name, "latin1", "UTF-8")))
}) |> list_rbind()
population <- bind_rows(population,
                        map(2024:2025, \(y) filter(population, year == 2023) |> mutate(year = y)))
names_dep <- distinct(population, department, name)

by_dep_year <- acc |>
  group_by(department, year) |>
  summarise(accidents = n(), killed = sum(n_killed), .groups = "drop") |>
  left_join(population, by = c("department", "year"))

rates <- function(df, years) {
  df |>
    filter(year %in% years) |>
    group_by(department) |>
    summarise(accidents = sum(accidents), killed = sum(killed),
              person_years = sum(population), years = n()) |>
    mutate(accidents_per_100k = 1e5 * accidents / person_years,
           killed_per_million = 1e6 * killed / person_years,
           killed_per_100_acc = 100 * killed / accidents)
}

geo <- rates(by_dep_year, RECENT) |>
  left_join(rates(by_dep_year, REFERENCE) |>
              select(department, killed_ref = killed, killed_per_million_ref = killed_per_million),
            by = "department") |>
  mutate(change_killed_per_million = killed_per_million / killed_per_million_ref - 1) |>
  left_join(names_dep, by = "department") |>
  relocate(name, .after = department)
save_table(geo, "geo_departments")

national <- by_dep_year |>
  filter(year %in% RECENT, !str_starts(department, "97")) |>
  summarise(killed_per_million = 1e6 * sum(killed) / sum(population),
            accidents_per_100k = 1e5 * sum(accidents) / sum(population))
message(sprintf("Mainland %d-%d: %.0f killed per million, %.0f accidents per 100k",
                min(RECENT), max(RECENT), national$killed_per_million, national$accidents_per_100k))

# Static maps (mainland France) --------------------------------------------------------
shapes <- st_read(file.path(DIR_RAW, "departments.geojson"), quiet = TRUE) |>
  rename(department = code) |>
  left_join(geo, by = "department")

map_theme <- theme_void() +
  theme(plot.background = element_rect(fill = SURFACE, colour = NA),
        plot.title = element_text(face = "bold", size = 11.5, colour = INK),
        plot.subtitle = element_text(colour = INK_2, size = 9),
        legend.position = "bottom", legend.key.width = unit(1.3, "cm"),
        legend.title = element_blank(), plot.margin = margin(6, 6, 6, 6))

choropleth <- function(var, title, subtitle, labels = scales::label_comma()) {
  ggplot(shapes) +
    geom_sf(aes(fill = .data[[var]]), colour = SURFACE, linewidth = 0.15) +
    scale_fill_gradientn(colours = BLUES, labels = labels) +
    labs(title = title, subtitle = subtitle) +
    map_theme
}

m1 <- choropleth("accidents_per_100k", "Accidents per 100,000 inhabitants",
                 "Highest in Paris, its inner suburbs and Corsica")
m2 <- choropleth("killed_per_million", "Killed per million inhabitants",
                 "Highest in rural departments such as Haute-Marne, Jura and Nievre")
m3 <- choropleth("killed_per_100_acc", "Killed per 100 accidents",
                 "How severe accidents are, independent of population")
p <- patchwork::wrap_plots(m1, m2, m3, nrow = 1) +
  patchwork::plot_annotation(
    title = "Where accidents happen is not where people die on the road",
    subtitle = with(filter(geo, !str_starts(department, "97")), sprintf(
      "Mainland France, %d-%d average. Deaths per 100 accidents range from %.1f in %s to %.1f in %s.",
      min(RECENT), max(RECENT), min(killed_per_100_acc), name[which.min(killed_per_100_acc)],
      max(killed_per_100_acc), name[which.max(killed_per_100_acc)])))
save_fig(p, "01_department_maps", width = 14, height = 5.6, part = PART)

lim <- max(abs(shapes$change_killed_per_million), na.rm = TRUE)
p <- ggplot(shapes) +
  geom_sf(aes(fill = change_killed_per_million), colour = SURFACE, linewidth = 0.15) +
  scale_fill_gradient2(low = COL[["blue"]], mid = "#f1f0ec", high = COL[["red"]], midpoint = 0,
                       limits = c(-lim, lim), labels = scales::label_percent()) +
  labs(title = "Change in road deaths per million inhabitants",
       subtitle = sprintf("%d-%d compared with %d-%d. Blue: fewer deaths.",
                          min(RECENT), max(RECENT), min(REFERENCE), max(REFERENCE))) +
  map_theme
save_fig(p, "02_change_map", width = 6.5, height = 6.5, part = PART)

# Urban versus rural: frequency against severity --------------------------------------
lab_deps <- geo |>
  filter(!str_starts(department, "97")) |>
  filter(rank(-accidents_per_100k) <= 4 | rank(-killed_per_100_acc) <= 4 |
           rank(accidents_per_100k) <= 2 | rank(killed_per_100_acc) <= 2)
p <- geo |>
  mutate(area = if_else(str_starts(department, "97"), "Overseas", "Mainland")) |>
  ggplot(aes(accidents_per_100k, killed_per_100_acc)) +
  geom_point(aes(colour = area, size = person_years / years), alpha = 0.8) +
  ggrepel::geom_text_repel(data = lab_deps, aes(label = name), size = 3.1, colour = INK_2,
                           seed = 1, min.segment.length = 0) +
  scale_colour_manual(values = unname(COL[c("blue", "orange")])) +
  scale_size_area(max_size = 9, guide = "none") +
  scale_x_log10() +
  labs(title = "Dense departments have many accidents but few deaths per accident",
       subtitle = sprintf("One point per department, %d-%d. Point size: population.",
                          min(RECENT), max(RECENT)),
       x = "Accidents per 100,000 inhabitants (log scale)", y = "Killed per 100 accidents",
       colour = NULL)
save_fig(p, "03_frequency_vs_severity", width = 9.5, height = 6, part = PART)

# Interactive map ---------------------------------------------------------------------
pal <- colorNumeric(BLUES, shapes$killed_per_million)
fatal_points <- acc |>
  filter(year == max(RECENT), fatal, !is.na(lat), !str_starts(department, "97"))
popup <- with(shapes, sprintf(
  "<b>%s (%s)</b><br>Accidents per 100k inhabitants: %.0f<br>Killed per million: %.0f<br>Killed per 100 accidents: %.1f<br>Change in deaths per million since %d-%d: %+.0f%%",
  nom, department, accidents_per_100k, killed_per_million, killed_per_100_acc,
  min(REFERENCE), max(REFERENCE), 100 * change_killed_per_million))

widget <- leaflet(shapes) |>
  addProviderTiles(providers$Esri.WorldGrayCanvas) |>
  addPolygons(fillColor = ~pal(killed_per_million), fillOpacity = 0.75, color = "white",
              weight = 0.6, popup = popup, label = ~nom, group = "Killed per million",
              highlightOptions = highlightOptions(weight = 2, color = INK, bringToFront = TRUE)) |>
  addCircleMarkers(data = fatal_points, lng = ~lon, lat = ~lat, radius = 2.5, stroke = FALSE,
                   fillColor = COL[["red"]], fillOpacity = 0.7, group = "Fatal accidents 2025",
                   popup = ~sprintf("%s, %s<br>%s, %s<br>Killed: %d", date, road, lighting,
                                    built_up, n_killed)) |>
  addLegend(pal = pal, values = ~killed_per_million, title = "Killed per million<br>inhabitants",
            position = "bottomright") |>
  addLayersControl(overlayGroups = c("Killed per million", "Fatal accidents 2025"),
                   options = layersControlOptions(collapsed = FALSE))

pandoc <- Sys.which("pandoc")
if (!nzchar(pandoc)) {
  quarto_pandoc <- file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Quarto/bin/tools/pandoc.exe")
  if (file.exists(quarto_pandoc)) Sys.setenv(RSTUDIO_PANDOC = dirname(quarto_pandoc))
}
dir.create(file.path(DIR_FIG, PART), showWarnings = FALSE)
htmlwidgets::saveWidget(widget, normalizePath(file.path(DIR_FIG, PART, "interactive_map.html"),
                                              mustWork = FALSE),
                        selfcontained = rmarkdown::pandoc_available(), title = "Road deaths by department")
unlink(file.path(DIR_FIG, PART, "interactive_map_files"), recursive = TRUE)
message("Geography outputs written to ", file.path(DIR_FIG, PART))
