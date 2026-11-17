# Shared settings, paths and plotting theme. Sourced by every script.

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
})

YEARS <- 2005:2025

DIR_RAW <- "data/raw"
DIR_INTERIM <- "data/interim"
DIR_FINAL <- "data/final"
DIR_FIG <- "figures"
DIR_RES <- "results"
for (d in c(DIR_INTERIM, DIR_FINAL, DIR_FIG, DIR_RES)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Coding changes documented by ONISR: new vehicle categories and safety-equipment
# coding from 2019, and "hospitalised" no longer an official indicator from 2018.
BREAK_YEARS <- c(2019)
COVID_YEAR <- 2020

# Palette: validated categorical order, sequential blues, reserved severity colours
COL <- c(blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a", yellow = "#eda100",
         magenta = "#e87ba4", green = "#008300", violet = "#6250d6", red = "#e34948")
SEVERITY_COL <- c("Unharmed" = "#a3a29b", "Slightly injured" = "#86b6ef",
                  "Hospitalised" = "#eb6834", "Killed" = "#8f1d1c")
BLUES <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
INK <- "#0b0b0b"
INK_2 <- "#52514e"
SURFACE <- "#fcfcfb"
GRID <- "#e4e3df"

theme_set(
  theme_minimal(base_size = 11) +
    theme(plot.background = element_rect(fill = SURFACE, colour = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = GRID, linewidth = 0.4),
          plot.title = element_text(face = "bold", colour = INK, size = 12.5),
          plot.title.position = "plot",
          plot.subtitle = element_text(colour = INK_2),
          plot.caption = element_text(colour = INK_2, size = 8.5, hjust = 0),
          axis.text = element_text(colour = INK_2),
          axis.title = element_text(colour = INK_2),
          strip.text = element_text(face = "bold", colour = INK, hjust = 0),
          legend.position = "top", legend.justification = "left")
)

save_fig <- function(plot, name, width = 9, height = 5.5, part = NULL) {
  dir <- if (is.null(part)) DIR_FIG else file.path(DIR_FIG, part)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(dir, paste0(name, ".png")), plot, width = width, height = height,
         dpi = 150, bg = SURFACE)
}

save_table <- function(df, name) write_csv(df, file.path(DIR_RES, paste0(name, ".csv")))

# Vertical markers for coding breaks and the 2020 lockdowns on yearly charts
year_markers <- function() {
  list(geom_vline(xintercept = BREAK_YEARS - 0.5, linetype = "dashed", colour = INK_2,
                  linewidth = 0.4),
       geom_vline(xintercept = COVID_YEAR, linetype = "dotted", colour = INK_2,
                  linewidth = 0.4))
}

read_pq <- function(name, dir = DIR_INTERIM) read_parquet(file.path(dir, paste0(name, ".parquet")))
write_pq <- function(df, name, dir = DIR_INTERIM) write_parquet(df, file.path(dir, paste0(name, ".parquet")))
