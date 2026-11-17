# Installs the R packages used by the project (R >= 4.4).
packages <- c("tidyverse", "arrow", "sf", "leaflet", "htmlwidgets", "rmarkdown", "readxl",
              "jsonlite", "patchwork", "ggrepel", "scales", "fixest", "Matrix", "glmnet",
              "ranger", "xgboost", "shapviz", "knitr")
missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
