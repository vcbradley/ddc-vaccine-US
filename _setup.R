## set up
pkg.list <- c(
  # general data wrangling
  "dplyr",
  "tidyr",
  "glue",
  "purrr",
  "fs",
  "readr",
  "tidyverse",
  # data reading
  "curl",
  "readxl",
  "data.table",
  # graphs and tables
  "ggplot2",
  "gridExtra",
  "ggpubr",
  "grid",
  "scales",
  "lemon",
  "knitr",
  "kableExtra",
  "lubridate",
  "patchwork",
  "ggrepel"
)


for (p in pkg.list) {
  loaded <- require(p, character.only = TRUE)
  if (!loaded) {
    cat(paste0("Installing package: ", p))
    install.packages(p)
    library(p, character.only = TRUE)
  }
}


# set color scales
scale_values <- c(
  "CDC (benchmark)" = "darkgray",
  "Delphi-Facebook" = "#9FB6DA", # blue
  "Census Household Pulse" = "#759C44", # green
  "Axios-Ipsos" = "#965127" # orange
)
