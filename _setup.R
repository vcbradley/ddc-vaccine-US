## set up
pkg.list <- c(
  # general data wrangling
  "dplyr",
  "tidyr",
  "glue",
  "purrr",
  "fs",
  "readr",
  "stringr",
  # data reading
  "dataverse",
  "haven",
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
color_pal_df <- tribble(
  ~survey,                   ~arxiv,  ~rainier, ~okabe,
  "Delphi-Facebook",        "#4891dc", "#9FB6DA", "#0072B2",
  "Census Household Pulse", "#69913b", "#759C44", "#009E73",
  "Axios-Ipsos",            "#cf7a30", "#965127", "#D55E00"
)
color_pal <- select(color_pal_df, survey, rainier) %>% tibble::deframe()
color_pal["Delphi-Facebook"] <- "#0072B2"
scale_values <- c("CDC (benchmark)" = "grey50", color_pal)

shape_values <- c(
  "Delphi-Facebook" = 17,
  "Census Household Pulse" = 15,
  "Axios-Ipsos" = 19
)


# dataverse path
dvdoi <- "10.7910/DVN/GKBUUK"
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
