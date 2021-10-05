library(haven)

#' Custom code for stacking Axios sav
read_ax_drop <- function(x) {
  haven::read_sav(x) %>%
    select(-ppmsacat) %>%
    mutate_if(is.labelled, as_factor) %>%
    mutate(
      date = str_remove_all(path_file(x), "(Axios-Ipsos\\s|Data\\.sav)"),
      date = str_c(date, ", 2021"),
      date = mdy(date)
    ) %>%
    mutate(date = replace(date, str_detect(x, "Axios W42 032221.sav"), ymd("2021-03-22")))
}


#' This data is one row per microdata
cleanIPSOSdata <- function() {
  axios_dates <- c("April 19", "April 5", "March 1", "March 22", "March 8", "May 10")
  axios_paths <- c(
    glue("Axios-Ipsos {axios_dates} Data.tab"),
    "Axios W42 032221.tab"
  )

  ax_raw0 <- map_dfr(
    .x = axios_paths,
    .f = function(x) {
      get_dataframe_by_name(
        filename = x,
        dataset = dvdoi,
        original = TRUE,
        .f = read_ax_drop
      )
    }
  )

  ax_raw <- ax_raw0 %>%
    mutate(
      educ = fct_collapse(ppeduc5,
        `HS or Less` = c(
          "No high school diploma or GED",
          "High school graduate (high school diploma or the equivalent GED)"
        )
      ),
      wt_final = coalesce(wt_final, wt_final_pid)
    ) %>%
    relocate(date, WAVE, matches("wt_final"), matches("pp"))

  # code vax, hes, will as binary
  ax_coded <- ax_raw %>%
    mutate(
      # willing is 0 if you have receivd, NA if you skipped the question, and 1 if you are likely
      willing = as.integer(Q73 %in% c("Somewhat likely", "Very likely")),
      willing = replace(willing, Q73 == "Skipped", NA_real_),
      willing = replace(willing, Q107_1 == "Yes, I have received the vaccine", 0),
      # vax is whether you got it
      vaccinated = as.integer(Q107_1 == "Yes, I have received the vaccine")
    ) %>%
    # hesitant is the subtraction
    mutate(hesitant = 1 - vaccinated - willing) %>%
    relocate(date, WAVE, wt_final, willing, hesitant, vaccinated)


  cat("Writing archived data")
  ax_coded %>%
    filter(is.na(acsnet3)) %>% # public data
    write_rds("data/axios-ipsos/axios-ipsos_stacked.rds")

  cat("Writing offlineflag.rds")
  ax_coded %>%
    filter(!is.na(acsnet3)) %>% # custom offline flag
    write_rds("data/axios-ipsos/axios-ipsos_offlineflag.rds")
}



#' This data is one row per estimate
cleanIPSOStables <- function() {
  ipsos_text <- get_dataframe_by_name(
    "axios_ipsos_full.tab",
    dataset = dvdoi,
    original =  TRUE,
    .f = read_csv
  )

  ipsos <- ipsos_text %>%
    mutate(
      source = "toplines",
      pct_hesitant = 1 - pct_haveorwillgetvax,
      pct_willing = pct_haveorwillgetvax - pct_vaccinated,
      pct_vaccinated_se = sqrt(deff * pct_vaccinated * (1 - pct_vaccinated) / n),
      pct_hesitant_se = sqrt(deff * pct_hesitant * (1 - pct_hesitant) / n),
      pct_willing_se = sqrt(deff * pct_willing * (1 - pct_willing) / n)
    )

  write.csv(ipsos, file = "data/final/axios_ipsos_cleaned.csv")
}
