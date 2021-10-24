# Functions for prepping CDC data --------

getPctVaxxedUnder18 <- function(cdc_age_path) {

  # read in data

  cdcdata_age <-  get_dataframe_by_name(
    filename = cdc_age_path,
    dataset = dvdoi,
    original =  TRUE,
    .f = function(x) fread(x,
                           skip = 2,
                           select = c("Date",
                                      "Age Group",
                                      "Demographic Group",
                                      "People with at least one dose",
                                      "People who are fully vaccinated"))
  )

  setnames(cdcdata_age, c("date", "age_group", "n_vaccinated", "n_fully_vaccinated"))

  # limit to rows with age data
  cdcdata_age <- cdcdata_age %>%
    mutate(age_flag = as.numeric(grepl("Ages", age_group)), date = as.Date(date)) %>%
    filter(age_flag == 1) %>%
    select(-age_flag)


  # calculate total number of vaccinations 1) in total 2) over 18 and 3) under 18
  # all TX reported as
  cdcdata_age <- cdcdata_age %>%
    group_by(date) %>%
    mutate(
      n_fully_vaccinated = as.numeric(n_fully_vaccinated),
      n_vaccinated = as.numeric(n_vaccinated),
      under_18_flag = as.numeric(age_group %in% c("Ages_<18yrs", "Ages_<12yrs", "Ages_12-15_yrs", "Ages_16-17_yrs"))
    ) %>%
    summarize(
      n_vax_notx = sum(n_vaccinated),
      n_vax_notx_fully = sum(n_fully_vaccinated),
      n_vax_notx_over18 = sum(n_vaccinated * (1 - under_18_flag)),
      n_vax_notx_fully_over18 = sum(n_fully_vaccinated * (1 - under_18_flag)),
      n_vax_notx_under18 = sum(n_vaccinated * under_18_flag),
      n_vax_notx_fully_under18 = sum(n_fully_vaccinated * under_18_flag)
    ) %>%
    mutate(
      pct_vax_under18 = n_vax_notx_under18 / n_vax_notx,
      pct_vax_fully_under18 = n_vax_notx_fully_under18 / n_vax_notx_fully
    )

  # we only really need
  # n_vax_notx: the total number of vaccinations outside of TX on each day
  # pct_vax_under18: the proportion of total one-dose vaccinations administered
  # to under-18s on each day
  cdcdata_age <- cdcdata_age %>%
    select(date, n_vax_notx, n_vax_notx_over18, pct_vax_under18)

  return(cdcdata_age)
}



getStatePopTotals <- function(filepath = "SCPRC-EST2019-18+POP-RES.tab",
                              download = FALSE) {

  # "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv"

  # read in pop totals
  state_pop_totals <- get_dataframe_by_name(
    path_file(filepath),
    dataset = dvdoi,
    original = TRUE,
    .f = function(x) fread(x, select = c("NAME", "POPEST18PLUS2019")))

  # get state abbreviations from state names
  state_pop_totals <- merge(state_pop_totals,
                            cbind(state.abb, state.name),
                            by.x = "NAME",
                            by.y = "state.name",
                            all.x = TRUE)

  # fix missing abbrevs
  state_pop_totals[NAME == "United States", state.abb := "US"]
  state_pop_totals[NAME == "District of Columbia", state.abb := "DC"]
  state_pop_totals[NAME == "Puerto Rico Commonwealth", state.abb := "PR"]

  # rename columns
  setnames(state_pop_totals,
           old = c("NAME", "state.abb", "POPEST18PLUS2019"),
           new = c("state_name", "state", "pop_total"))

  return(state_pop_totals)
}




#' function to clean raw CDC vaccination data
#' data must be downloaded manually from https://covid.cdc.gov/covid-data-tracker/#vaccination-trends
#' or can used one of the historical sets supplied in the repo
#'
#' @param cdc_path path to file with CDC data
#'
cleanCDCdata <- function(cdc_path,
                         cdc_age_path,
                         statepop_download = FALSE,
                         statepop_filepath = "SCPRC-EST2019-18+POP-RES.tab") {
  # read in data
  cdcdata <-  get_dataframe_by_name(
    filename = cdc_path,
    dataset = dvdoi,
    original =  TRUE,
    .f = function(x) fread(x,
                           skip = 2,
                           select = c("Date Type",
                                      "Date",
                                      "Program",
                                      "People with at least One Dose Cumulative",
                                      "People Fully Vaccinated Cumulative"))
  )

  # rename columns
  setnames(cdcdata, c("report_type", "date", "program", "n_vaccinated_allages", "n_vaccinated_allages_fully"))

  # filter to only administered doses at the national level (includes LTC doses)
  cdcdata <- cdcdata %>%
    filter(report_type == "Admin" & program == "US") %>%
    mutate(
      date = as.Date(date),
      n_vaccinated_allages = as.numeric(n_vaccinated_allages),
      n_vaccinated_allages_fully = as.numeric(n_vaccinated_allages_fully)
    ) %>%
    rename(state = program)


  # we also need cdc doses by age data -- TX does not report doses by age, so need to impute
  cdc_by_age <- getPctVaxxedUnder18(cdc_age_path)

  # join age data onto rest of CDC data and use age data to impute the number of over-18 doses administered in TX
  cdcdata <- left_join(cdcdata, cdc_by_age, by = "date") %>%
    replace_na(list(n_vax_notx = 0, pct_vax_under18 = 0, n_vax_notx_over18 = 0)) %>%
    mutate(
      n_vax_tx = n_vaccinated_allages - n_vax_notx, # impute TX-only doses
      n_vax_tx_over18 = n_vax_tx * (1 - pct_vax_under18), # impute number of TX doses administered to adults
      n_pop_vaccinated = n_vax_tx_over18 + n_vax_notx_over18 # get total number of US adults with >=1 dose by adding TX and non-TX adult doses
    ) %>%
    select(date, state, n_pop_vaccinated)


  # get state pop totals to calculate % uptake
  state_pop_totals <- getStatePopTotals(filepath = statepop_filepath,
                                        download = statepop_download) %>%
    filter(state == "US") %>%
    select(-state_name)

  cdcdata <- left_join(cdcdata, state_pop_totals, by = "state") %>%
    mutate(pct_pop_vaccinated = n_pop_vaccinated / pop_total)

  return(cdcdata)
}


#' Get data from OWID
getOWIDdata <- function(download_date = NULL,
                        download = FALSE,
                        statepop_filepath = "data/raw/CDC/SCPRC-EST2019-18+POP-RES.tab",
                        statepop_download = FALSE) {

  # download new version of data
  filepath <- glue("owid_raw_{download_date}.tab")

  # read in data and clean
  owid <- get_dataframe_by_name(filepath, dataset = dvdoi, original = TRUE, .f = fread) %>%
    rename(state_name = location) %>%
    filter(state_name != "Long Term Care") %>%
    mutate(
      state_name = ifelse(state_name == "New York State", "New York", state_name),
      date = as.Date(date)
    ) %>%
    select(date, state_name, people_vaccinated)

  # impute missing data with time series interpolation
  owid <- owid %>%
    group_by(state_name) %>%
    mutate(
      n_pop_vaccinated = imputeTS::na_interpolation(people_vaccinated),
      n_pop_vaccinated_imputedflag = is.na(people_vaccinated)
    ) %>%
    ungroup()

  cat("Check number of days imputed:\n")
  print(owid %>% summarize(n_imputed = sum(n_pop_vaccinated_imputedflag),
                           pct_imputed = mean(n_pop_vaccinated_imputedflag)))


  # merge in state pop totals
  state_pop_totals <- getStatePopTotals(
    filepath = statepop_filepath,
    download = statepop_download)

  owid <- left_join(owid, state_pop_totals, by = "state_name") %>%
    mutate(
      pct_pop_vaccinated = n_pop_vaccinated / pop_total,
      state = ifelse(is.na(state), state_name, state)
    ) %>%
    select(date,
           state,
           n_pop_vaccinated,
           n_pop_vaccinated_imputedflag,
           pct_pop_vaccinated,
           pop_total)

  return(owid)
}



# RUN CLEAN CDC DATA --------

getBenchmark <- function(benchmark_date,
                         cdc_path,
                         cdc_age_path,
                         download_owid = FALSE,
                         statepop_download = FALSE,
                         statepop_filepath = "data/raw/CDC/SCPRC-EST2019-18+POP-RES.tab") {

  ##### CDC
  # clean and write out CDC data
  cdc <- cleanCDCdata(cdc_path,
                      cdc_age_path,
                      statepop_download = statepop_download,
                      statepop_filepath = statepop_filepath)

  write.csv(cdc,
            file = file.path("data", "CDC", paste0("cdc_cleaned_", benchmark_date, ".csv")),
            row.names = FALSE
  )


  #### OWID
  # clean and write out OWID data
  # only if the data exists for that date
  if (!is.null(benchmark_date)) {
    owid <- getOWIDdata(
      download_date = benchmark_date,
      download = download_owid,
      statepop_download = statepop_download,
      statepop_filepath = statepop_filepath
    )

    write.csv(owid,
              file = file.path("data", "OWID", paste0("owid_cleaned_", benchmark_date, ".csv")),
              row.names = FALSE
    )

    # check OWID interpolation
    check_owid_interp(owid, benchmark_date)

    #### combine CDC and OWID data
    benchmark <- bind_rows(
      cdc %>% mutate(source = "CDC_historical"),
      owid %>% mutate(source = "OWID")
    )
  }

  if (is.null(benchmark_date)) {
    benchmark <- cdc %>% mutate(source = "CDC_historical")
  }


  benchmark <- benchmark %>% mutate(
    day_of_vax_program = difftime(date, min(date) - 1, units = "days"),
    day_of_vax_program = as.numeric(day_of_vax_program),
    sd_G = sqrt(pct_pop_vaccinated * (1 - pct_pop_vaccinated))
  )

  ## write out to file
  write.csv(benchmark,
            file = file.path("data", "final", glue("benchmark_{benchmark_date}.csv")),
            row.names = FALSE
  )


  # check comparison
  comp_cdc_owid(benchmark, benchmark_date)

  return("Done!")
}



#' Compare CDC and OWID and save the result
comp_cdc_owid <- function(benchmark, benchmark_date) {
  gg_chk <- ggplot(filter(benchmark, state == "US")) +
    geom_line(aes(x = date, y = pct_pop_vaccinated, color = source)) +
    theme_pubclean() +
    labs(title = "Comparison of Vaccine Uptake from CDC and OWID", x = "", y = "% vaccinated") +
    scale_y_continuous(labels = percent)
  ggsave(glue("data/CDC/checks/plot_benchmark_comparison_{benchmark_date}.png"),
         gg_chk,
         height = 5, width = 7, units = "in")
}


# Check OWID interpolation ----

check_owid_interp <- function(owid, benchmark_date) {
  gg_chk <- owid %>%
    ggplot(aes(x = date,
               y = pct_pop_vaccinated,
               group = state,
               color = n_pop_vaccinated_imputedflag)) +
    geom_line() +
    ggtitle("Linear interpolation of missing state benchmark data") +
    facet_wrap(~ as.numeric(state == "US"), scales = "free") +
    theme_bw()

  ggsave(
    filename = path("data", "CDC", "checks",
                    glue("plot_owid_interp_check_{benchmark_date}.png")),
    gg_chk,
    width = 8, height = 4, units = "in", device = "png"
  )
}