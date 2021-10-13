# Functions to prep CHP data

prepCHPmicrodata <- function(chp_waves = 22:29,
                             overwrite_chp = FALSE) {

  # read in data and stack
  hpvars <- c(
    "SCRAM", "WEEK", "EST_ST", "RECVDVACC", "DOSES", "GETVACC", "GETVACRV", "PWEIGHT",
    "TBIRTH_YEAR", "RRACE", "RHISPANIC", "EEDUC"
  )

  hpdata <- map_dfr(chp_waves,
                    function(w) {
                      get_dataframe_by_name(
                        glue("pulse2021_puf_{w}.tab"),
                        dataset = dvdoi,
                        original = TRUE,
                        .f = function(x) fread(x, select = hpvars)
                      )
                    }
  )

  # translate EST_ST to state name
  estcode_to_state <- fread("data/census-household-pulse/estcode_to_state.csv")
  hpdata <- left_join(hpdata, estcode_to_state, by = "EST_ST")

  # calculate age
  hpdata <- hpdata %>% mutate(age = 2021 - TBIRTH_YEAR)

  # get start and end dates for each wave
  chp_wave_dates <- fread("data/census-household-pulse/chp_wave_dates.csv")
  hpdata <- left_join(hpdata, chp_wave_dates, by = c("WEEK" = "wave_num")) %>%
    mutate(
      start_date = as.Date(as.character(start_date), format = "%Y%m%d"),
      end_date = as.Date(as.character(end_date), format = "%Y%m%d")
    )

  # get state abbrvs
  stdata <- tibble(state_name = c(state.name, "District of Columbia"),
                   state = c(state.abb, "DC"))
  hpdata <- left_join(hpdata, stdata, by = "state_name")

  # create some indicators
  hpdata <- hpdata %>%
    mutate(
      vaccinated = as.numeric(RECVDVACC == 1),
      willing_def = as.numeric(GETVACC == 1 | GETVACRV == 1),
      willing_prob = as.numeric(GETVACC == 2 | GETVACRV == 2),
      # question code switched from GETVACC to GETVACRV on wave 28 when they added 3 = unsure
      hesitant_prob = as.numeric(GETVACC == 3 | GETVACRV == 4),
      hesitant_def = as.numeric(GETVACC == 4 | GETVACRV == 5),
      # refusals and explicit unsures coded as unsure
      unsure = as.numeric(RECVDVACC != 1 & (GETVACRV == 3 | GETVACC < 0 | GETVACRV < 0))
    ) %>%
    mutate_at(vars(vaccinated:unsure), ~ replace(., is.na(.), 0))

  # write cleaned microdata to file
  write_csv(hpdata,
            file = path(
              "data/final",
              glue("chp_cleaned_waves{min(hpdata$WEEK)}to{max(hpdata$WEEK)}.csv.gz")
            )
  )

  return(hpdata)
}


#' Download rep weights
writeHPrepweights <- function(chp_waves) {
  # combine replicate weights and write to file
  hpdata_repwt <- map_dfr(chp_waves,
                          function(w) {
                            get_dataframe_by_name(
                              glue("pulse2021_repwgt_puf_{w}.{case_when(w %in% c(22, 28) ~ 'tab', w %in% c(23:27, 29) ~ 'csv')}"),
                              dataset = dvdoi,
                              original = TRUE,
                              .f = fread
                            )
                          }
  )

  write_csv(hpdata_repwt,
            file = path("data/census-household-pulse/microdata/",
                        glue("chp_rpwgts_cleaned_waves{min(hpdata_repwt$WEEK)}to{max(hpdata_repwt$WEEK)}_repwts.csv.gz")
            )
  )
}

getHPTabColnames <- function(cname, which = "est") {
  cname_new <- case_when(
    cname == "Select characteristics" ~ "subgroup",
    cname == "Total" ~ "pop_total",
    cname == "Received a COVID-19 vaccine_Yes_Total" ~ "n_vaccinated",
    cname == "Received a COVID-19 vaccine_Yes_Received or plan to receive all required doses" ~ "n_vaccinated_willgetalldoses",
    cname == "Received a COVID-19 vaccine_Yes_Have not received/do not plan to receive all required doses" ~ "n_vaccinated_wontgettalldoses",
    cname == "Received a COVID-19 vaccine_Yes_Did not report" ~ "n_vaccinated_DNKdoses",
    cname == "Received a COVID-19 vaccine_No_Total" ~ "n_not_vaccinated",
    cname == "Received a COVID-19 vaccine_No_Will definitely get a vaccine" ~ "n_willing_definitely",
    cname == "Received a COVID-19 vaccine_No_Will probably get a vaccine" ~ "n_willing_probably",
    cname == "Received a COVID-19 vaccine_No_Unsure about getting a vaccine" ~ "n_vaxunsure",
    cname == "Received a COVID-19 vaccine_No_Will probably not get a vaccine" ~ "n_hesitant_probably",
    cname == "Received a COVID-19 vaccine_No_Will definitely not get a vaccine" ~ "n_hesitant_definitely",
    cname == "Received a COVID-19 vaccine_No_Did not report" ~ "n_hesitant_DK",
    cname == "Received a COVID-19 vaccine_Did not report" ~ "n_vaccinated_DK",
    TRUE ~ cname
  )

  if (which == "SE") {
    cname_new <- paste0(cname_new, ifelse(grepl("n_", cname_new) | cname_new == "pop_total", "_SE", ""))
  }

  return(cname_new)
}




# toplines
# filename <- paste0("health5_week", w, ".xlsx")
# paste0("https://www2.census.gov/programs-surveys/demo/tables/hhp/2021/wk", w, "/", filename)

#' Prep CHP Tables
prepCHPtables <- function(chp_waves = 22:29,
                          overwrite_chp = FALSE) {
  ######## extract vax data from correct sheets and stack
  # for all tables

  census_tables <- glue("health5_week{chp_waves}.xlsx")
  all_tbl <- map_dfr(census_tables, function(t) read_CHP_health_tbls(t))

  # bind together
  all_tables    <- filter(all_tbl, type == "est") %>%
    rename_with(function(x) getHPTabColnames(x, which = "est"))

  all_tables_se <- filter(all_tbl, type == "se") %>%
    rename_with(function(x)getHPTabColnames(x, which = "SE"))

  # merge
  all_tables_full <- full_join(all_tables, all_tables_se,
                               by = c("demo", "subgroup", "pop", "wave")) %>%
    relocate(pop, wave, .before = demo) %>%
    select(-matches("type\\.(x|y)")) %>%
    mutate_at(vars(pop_total:n_vaxunsure_SE), as.numeric)


  # get start and end dates for each wave
  chp_wave_dates <- fread("data/census-household-pulse/chp_wave_dates.csv")

  all_tables_full <- left_join(all_tables_full, chp_wave_dates,
                               by = c("wave" = "wave_num")
  ) %>%
    mutate(
      start_date = as.Date(as.character(start_date), format = "%Y%m%d"),
      end_date = as.Date(as.character(end_date), format = "%Y%m%d")
    )


  # calc percentages and clean
  all_tables_full <- all_tables_full %>% mutate(
    wave = as.numeric(wave),

    # calc percentages
    pct_vaccinated = n_vaccinated / pop_total,
    pct_willing_definitely = n_willing_definitely / pop_total,
    pct_willing_probably = n_willing_probably / pop_total,
    pct_hesitant_definitely = n_hesitant_definitely / pop_total,
    pct_hesitant_probably = n_hesitant_probably / pop_total,
    pct_vaxunsure = n_vaxunsure / pop_total,

    # option not offered in earlier waves, so replace NAs with 0
    pct_vaxunsure = ifelse(is.na(pct_vaxunsure), 0, pct_vaxunsure),
    n_vaxunsure_SE = ifelse(is.na(n_vaxunsure_SE), 0, n_vaxunsure_SE)

    # calc net willing / hesitant
    , pct_willing = pct_willing_definitely + pct_willing_probably,
    pct_hesitant = pct_hesitant_definitely + pct_hesitant_probably + pct_vaxunsure,
    pct_haveorwillgetvax = pct_willing + pct_vaccinated # unsure not uncluded in have or will get vax
  )

  # write for temporarry saving
  write_csv(all_tables_full,
            file = file.path("data", "census-household-pulse", "tables",
                             paste0("chp_tables_cleaned_waves",
                                    min(chp_waves), "to", max(chp_waves), ".csv.gz")
            )
  )

  return(all_tables_full)
}



#' Combine micro and table
prepCHPcombined <- function(chp_waves = 22:29, overwrite_chp = FALSE) {

  # prep microdata and tables
  chp_microdata <- prepCHPmicrodata(chp_waves = chp_waves, overwrite_chp = overwrite_chp)
  chp_tables <- prepCHPtables(chp_waves = chp_waves, overwrite_chp = overwrite_chp)


  # aggregate microdata
  chp_microdata_agg <- bind_rows(
    chp_microdata %>% mutate(pop = "US"),
    chp_microdata %>% mutate(pop = state)
  ) %>%
    group_by(pop, wave = WEEK, start_date, end_date) %>%
    summarize(
      n = n(),
      n_weighted = sum(PWEIGHT),
      weight_mean = mean(PWEIGHT),
      weight_var = var(PWEIGHT),
      CV = sqrt(var(PWEIGHT / mean(PWEIGHT))),
      deff = 1 + CV^2,
      pct_vaccinated_raw = mean(vaccinated),
      pct_vaccinated = sum(vaccinated * PWEIGHT) / sum(PWEIGHT),
      pct_willing_definitely_raw = mean(willing_def),
      pct_willing_probably_raw = mean(willing_prob),
      pct_hesitant_probably_raw = mean(hesitant_prob),
      pct_hesitant_definitely_raw = mean(hesitant_def),
      pct_vaxunsure_raw = mean(unsure),
      pct_willing_raw = pct_willing_definitely_raw + pct_willing_probably_raw,
      pct_hesitant_raw = pct_hesitant_definitely_raw + pct_hesitant_probably_raw,
      pct_willing_definitely = sum(willing_def * PWEIGHT) / sum(PWEIGHT),
      pct_willing_probably = sum(willing_prob * PWEIGHT) / sum(PWEIGHT),
      pct_hesitant_probably = sum(hesitant_prob * PWEIGHT) / sum(PWEIGHT),
      pct_hesitant_definitely = sum(hesitant_def * PWEIGHT) / sum(PWEIGHT),
      pct_vaxunsure = sum(unsure * PWEIGHT) / sum(PWEIGHT),
      pct_willing = pct_willing_definitely + pct_willing_probably,
      pct_hesitant = pct_hesitant_definitely + pct_hesitant_probably + pct_vaxunsure,
      pct_haveorwillgetvax = pct_vaccinated + pct_willing,

      # calc standard errors
      pct_vaccinated_raw_se = sqrt(pct_vaccinated_raw * (1 - pct_vaccinated_raw) / n),
      pct_vaccinated_se = sqrt(pct_vaccinated * (1 - pct_vaccinated) * deff / n),
      pct_hesitant_se = sqrt(pct_hesitant * (1 - pct_hesitant) * deff / n),
      pct_willing_se = sqrt(pct_willing * (1 - pct_willing) * deff / n),
      .groups = "drop"
    )

  # check all rows are there
  chp_microdata_agg %>% group_by(pop) %>% summarize(n(), sum(n)) %>% print(n=100)

  chp_microdata_agg <- chp_microdata_agg %>%
    # add these columns for merging with data from tables
    mutate(
      source = "microdata",
      demo = "Total",
      subgroup = "Total",
      mode = "household_pulse"
    )

  # check that we're not miscategorizing
  # chp_microdata_agg %>%
  #   summarize(sum = pct_vaccinated + pct_willing_definitely + pct_willing_probably + pct_hesitant_probably + pct_hesitant_definitely + pct_vaxunsure) %>%
  #   group_by(sum)%>% count()
  # chp_microdata_agg %>%
  #   summarize(sum = pct_vaccinated_raw + pct_willing_definitely_raw + pct_willing_probably_raw + pct_hesitant_probably_raw + pct_hesitant_definitely_raw + pct_vaxunsure_raw) %>%
  #   group_by(sum)%>% count()

  #### prep tables -- grab n, CV, and deff from microdata (they're not included)
  chp_tables <- chp_tables %>%
    mutate(source = "tables", mode = "household_pulse") %>%
    left_join(., chp_microdata_agg %>% select(pop, wave, subgroup, n, CV, deff), by = c("pop", "wave", "subgroup")) %>%
    mutate(
      pct_vaccinated_se = n_vaccinated_SE / pop_total,
      pct_hesitant_se = sqrt(n_hesitant_probably_SE^2 + n_hesitant_definitely_SE^2 + n_vaxunsure_SE^2) / pop_total,
      pct_willing_se = sqrt(n_willing_probably_SE^2 + n_willing_definitely_SE^2) / pop_total,
      MoE = 2 * sqrt(deff) * pct_vaccinated_se
    ) %>%
    filter(subgroup == "Total")


  # compare tables and microdata ests -- check that numbers match to 3 decimals
  chp_comp <- full_join(
    chp_tables %>%
      select(wave, pop, subgroup, demo,
             pct_vaccinated_tab = pct_vaccinated,
             pct_willing_tab = pct_willing,
             pct_hesitant_tab = pct_hesitant,
             pct_vaccinated_SE_tab = pct_vaccinated_se,
             pct_willing_SE_tab = pct_willing_se,
             pct_hesitant_SE_tab = pct_hesitant_se
      ),
    chp_microdata_agg %>%
      select(wave, pop, subgroup, demo,
             pct_vaccinated_micro = pct_vaccinated,
             pct_willing_micro = pct_willing,
             pct_hesitant_micro = pct_hesitant,
             pct_vaccinated_SE_micro = pct_vaccinated_se,
             pct_willing_SE_micro = pct_willing_se,
             pct_hesitant_SE_micro = pct_hesitant_se
      ),
    by = c("wave", "pop", "subgroup", "demo")
  )

  chp_comp %>%
    mutate(
      pct_vaccinated_flag = trunc(pct_vaccinated_tab * 1000) == trunc(pct_vaccinated_micro * 1000),
      pct_willing_flag = trunc(pct_willing_tab * 1000) == trunc(pct_willing_micro * 1000),
      pct_hesitant_flag = trunc(pct_hesitant_tab * 1000) == trunc(pct_hesitant_micro * 1000),
      pct_vaccinated_SE_flag = as.numeric(trunc(pct_vaccinated_SE_tab * 1000) == trunc(pct_vaccinated_SE_micro * 1000)),
      pct_willing_SE_flag = as.numeric(trunc(pct_willing_SE_tab * 1000) == trunc(pct_willing_SE_micro * 1000)),
      pct_hesitant_SE_flag = as.numeric(trunc(pct_hesitant_SE_tab * 1000) == trunc(pct_hesitant_SE_micro * 1000))
    ) %>%
    summarize(
      mean(pct_vaccinated_flag, na.rm = TRUE),
      mean(pct_willing_flag, na.rm = TRUE),
      mean(pct_hesitant_flag, na.rm = TRUE),
      mean(pct_vaccinated_SE_flag, na.rm = TRUE),
      mean(pct_willing_SE_flag, na.rm = TRUE),
      mean(pct_hesitant_SE_flag, na.rm = TRUE)
    )


  # write out cleaned, final CHP data
  write.csv(chp_tables, glue("data/final/chp_cleaned_waves{min(chp_waves)}to{max(chp_waves)}.csv"))
}




#' Read entire tables
read_CHP_health_tbls <- function(t) {

  # get wave name
  wave_num <- as.numeric(gsub("health5_week|[.]xlsx", "", t))

  # download
  tmp <-  get_file_by_name(
    filename = str_replace(t, "xlsx$", "tab"),
    dataset = dvdoi,
    original = TRUE)

  dest_dir <- "data/census-household-pulse/tables"
  writeBin(tmp, path(dest_dir, t))

  t_se <- str_replace(t, "health5", "health5_se")
  tmp <-  get_file_by_name(
    filename = str_replace(t_se, "xlsx$", "tab"),
    dataset = dvdoi,
    original = TRUE)
  writeBin(tmp, path(dest_dir, t_se))

  # iterate through sheets
  sheets <- c("US", state.abb, "DC")
  tbl_est <- map_dfr(sheets, function(s) read_CHP_health(sh = s, tgt_tbl = path(dest_dir, t), wv = wave_num))
  tbl_se  <- map_dfr(sheets, function(s) read_CHP_health(sh = s, tgt_tbl = path(dest_dir, t_se), wv = wave_num))

  bind_rows(
    tbl_est %>% mutate(type = "est"),
    tbl_se %>% mutate(type = "se")
  ) %>%
    relocate(type)
}

#' Read sheets in a table
read_CHP_health <- function(sh, tgt_tbl, wv) {
  tmp <- suppressMessages(read_excel(tgt_tbl, sheet = sh, skip = 3, na = ""))

  # get colnames -- same in each file, pull from estimates
  colnames <- as.vector(apply(tmp, 2, function(c) {
    c <- c[1:3] # make col names from first 3 rows
    c <- c[!(is.na(c) | c == "NA")] # drop NAs
    paste(c, collapse = "_")
  }))

  # setcolnames and drop extra rows
  setnames(tmp, colnames)

  # do remaining cleaning
  out <- tmp %>%
    slice(5:n()) %>%
    mutate(demo = ifelse(is.na(Total), `Select characteristics`, NA),
           .before = `Select characteristics`) %>%
    fill(demo) %>%
    mutate(demo = ifelse(is.na(demo), "Total", demo),
           pop = sh,
           wave = wv) %>%
    filter(!(is.na(Total) | grepl("^[*]", `Select characteristics`)))

  return(out)
}