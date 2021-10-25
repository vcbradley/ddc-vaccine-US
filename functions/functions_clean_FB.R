#' Clean from stacked source
cleanFBdata <- function(datapath = "data/facebook/weekly_overall.csv") {

  # basic cleaning
  fbdata <- read_csv(datapath) %>%
    mutate(
      mode = "facebook",
      source = "fb_api",
      pop = toupper(region),
      pop = ifelse(pop == "OVERALL", "US", pop)
    ) %>%
    separate(wave, into = c("start_date", "end_date"), sep = "_") %>%
    mutate(
      start_date = as.Date(start_date, format = "%Y%m%d"),
      end_date = as.Date(end_date, format = "%Y%m%d"),
      n_days_wave = as.numeric(difftime(end_date, start_date, units = "days")) + 1,
      epiweek = lubridate::epiweek(end_date),
      epiyear = lubridate::epiyear(end_date),
      n = sample_size_pct_vaccinated # use full sample size - vaccination questions don't suffer from as much item nonresponse
      , pct_vaccinated = val_pct_vaccinated / 100,
      pct_haveorwillgetvax = (as.numeric(val_pct_vaccinated_or_accept) / 100),
      pct_hesitant = 1 - pct_haveorwillgetvax,
      pct_willing = pct_haveorwillgetvax - pct_vaccinated,
      pct_vaccinated_se = sqrt(pct_vaccinated * (1 - pct_vaccinated) / n),
      pct_hesitant_se = sqrt(pct_hesitant * (1 - pct_hesitant) / n),
      pct_willing_se = sqrt(pct_willing * (1 - pct_willing) / n)
    ) %>%
    select(start_date:pct_willing_se)


  # load deffs from CMU
  fb_deff <- get_dataframe_by_name("deff.tab", dvdoi, original = TRUE, .f = read_csv) %>%
    mutate(pop = "US") %>%
    filter(epiyear == 2021)

  fb_deff_mean <- fb_deff %>%
    summarize(mean_deff = mean(deff)) %>%
    pull(mean_deff)

  fbdata <- left_join(fbdata,
    fb_deff %>% select(-n, -week),
    by = c("epiweek", "epiyear", "pop")
  ) %>%
    mutate(deff = ifelse(pop == "US" & is.na(deff), fb_deff_mean, deff))


  write.csv(fbdata, file.path("data", "final", "fb_cleaned.csv"))
}


#' All fb paths
#'
#' Data on dataverse
FB_dates <- c("20210103_20210109", "20210110_20210116", "20210117_20210123",
              "20210124_20210130", "20210131_20210206", "20210207_20210213",
              "20210214_20210220", "20210221_20210227", "20210228_20210306",
              "20210307_20210313", "20210314_20210320", "20210321_20210327",
              "20210328_20210403", "20210404_20210410", "20210411_20210417",
              "20210418_20210424", "20210425_20210501", "20210502_20210508",
              "20210509_20210515", "20210516_20210522", "20210523_20210529",
              "20210530_20210605", "20210606_20210612", "20210613_20210619",
              "20210620_20210626", "20210627_20210703", "20210704_20210710",
              "20210711_20210717", "20210718_20210724", "20210725_20210731",
              "20210801_20210807", "20210808_20210814", "20210815_20210821",
              "20210822_20210828", "20210829_20210904", "20210905_20210911",
              "20210912_20210918", "20210919_20210925")

FB_files <- c(str_c(FB_dates, "_weekly_state_overall.csv"),
              str_c(FB_dates, "_weekly_nation_overall.csv"))




#' Get individual files (either locally or from dataverse), then stack
#'
getFBdata_from_raw <- function(paths = FB_files) {
  fb_vars <- c(
    "region", "survey_geo",
    "val_pct_vaccinated", "se_pct_vaccinated", "sample_size_pct_vaccinated", "represented_pct_vaccinated",
    "val_pct_accept_vaccine", "se_pct_accept_vaccine", "sample_size_pct_accept_vaccine", "represented_pct_accept_vaccine",
    "val_pct_vaccinated_or_accept", "se_pct_vaccinated_or_accept", "sample_pct_vaccinated_or_accept", "represented_pct_vaccinated_or_accept",
    "val_pct_hesitant_vaccine", "se_pct_hesitant_vaccine", "sample_size_pct_hesitant_vaccine", "represented_pct_hesitant_vaccine"
    # , 'val_pct_received_2_vaccine_doses', 'se_pct_received_2_vaccine_doses', 'sample_size_pct_received_2_vaccine_doses', 'represented_pct_received_2_vaccine_doses'
    # , 'val_pct_accept_vaccine_defyes', 'se_pct_accept_vaccine_defyes', 'sample_size_pct_accept_vaccine_defyes', 'represented_pct_accept_vaccine_defyes'
    # , 'val_pct_accept_vaccine_probyes', 'se_pct_accept_vaccine_probyes', 'sample_size_pct_accept_vaccine_probyes', 'represented_pct_accept_vaccine_probyes'
    # , 'val_pct_accept_vaccine_probno', 'se_pct_accept_vaccine_probno', 'sample_size_pct_accept_vaccine_probno', 'represented_pct_accept_vaccine_probno'
    # , 'val_pct_accept_vaccine_defno', 'se_pct_accept_vaccine_defno', 'sample_size_pct_accept_vaccine_defno', 'represented_pct_accept_vaccine_defno'
  )

  # download from Dataverse  https://doi.org/10.7910/DVN/GKBUUK if recreating

  fbdata <- rbindlist(
    lapply(
      FB_files,
      FUN = function(f) {
        wave <- gsub("_weekly_nation_overall.csv|_weekly_state_overall.csv", "", f)
        temp <- fread(file.path(fb_data_dir, f), select = fb_vars)
        temp$wave <- wave
        temp
      }),
    fill = TRUE)


  write_csv(fbdata, "data/facebook/weekly_overall.csv")
}


#' If trying to download from API, use this
#' e.g. `getFBdataAPI("data/raw/facebook-delphi")`
getFBdataAPI <- function(download_fb = TRUE, overwrite_fb = FALSE, fb_data_dir) {
  base_url <- "https://www.cmu.edu/delphi-web/surveys/weekly/"

  if (!dir.exists(fb_data_dir)) {
    dir.create(fb_data_dir)
  }

  # get list of files
  h <- new_handle(dirlistonly = TRUE)
  con <- curl(base_url, "r", h)
  tbl <- read.table(con, stringsAsFactors = FALSE, fill = TRUE)
  close(con)

  # parse
  all_files <- unlist(lapply(tbl$V5, function(a) {
    unlist(strsplit(a, '"'))[2]
  }))
  all_files <- unique(all_files)
  natl_files <- all_files[grepl("nation_overall.csv", all_files)]
  state_files <- all_files[grepl("state_overall.csv", all_files)]

  if (download_fb) {
    # download
    for (f in c(natl_files, state_files)) {
      url <- paste0(base_url, f)
      if (!file.exists(file.path(fb_data_dir, f)) | overwrite_fb) {
        download.file(url = url, destfile = file.path(fb_data_dir, f))
      }
    }
  }
  return(list(natl_files = natl_files, state_files = state_files))
}
