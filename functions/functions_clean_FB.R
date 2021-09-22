
getFBdata = function(download_fb = T, overwrite_fb = F, fb_data_dir){
  base_url = 'https://www.cmu.edu/delphi-web/surveys/weekly/'

  if(!dir.exists(fb_data_dir)){
    dir.create(fb_data_dir)
  }

  # get list of files
  h = new_handle(dirlistonly=TRUE)
  con = curl(base_url, "r", h)
  tbl = read.table(con, stringsAsFactors=F, fill=TRUE)
  close(con)

  # parse
  all_files = unlist(lapply(tbl$V5, function(a){ unlist(strsplit(a, '"'))[2]}))
  all_files = unique(all_files)
  natl_files = all_files[grepl('nation_overall.csv', all_files)]
  state_files = all_files[grepl('state_overall.csv', all_files)]

  if(download_fb){
    # download
    for(f in c(natl_files, state_files)){
      url = paste0(base_url,f)
      if(!file.exists(file.path(fb_data_dir, f)) | overwrite_fb){
        download.file(url = url, destfile = file.path(fb_data_dir, f))
      }
    }
  }
  return(list(natl_files = natl_files, state_files = state_files))
}


cleanFBdata = function(download_fb = T, overwrite_fb = F){

  #download data
  fb_data_dir = file.path('data', 'raw', 'facebook-delphi')
  file_list = getFBdata(download_fb = download_fb, overwrite_fb = overwrite_fb, fb_data_dir = fb_data_dir)

  # combine select variables from all waves
  fb_vars = c( 'region', 'survey_geo'
               , 'val_pct_vaccinated', 'se_pct_vaccinated', 'sample_size_pct_vaccinated', 'represented_pct_vaccinated'
               #, 'val_pct_received_2_vaccine_doses', 'se_pct_received_2_vaccine_doses', 'sample_size_pct_received_2_vaccine_doses', 'represented_pct_received_2_vaccine_doses'
               , 'val_pct_accept_vaccine', 'se_pct_accept_vaccine', 'sample_size_pct_accept_vaccine', 'represented_pct_accept_vaccine'
               , 'val_pct_vaccinated_or_accept', 'se_pct_vaccinated_or_accept', 'sample_pct_vaccinated_or_accept', 'represented_pct_vaccinated_or_accept'
               , 'val_pct_hesitant_vaccine', 'se_pct_hesitant_vaccine', 'sample_size_pct_hesitant_vaccine', 'represented_pct_hesitant_vaccine'
               #, 'val_pct_accept_vaccine_defyes', 'se_pct_accept_vaccine_defyes', 'sample_size_pct_accept_vaccine_defyes', 'represented_pct_accept_vaccine_defyes'
               #, 'val_pct_accept_vaccine_probyes', 'se_pct_accept_vaccine_probyes', 'sample_size_pct_accept_vaccine_probyes', 'represented_pct_accept_vaccine_probyes'
               #, 'val_pct_accept_vaccine_probno', 'se_pct_accept_vaccine_probno', 'sample_size_pct_accept_vaccine_probno', 'represented_pct_accept_vaccine_probno'
               #, 'val_pct_accept_vaccine_defno', 'se_pct_accept_vaccine_defno', 'sample_size_pct_accept_vaccine_defno', 'represented_pct_accept_vaccine_defno'
  )
  fbdata = rbindlist(lapply(c(file_list$natl_files, file_list$state_files), function(f){
    wave = gsub('_weekly_nation_overall.csv|_weekly_state_overall.csv', '', f)
    temp = fread(file.path(fb_data_dir, f), select = fb_vars)
    temp$wave = wave
    temp
  }), fill = T)

  # basic cleaning
  fbdata <- fbdata %>%
    mutate(mode = 'facebook'
           , source = 'fb_api'
           , pop = toupper(region)
           , pop = ifelse(pop == 'OVERALL', 'US', pop)
           ) %>%
    separate(wave, into = c('start_date', 'end_date'), sep = '_') %>%
    mutate(start_date = as.Date(start_date, format = '%Y%m%d')
           , end_date = as.Date(end_date, format = '%Y%m%d')
           , n_days_wave = as.numeric(difftime(end_date, start_date, units = 'days')) + 1
           , epiweek = lubridate::epiweek(end_date)
           , epiyear = lubridate::epiyear(end_date)

           , n = sample_size_pct_vaccinated  #use full sample size - vaccination questions don't suffer from as much item nonresponse
           , pct_vaccinated = val_pct_vaccinated / 100
           , pct_haveorwillgetvax = (as.numeric(val_pct_vaccinated_or_accept) / 100)
           , pct_hesitant = 1 - pct_haveorwillgetvax
           , pct_willing = pct_haveorwillgetvax - pct_vaccinated

           , pct_vaccinated_se = sqrt(pct_vaccinated * (1 - pct_vaccinated) / n)
           , pct_hesitant_se = sqrt(pct_hesitant * (1 - pct_hesitant) / n)
           , pct_willing_se = sqrt(pct_willing * (1 - pct_willing) / n)
           ) %>%
    select(start_date:pct_willing_se)


  #load deffs from CMU
  fb_deff = fread(file.path(fb_data_dir, 'deff.csv')) %>%
    mutate(pop = 'US') %>%
    filter(epiyear == 2021)

  fb_deff_mean <- fb_deff %>% summarize(mean_deff = mean(deff)) %>% pull(mean_deff)

  fbdata <- left_join(fbdata
                      , fb_deff %>% select(-n, -week)
                      , by = c('epiweek', 'epiyear', 'pop')) %>%
    mutate(deff = ifelse(pop == 'US' & is.na(deff), fb_deff_mean, deff))



  write.csv(fbdata, file.path('data', 'final', 'fb_cleaned.csv'))


}
