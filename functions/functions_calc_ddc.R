
addBenchmarkError = function(data, error_levels = c(-0.1,-0.05, 0.05, 0.1), include_0 = T){
  if(include_0){
    error_levels = c(0, error_levels)
  }

  data <- rbindlist(lapply(error_levels, function(e){
    data %>% mutate(pct_error = e
                    , pct_error_lab = ifelse(e == 0, 'no error', paste0(ifelse(e < 0, 'less', 'plus'), abs(e) * 100, 'pct'))
                    )
  }))

  data %>%
    mutate(pct_pop_vaccinated = pct_pop_vaccinated * (1 + pct_error))
}
