

source("_setup.R")

# source helper functions for cleaning data
source("functions/functions_clean_CDC.R")
source("functions/functions_clean_CHP.R")
source("functions/functions_clean_FB.R")
source("functions/functions_clean_others.R")
source("functions/functions_calc_ddc.R")


######### PREP BENCHMARK DATA #########
### Loop through all dates with benchmark data in the repo
benchmark_dates <- c("2021-04-21", "2021-05-05", "2021-05-26")

for (b in benchmark_dates) {
  # have to download CDC locally and specify paths
  cdc_path <- file.path(
    "data", "raw", "CDC",
    paste0("trends_in_number_of_covid19_vaccinations_in_the_us_", b, ".csv")
  )
  cdc_age_path <- file.path(
    "data", "raw", "CDC",
    paste0("demographic_trends_of_people_receiving_covid19_vaccinations_in_the_united_states_", b, ".csv")
  )

  getBenchmark(b, cdc_path = cdc_path, cdc_age_path = cdc_age_path)
}

######### PULL IN BENCHMARK DATA #########
# pull in the one we're going to use
which_benchmark <- "2021-05-26"
benchmark <- fread(file.path("data", "final", glue("benchmark_{which_benchmark}.csv")))


#### RUN CLEAN CHP DATA #####
chp_waves <- 22:29
prepCHPcombined(chp_waves = chp_waves)


#### RUN CLEAN FACEBOOK ######
cleanFBdata()


#### RUN CLEAN IPSOS-AXIOS ######
cleanIPSOSdata()


######### PULL IN POLL DATA #########
poll_vars <- c(
  "mode", "wave", "pop", "start_date", "end_date", "n",
  "pct_haveorwillgetvax", "pct_vaccinated", "pct_hesitant", "pct_willing",
  "pct_vaccinated_se", "pct_hesitant_se", "pct_willing_se",
  "MoE", "deff", "source"
)

chp_paths <- path("data", "final", glue("chp_cleaned_waves{min(chp_waves)}to{max(chp_waves)}.csv"))
fb_paths <- path("data", "final", "fb_cleaned.csv")
ip_paths <- path("data", "final", "axios_ipsos_cleaned.csv")

chpdata <- map_dfr(chp_paths, ~ fread(.x, select = poll_vars))
fbdata  <- map_dfr(fb_paths, ~ fread(.x, select = poll_vars))
ipsos   <- map_dfr(ip_paths, ~ fread(.x, select = poll_vars))


# stack polls
all_polls_fmt <- list(`Delphi-Facebook` = fbdata,
                  `Census Household Pulse` = chpdata,
                  `Axios-Ipsos` = ipsos) %>%
  bind_rows(.id = "study_name") %>%
  rename(source_poll = source)

bench_fmt <-  benchmark %>%
  # drop national-level OWID data
  filter(!(state == "US" & source == "OWID")) %>%
  select(-n_pop_vaccinated_imputedflag)

# merge in benchmark
all_polls <- left_join(
  all_polls_fmt,
  bench_fmt,
  by = c("end_date" = "date", "pop" = "state")
)



######### ADD IN BENCHMARK ERROR #########
all_polls_witherror <- addBenchmarkError(all_polls,
  error_levels = c(-0.1, -0.05, 0.05, 0.1),
  include_0 = TRUE
)


######### CALCULATE DDC #########
all_polls_out <- all_polls_witherror %>%
  mutate(
    # calculate pieces we need for ddc
    f = n / pop_total,
    error = pct_vaccinated - pct_pop_vaccinated,
    sd_G = sqrt(pct_pop_vaccinated * (1 - pct_pop_vaccinated)),

    # get ddc and ddi
    ddc = error / (sqrt((pop_total - n) / n) * sd_G),
    ddi = ddc^2,

    # calculate standard errors, MoEs and CIs
    se_samp = sqrt(pct_vaccinated * (1 - pct_vaccinated) / n),
    MoE_samp = 2 * se_samp,
    ci_2.5_samp = pct_vaccinated - MoE_samp,
    ci_97.5_samp = pct_vaccinated + MoE_samp,

    # impute deff and MoE where missing
    deff = ifelse(is.na(deff), 1, deff),
    MoE = ifelse(is.na(MoE), MoE_samp * sqrt(deff), MoE),

    # calculate CIs using MoEs that include variance from weighting
    ci_2.5 = pct_vaccinated - MoE,
    ci_97.5 = pct_vaccinated + MoE,

    # calculate weighted ddc
    n_w = n / deff,
    ddc_weighted = error / (sqrt((pop_total - n_w) / n_w) * sd_G),
    ddi_weighted = ddc_weighted^2,

    # calculate classic effective sample size from weighting
    n_eff = n / deff,
    f_eff = n_eff / pop_total,

    # calculate drop out odds, weighted and unweighted
    DO = (pop_total - n) / n,
    DO_sqrt = sqrt(DO),
    DO_weighted = (pop_total - n_w) / n_w,
    DO_weighted_sqrt = sqrt(DO),

    # calculate bias-adjusted effective sample size
    n_eff_star = (sd_G / error)^2,
    n_eff_star_cap = ifelse(n_eff_star > n, n_eff, n_eff_star),

    # calculate percent reduction in effective sample size
    pct_reduction_n_eff = 1 - (n_eff_star / n_eff),
    pct_reduction_n_eff_cap = 1 - (n_eff_star_cap / n_eff)
  )


write_csv(all_polls_out, path("data", "final", "all_polls_all_vars.csv.gz"))
