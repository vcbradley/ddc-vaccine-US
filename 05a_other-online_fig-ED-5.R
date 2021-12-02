library(tidyverse)
library(fs)
library(dataverse)
library(haven)


# file_copy("../ddi-covid-studies/data/axios_ipsos/axios-ipsos_stacked.rds",
#           "data/axios-ipsos", overwrite = TRUE)
# file_copy(
#   "../ddi-covid-studies/data/CDC_benchmark/combined_benchmark.csv",
#   "data/CDC",
#   overwrite = TRUE
# )

# CDC
bench_df <- read_csv("data/CDC/combined_benchmark.csv") %>%
  filter(state == "US", source == "CDC_historical")

# raw data ----
ip_df <- read_rds("data/axios-ipsos/axios-ipsos_stacked.rds")

dfp_raw <- get_dataframe_by_name(
  filename = "dfp_covid_tracking_poll.tab",
  dataset = "10.7910/DVN/XJLZIN",
  .f = read_dta,
  original = TRUE,
  server = "dataverse.harvard.edu"
)

# format raw data
dfp_fmt <- dfp_raw %>%
  transmute(
    wave,
    date = lubridate::date(starttime),
    rid,
    nationalweight,
    vaccinated = as.numeric(vax == 1)
  )


# get estimates ------
## Ipsos ----
df_IP <- ip_df %>%
  group_by(date) %>%
  summarize(
    wave = as.numeric(unique(WAVE)),
    vax_w = weighted.mean(vaccinated, wt_final, na.rm = TRUE),
    vax_raw = mean(vaccinated, na.rm = TRUE),
    n_raw = sum(!is.na(vaccinated)),
    n_w = sum(wt_final)^2 / sum(wt_final^2)
  )


## DFP ----
df_DFP <- dfp_fmt %>%
  group_by(wave) %>%
  summarize(
    date_end = last(date),
    date = first(date),
    vax_w = weighted.mean(vaccinated, nationalweight, na.rm = TRUE),
    vax_raw = mean(vaccinated, na.rm = TRUE),
    n_raw = sum(!is.na(vaccinated)),
    n_w = sum(nationalweight)^2 / sum(nationalweight^2)
  ) %>%
  filter(n_raw > 0)

# check dates
# dfp_fmt %>%
# 　semi_join(distinct(df_DFP, wave)) %>%
#   count(wave, date) %>%
#   ggplot(aes(x = date, fill = factor(wave))) +
#   geom_col(aes(y = n))

## Morning Consult ----
# https://morningconsult.com/covid19-vaccine-dashboard/
df_MC <- tribble(
  ~pollster, ~date_start,  ~vax_w,
  "MC", "2021-03-15", 0.26,
  "MC", "2021-03-22", 0.29,
  "MC", "2021-03-29", 0.35,
  "MC", "2021-04-05", 0.38,
  "MC", "2021-04-12", 0.43,
  "MC", "2021-04-19", 0.47,
  "MC", "2021-04-26", 0.49,
  "MC", "2021-05-03", 0.52,
  "MC", "2021-05-10", 0.54,
  "MC", "2021-05-17", 0.55
) %>%
  mutate(wave = 1:n()) # fake wave

# Harris
df_HR <- tribble(
  ~wave, ~date, ~vax_w, ~n_w,
  48, "2021-01-25", 0.08, 1956,
  49, "2021-01-31", 0.10, 2025,
  50, "2021-02-07", 0.12, 2043,
  51, "2021-02-14", 0.14, 1984,
  52, "2021-02-21", 0.14, 1961,
  53, "2021-02-28", 0.21, 2000,
  54, "2021-03-07", 0.21, 1963,
  55, "2021-03-14", 0.28, 1977,
  56, "2021-03-21", 0.30, 1948, # A
  57, "2021-03-28", 0.32, 1989,
  58, "2021-04-04", 0.36, 1943,
  59, "2021-04-11", 0.37, 1963,
  60, "2021-04-18", 0.42, 2024,
  61, "2021-04-25", 0.43, 2097,
  62, "2021-05-02",  0.45, 2096,
  63, "2021-05-09", 0.47, 2062,
  64, "2021-05-16", 0.48, 2063
)


## NORC ----
# https://apnorc.org/wp-content/uploads/2021/07/COVID2-topline.pdf


# Data ----
polls_est <- bind_rows(
  df_DFP %>% mutate(pollster = "DFP"),
  df_MC  %>% mutate(date = lubridate::as_date(date)),
  df_IP  %>% mutate(pollster = "Ipsos"),
  df_HR  %>% mutate(date = lubridate::as_date(date_start) + date(),
                    pollster = "Harris")
)

ns <- polls_est %>%
  select(date, pollster, matches("n_")) %>%
  pivot_longer(
    cols = c(n_w, n_raw),
    names_pattern = "n_(w|raw)",
    values_to  = c("n"),
    names_to = "weighted"
  )

ests <- polls_est %>%
  select(wave, date, pollster, matches("vax_")) %>%
  pivot_longer(
    cols = c(vax_w, vax_raw),
    names_pattern = "vax_(w|raw)",
    values_to  = c("vax"),
    names_to = "weighted"
  )


vax_df <- left_join(
  ests,
  ns,
  by = c("date", "pollster", "weighted")
) %>%
  bind_rows(
    bench_df %>%
      transmute(date,
                pollster = "CDC",
                vax = pct_pop_vaccinated)
  )

write_csv(vax_df, "data/small-polls_toplines.csv")

