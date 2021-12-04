

# from https://data.cdc.gov/Vaccinations/COVID-19-Vaccination-Trends-in-the-United-States-N/rh2h-3yt2
cdc_jurisdiction_raw <- read_csv('~/Downloads/COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv')

cdc_jurisdiction <- cdc_jurisdiction_raw %>%
  mutate(date = as.Date(Date, tryFormats = c('%m/%d/%Y'))) %>%
  select(date, Location
         , Administered_Dose1_Recip
         , Admin_Per_100k_18Plus
         , Administered_Dose1_Recip_18PlusPop_Pct)

# from https://data.cdc.gov/Vaccinations/COVID-19-Vaccination-Trends-in-the-United-States-N/rh2h-3yt2
cdc_trends_raw <- read_csv('~/Downloads/COVID-19_Vaccination_Trends_in_the_United_States_National_and_Jurisdictional.csv')

cdc_trends <- cdc_trends_raw %>%
  mutate(date = as.Date(Date, tryFormats = c('%m/%d/%Y'))) %>%
  select(date, Location, Admin_Dose_1_Cumulative, date_type)

both <- full_join(cdc_jurisdiction, cdc_trends, by = c('date', 'Location'))

both %>% filter(Location == 'US') %>%
ggplot() +
  geom_line(aes(x = date, y = Administered_Dose1_Recip, color = 'jurisdiction')) +
  geom_line(aes(x = date, y = Admin_Dose_1_Cumulative, color = 'trends', group = date_type))

# from https://data.cdc.gov/Vaccinations/COVID-19-Vaccination-Demographics-in-the-United-St/km4m-vcsb
cdc_demos_raw <- read_csv('~/Downloads/COVID-19_Vaccination_Demographics_in_the_United_States_National.csv')

cdc_demos <- cdc_demos_raw %>%
  filter(grepl('Age', Demographic_category)) %>%
  mutate(date = as.Date(Date, tryFormats = c('%m/%d/%Y'))
         , under_18_flag = Demographic_category %in% c('Ages_12-17_yrs', 'Ages_5-11_yrs')
         , over_18_flag = !Demographic_category %in% c('Ages_12-17_yrs', 'Ages_5-11_yrs', 'Age_unknown', 'Age_known')
         , unknown_flag = Demographic_category == 'Age_unknown'
         , known_flag = Demographic_category == 'Age_known'
         ) %>%
  group_by(date) %>%
  summarize(n_vaxxed_under18 = sum(under_18_flag * Administered_Dose1)
            , n_vaxxed_over18 = sum(over_18_flag * Administered_Dose1)
            , n_vaxxed_unknown = sum(unknown_flag * Administered_Dose1)
            , n_vaxxed = sum(Administered_Dose1 * (known_flag))
            ) %>%
  mutate(pc_vaxxed_over18 = n_vaxxed_over18/255200373)
cdc_demos

all <- full_join(cdc_demos, both %>% filter(Location == 'US'), by = c('date'))
all

all %>%
ggplot() +
  geom_line(aes(x = date, y = Administered_Dose1_Recip, color = 'jurisdiction')) +
  geom_line(aes(x = date, y = Admin_Dose_1_Cumulative, color = 'trends', group = date_type))+
  geom_line(aes(x = date, y = n_vaxxed, color = 'demos'))



# include our data ---------------------------

all_polls <- read_csv('data/final/all_polls.csv') %>% filter(pct_error == 0, pop == 'US') %>% distinct()





