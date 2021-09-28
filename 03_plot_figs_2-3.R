
source("_setup.R")
source("functions/functions_plots.R")


######### SETTINGS ###########
# max date for analysis -- CDC benchmark has about 5days of reporting delays, so making this 5 days before it was pulled
max_date <- "2021-05-19"

# which version of the benchmark to use
benchmark_date <- "2021-05-26"

plot_width <- 9
plot_height <- 6


######### BENCHMARK DATA ###########
# read in benchmark data
benchmark <- fread(file.path("data", "final", glue("benchmark_{benchmark_date}.csv"))) %>%
  # use CDC data, not OWID
  filter(source == "CDC_historical", date <= max_date) %>%
  select(-source) %>%
  mutate(study_name = "CDC (benchmark)")


######### POLL DATA ###########
# read in poll data
all_polls_plt <- fread(file.path("data", "final", "all_polls_all_vars.csv.gz")) %>%
  filter(pop == "US", end_date <= max_date, end_date <= max(benchmark$date))

all_polls_plt_noerror <- all_polls_plt %>% filter(pct_error == 0)

# make pivoted version
all_polls_plt_wide <- all_polls_plt %>%
  pivot_wider(
    id_cols = c("end_date", "pop", "mode", "start_date", "study_name", "n", "source"),
    names_from = "pct_error_lab",
    values_from = c(
      "pct_vaccinated", "deff", "pct_pop_vaccinated",
      "ddc_weighted", "n_eff_star", "n_eff_star_cap",
      "error", "sd_G", "f", "DO", "DO_sqrt", "pct_reduction_n_eff_cap"
    )
  )




########## MAKE PLOT ##########


######### PLOT FIG 2 - ESTIMATES OVER TIME ###########
plt_annotate <- tibble(
  study_name = c("CDC (benchmark)", "Delphi-Facebook", "Axios-Ipsos", "Census Household Pulse"),
  x = c(as.Date("2021-05-13"), as.Date("2021-05-17"), as.Date("2021-05-15"), as.Date("2021-05-12")),
  y = c(0.58, 0.78, 0.645, 0.72),
  n = c("", "250,000", "1000", "75,000")
) %>%
  mutate(plt_lbl = glue("'{study_name}' (n%~~%'{n}')"),
         plt_lbl = replace(plt_lbl, study_name == "CDC (benchmark)", "CDC (benchmark)"))

plot_fig2 <- ggplot(all_polls_plt_noerror) +
  geom_line(
    data = benchmark,
    aes(x = as.Date(date), y = pct_pop_vaccinated, color = "CDC (benchmark)")) +
  geom_pointline(
    aes(x = as.Date(end_date), y = pct_vaccinated, color = study_name),
    size = 0.5,
    position = position_dodge(0.008 * 365)
  ) +
  geom_pointrange(
    aes(x = as.Date(end_date), y = pct_vaccinated, ymin = ci_2.5, ymax = ci_97.5, color = study_name),
    position = position_dodge(0.008 * 365),
    fatten = 2
  ) +
  geom_text(
    data = plt_annotate,
    aes(x = as.Date(x),
        y = y, label = plt_lbl, color = study_name),
    # direction = "x",
    # nudge_x = 1,
    parse = TRUE,
    inherit.aes = FALSE,
    hjust = 0,
    # min.segment.length = 10,
    size = 3
  ) +
  theme_pubr() +
  labs(x = NULL,
       y = "% Vaccinated (at least 1 dose)",
       color = "Study") +
  scale_color_manual(values = scale_values) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.8, 0.1),
                     expand = expansion(mult = c(0, 0.01))) +
  scale_x_date(date_labels = "%b\n%Y",
               breaks = seq(as.Date("2021-01-01"), as.Date("2021-05-01"), by = "month"),
               limits = c(as.Date("2021-01-01"), as.Date("2021-07-15"))) +
  coord_cartesian(clip = "off") +
  guides(color = FALSE)
plot_fig2

ggsave(plot_fig2,
  filename = file.path("plots", "fig2.png"),
  width = 6,
  height = 3,
  units = "in"
)





################# MAKE FIG 3 ################
xlims <- c(as.Date("2021-01-01"), as.Date("2021-05-11"))

fig3_pl <- list()

## panel A - error
fig3_pl[["panelA_error"]] <- plot_with_errorbands(
  data = all_polls_plt_wide,
  outcome = "error",
  ylab = "Error",
  include_legend = TRUE,
  title = "Estimate error",
  xlim_val = xlims
)
fig3_pl[["panelA_error"]] <- fig3_pl[["panelA_error"]] + geom_hline(yintercept = 0, lty = 2)


## panel B - sd_G
fig3_pl[["panelB_sdG"]] <- ggplot(benchmark, aes(x = as.Date(date), y = sd_G)) +
  lemon::geom_pointline(aes(color = study_name)) +
  theme_pubr() +
  labs(x = NULL, y = expression(sigma[Y]), color = "Study", title = expression("Problem difficulty")) +
  scale_color_manual(values = scale_values) +
  xlim(xlims) +
  annotate(geom = "text", color = "darkgray", x = as.Date("2021-04-13"), y = 0.42, label = "CDC (benchmark)")


## panel C - f, sampling fraction
fig3_pl[["panelC_f"]] <- plot_with_errorbands(
  data = all_polls_plt_wide,
  outcome = "f",
  ylab = "% of pop sampled",
  include_legend = TRUE,
  title = "Proportion of population sampled (n/N)",
  use_ribbons = NULL,
  xlim_val = xlims
)
fig3_pl[["panelC_f"]] <- fig3_pl[["panelC_f"]] + scale_y_continuous(labels = scales::percent)

## panel D - dropout odds
fig3_pl[["panelC_DO"]] <- plot_with_errorbands(
  data = all_polls_plt_wide,
  outcome = "DO_sqrt",
  include_legend = TRUE,
  title = "Data quantity index",
  use_ribbons = NULL,
  xlim_val = xlims
) +
  labs(y = expression(sqrt((N-n)/N)))

## panel D - ddc
fig3_pl[["panelD_ddc"]] <- plot_with_errorbands(
  data = all_polls_plt_wide,
  outcome = "ddc_weighted",
  ylab = "ddc",
  include_legend = TRUE,
  title = "Data defect correlation (ddc)",
  xlim_val = xlims
)
fig3_pl[["panelD_ddc"]] <- fig3_pl[["panelD_ddc"]] + geom_hline(yintercept = 0, lty = 2)



## panel E - effective sample size, Facebook
fig3_pl[["panelE_neff_fb"]] <- plot_with_errorbands(
  data = all_polls_plt_wide %>% filter(mode == "facebook"),
  outcome = "n_eff_star_cap",
  ylab = "Effective sample size",
  include_legend = FALSE,
  title = "Effective sample size - Facebook",
  xlim_val = xlims
)
fig3_pl[["panelE_neff_fb"]] <- fig3_pl[["panelE_neff_fb"]] + scale_y_continuous(expand = c(0, 0))

## panel F - effective sample size, Census Household Pulse
fig3_pl[["panelF_neff_chp"]] <- plot_with_errorbands(
  data = all_polls_plt_wide %>% filter(mode == "household_pulse"),
  outcome = "n_eff_star_cap",
  ylab = "Effective sample size",
  xlim_val = xlims,
  include_legend = FALSE,
  title = "Effective sample size - Census"
)
fig3_pl[["panelF_neff_chp"]] <- fig3_pl[["panelF_neff_chp"]] + scale_y_continuous(expand = c(0, 0))


## save plots
for (p in names(fig3_pl)) {
  ggsave(fig3_pl[[p]],
    filename = file.path("plots", glue("fig3_{p}.png")),
    device = "png",
    width = plot_width - 2,
    height = plot_height - 1,
    units = "in"
  )
}







######### MAKE PANELS #########

## 4panel
fig3_4panel <- ggarrange(fig3_pl[["panelA_error"]],
  fig3_pl[["panelB_sdG"]],
  fig3_pl[["panelC_DO"]],
  fig3_pl[["panelD_ddc"]],
  common.legend = TRUE,
  legend = "bottom",
  labels = c("A", "B", "C", "D"),
  align = "hv"
)
ggsave(fig3_4panel,
  filename = file.path("plots", "fig3_4panel.png"),
  device = "png",
  width = plot_width,
  height = plot_height,
  units = "in"
)


## 6 panel
fig3_6panel <- ggarrange(fig3_pl[["panelA_error"]],
  fig3_pl[["panelB_sdG"]],
  fig3_pl[["panelC_DO"]],
  fig3_pl[["panelD_ddc"]],
  fig3_pl[["panelE_neff_fb"]],
  fig3_pl[["panelF_neff_chp"]],
  common.legend = TRUE,
  legend = "bottom",
  labels = c("A", "B", "C", "D", "E", "F"),
  nrow = 2, ncol = 3,
  align = "hv"
)
ggsave(fig3_6panel,
  filename = file.path("plots", "fig3_6panel.png"),
  device = "png",
  width = 12,
  height = 7,
  units = "in"
)
