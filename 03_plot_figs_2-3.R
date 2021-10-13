
source("_setup.R")
source("functions/functions_plots.R")

library(patchwork)

######### SETTINGS ###########
# max date for analysis -- CDC benchmark has about 5days of reporting delays, so making this 5 days before it was pulled
max_date <- "2021-05-19"

# which version of the benchmark to use
benchmark_date <- "2021-05-26"


plot_width = 6.5
plot_height = 4



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

xdate_m <- scale_x_date(
  labels = function(x) recode(format(as.Date(x), "%b"), "Jan" = "Jan 2021"),
  breaks = seq(as.Date("2021-01-01"), as.Date("2021-05-01"), by = "month"),
  limits = c(as.Date("2021-01-01"), as.Date("2021-05-20")))

xdate_m2 <- scale_x_date(
  labels = function(x) recode(format(as.Date(x), "%b"), "Jan" = "Jan '21"),
  breaks = seq(as.Date("2021-01-01"), as.Date("2021-05-01"), by = "month"),
  limits = c(as.Date("2021-01-01"), as.Date("2021-05-20")))


######### PLOT FIG 2 - ESTIMATES OVER TIME ###########
plt_annotate <- tibble(
  study_name = c("CDC (benchmark)", "Delphi-Facebook", "Axios-Ipsos", "Census Household Pulse"),
  x = c(as.Date("2021-05-13"), as.Date("2021-05-17"), as.Date("2021-05-15"), as.Date("2021-05-12")),
  y = c(0.58, 0.78, 0.645, 0.72),
  n = c("", "250,000", "1000", "75,000")
) %>%
  mutate(plt_lbl = glue("'{study_name}'~~(n%~~%'{n}')"),
         plt_lbl = replace(plt_lbl, study_name == "CDC (benchmark)", "CDC (benchmark)"))



plot_fig2 = ggplot(all_polls_plt_noerror) +
  # benchmark line
  geom_line(data = benchmark,
            size = 1.5,
            aes(x = as.Date(date),
                y = pct_pop_vaccinated,
                color = 'CDC (benchmark)')) +
  # points
  geom_pointline(
    aes(x = as.Date(end_date),
        y = pct_vaccinated,
        color = study_name,
        shape = study_name),
    position = position_dodge(0.008 * 365),
    size = 1) +
  # errorbar
  geom_errorbar(
    aes(x = as.Date(end_date), y = pct_vaccinated, ymin = ci_2.5, ymax = ci_97.5,
        color = study_name,
        shape = study_name),
    position = position_dodge(0.008 * 365),
    width = 0,
    show.legend = FALSE) +
  # legend text
  geom_text(
    data = plt_annotate,
    aes(x = as.Date("2021-01-01"),
        y = y,
        label = plt_lbl,
        color = study_name),
    nudge_x = 1,
    parse = TRUE,
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    show.legend = FALSE
  ) +
  # legend
  geom_point(
    data = plt_annotate %>% filter(study_name != "CDC (benchmark)"),
    aes(shape = study_name,
        color = study_name,
        y = y),
    x = as.Date("2020-12-30"),
    hjust = 0,
    size = 2,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  theme_pubr() +
  theme(legend.position = 'none',
        plot.margin = unit(rep(0, 4), "lines"),
        text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)
  ) +
  labs(x = NULL,
       y = '% Vaccinated (at least 1 dose)',
       color = FALSE,
       title = "Estimates of Vaccination Uptake") +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = scale_values) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.8, 0.1),
                     expand = expansion(mult = c(0, 0.01))) +
  xdate_m +
  coord_cartesian(clip = "off") +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.5) +
  expand_limits(y = 0.8)
# annotate('text', x = as.Date('2021-01-25'), y = 0.52, label = '50% with one dose')
plot_fig2

ggsave(plot_fig2,
       filename = file.path('plots', 'fig2.png'),
       device = 'png',
       width = plot_width,
       height = plot_height,
       units = 'in')




################# MAKE FIG 3 ################
xlims <- c(as.Date("2021-01-01"), as.Date("2021-05-11"))

fig3_pl <- list()

## panel A - error
fig3_pl[["panelA_error"]] <- plot_with_errorbands(
  data = all_polls_plt_wide,
  outcome = "error",
  ylab = "Error",
  include_legend = TRUE,
  title = "Total error",
  xlim_val = xlims
)

fig3_pl[["panelA_error"]] <- fig3_pl[["panelA_error"]] +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  xdate_m2 +
  labs(y = expression(bar(Y)[n] - bar(Y)[N]))


## panel B - sd_G
fig3_pl[["panelB_sdG"]] <- ggplot(benchmark, aes(x = as.Date(date), y = sd_G)) +
  lemon::geom_pointline(aes(color = study_name), size = 0.1) +
  theme_pubr() +
  labs(x = NULL, y = expression(sigma[Y]),
       color = "Study",
       shape = "Study",
       title = "Inherent problem difficulty") +
  scale_color_manual(values = scale_values) +
  expand_limits(y = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  xdate_m2 +
  annotate(geom = "text",
           color = "darkgray",
           x = as.Date("2021-04-13"),
           y = 0.35,
           size = 2.5,
           label = "CDC (benchmark)") +
  theme(axis.title = element_text(size = 8),
        axis.text = element_text(size = 8))


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
fig3_pl[["panelC_f"]] <- fig3_pl[["panelC_f"]] +
  scale_y_continuous(labels = scales::percent) +
  xdate_m2

## panel D - dropout odds
fig3_pl[["panelC_DO"]] <- plot_with_errorbands(
  data = all_polls_plt_wide,
  outcome = "DO_sqrt",
  include_legend = TRUE,
  title = "Data scarcity",
  use_ribbons = NULL,
  xlim_val = xlims
) +
  labs(y = expression(sqrt((N-n)/N))) +
  xdate_m2 +
  expand_limits(y = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

## panel D - ddc
fig3_pl[["panelD_ddc"]] <- plot_with_errorbands(
  data = all_polls_plt_wide,
  outcome = "ddc_weighted",
  ylab = "ddc",
  include_legend = TRUE,
  title = "Data quality defect\n(data defect correlation)",
  xlim_val = xlims
)

fig3_pl[["panelD_ddc"]] <- fig3_pl[["panelD_ddc"]] +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  xdate_m2 +
  labs(y = expression(hat(rho)[list(R, Y)]))



## panel E - effective sample size, Facebook
fig3_pl[["panelE_neff_fb"]] <- plot_with_errorbands(
  data = all_polls_plt_wide %>% filter(mode == "facebook"),
  outcome = "n_eff_star_cap",
  ylab = "Effective sample size",
  include_legend = FALSE,
  title = "Effective sample size",
  xlim_val = xlims
)
fig3_pl[["panelE_neff_fb"]] <- fig3_pl[["panelE_neff_fb"]] +
  scale_y_continuous(expand = c(0, 0)) +
  xdate_m2

## panel F - effective sample size, Census Household Pulse
fig3_pl[["panelF_neff_chp"]] <- plot_with_errorbands(
  data = all_polls_plt_wide %>% filter(mode == "household_pulse"),
  outcome = "n_eff_star_cap",
  ylab = "Effective sample size",
  xlim_val = xlims,
  include_legend = FALSE,
  title = "Effective sample size"
)
fig3_pl[["panelF_neff_chp"]] <- fig3_pl[["panelF_neff_chp"]] +
  scale_y_continuous(expand = c(0, 0)) +
  xdate_m2

# E and F together
fig3_pl[["panelG_neff_all"]] <-
  plot_with_errorbands(
    data = all_polls_plt_wide %>%
      rowwise() %>%
      mutate(
        n_eff_adj_less10pct = min(n_eff_star_cap_less10pct, n_eff_star_cap_plus10pct),
        n_eff_adj_less5pct  = min(n_eff_star_cap_less5pct, n_eff_star_cap_plus5pct),
        n_eff_adj_plus10pct = max(n_eff_star_cap_less10pct, n_eff_star_cap_plus10pct),
        n_eff_adj_plus5pct  = max(n_eff_star_cap_less5pct, n_eff_star_cap_plus5pct),
        `n_eff_adj_no error` = `n_eff_star_cap_no error`,
      ),
    outcome = "n_eff_adj",
    ylab = "Bias-adjusted\nEffective sample size",
    xlim_val = xlims,
    include_legend = FALSE,
    title = NULL,
    use_ribbons = c('5pct')
  ) +
  scale_y_log10(labels = comma_format(accuracy = 1)) +
  xdate_m +
  theme_pubclean() +
  # facet_wrap(~ study_name) +
  # guides(color = FALSE) +
  scale_x_date(
    labels = function(x) recode(format(as.Date(x), "%b"), "May" = "May 2021"),
    breaks = seq(as.Date("2021-01-01"), as.Date("2021-05-01"), by = "month"),
    limits = c(as.Date("2021-01-01"), as.Date("2021-05-20"))) +
  scale_color_manual(values = scale_values) +
  theme(axis.line = element_line(),
        axis.text = element_text(color = "gray10"),
        legend.position = "right") +
  labs(color = NULL, shape = NULL)



######### MAKE PANELS #########

## New Fig. 1 ------
layout =
  "AABC
 AADE"

(plot_fig2 +  guides(color = FALSE, shape = FALSE)) +
  fig3_pl[["panelA_error"]] +
  fig3_pl[["panelD_ddc"]] +
  fig3_pl[["panelC_DO"]] +
  (fig3_pl[["panelB_sdG"]] + guides(color = FALSE)) +
  plot_layout(design = layout,
              guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom",
        plot.title = element_text(size = 10, face = "bold"),
        # axis.text = element_text(size = 8),
        plot.tag = element_text(face = 'bold'))

ggsave("plots/fig1_topline.pdf",
       w = 18*1.2,
       h = 9*1.2,
       units = "cm")


## New Fig. 2 ----
ggsave("plots/fig2_n-eff.pdf",
       fig3_pl[["panelG_neff_all"]] + labs(shape = NULL),
       w = 9*1.5,
       h = 5*1.5,
       units = "cm")

## OLD R and R PANELS (Fig. 2 - 3)

## 4 panel -------
fig3_4panel <- ggarrange(fig3_pl[["panelA_error"]],
                         fig3_pl[["panelB_sdG"]],
                         fig3_pl[["panelC_DO"]],
                         fig3_pl[["panelD_ddc"]],
                         common.legend = TRUE,
                         legend = "bottom",
                         labels = c("A", "B", "C", "D", "E", "F"),
                         nrow = 2, ncol = 2,
                         align = "hv"
)
ggsave(fig3_4panel,
       filename = file.path("plots", "fig3_4panel.pdf"),
       device = "pdf",
       width = 8,
       height = 7,
       units = "in"
)

## 6 panel -----
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
  filename = file.path("plots", "fig3_6panel.pdf"),
  device = "pdf",
  width = 11,
  height = 5,
  units = "in"
)