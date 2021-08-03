library(lubridate)
library(patchwork)
library(lemon)
library(ggpubr)
library(scales)
library(wacolors)


toplines <- read_csv("small-polls_toplines.csv")

bench <- toplines %>%
  filter(pollster == "CDC") %>%
  select(-pollster)

polls <- toplines %>%
  filter(pollster != "CDC") %>%
  mutate(se = sqrt(vax * (1 - vax)) / sqrt(n),
         se = replace(se, pollster == "MC", 0.01),
         lb = vax - 2*se,
         ub = vax + 2*se) %>%
  mutate(pollster =
           recode_factor(pollster,
                         Ipsos = "Axios-Ipsos",
                         DFP = "Data for Progress",
                         MC = "Morning Consult",
                         Harris = "Harris Poll"))

polls %>%
  ggplot(aes(x = date,
             y = vax,
             color = pollster,
             fill = pollster,
             group = interaction(pollster, weighted))) +
  geom_line(data = bench,
            aes(x = date, y = vax),
            color = "black",
            alpha = 0.3,
            lwd = 1,
            inherit.aes = FALSE) +
  facet_wrap(~ pollster, nrow = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  geom_pointline(aes(linetype = weighted)) +
  geom_ribbon(aes(ymax = ub, ymin = lb),
              color = NA,
              alpha = 0.2) +
  scale_color_manual(values = unname(wa_pal("larch"))) +
  scale_fill_manual(values = unname(wa_pal("larch"))) +
  scale_linetype_manual(values = c(raw = "dotted", w = "solid")) +
  theme_pubclean() +
  guides(color = FALSE, fill = FALSE, linetype = FALSE) +
  labs(y = "At Least one Dose",
       x = NULL)

ggsave("figures/vax_onlinepolls.pdf", w = 7.5, h = 2.5)

  labs(
    caption = "Do you personally know anyone who has already received the COVID-19 vaccine?
     (1) Yes, I have received the vaccine
    (2) Yes, a member of my immediate family
    (3) Yes, someone else
    (4) No"
  )



df_DFP %>%
  ggplot(aes(x = date)) +
  geom_line(data = bench,
            aes(x = date,  y = pct_pop_vaccinated), color = "black") +
  geom_pointline(aes(y = vax_w), color = "brown") +
  annotate("text", x = date("2021-05-25"), y = 0.50, label = "DFP\nWeighted", color = "brown") +
  geom_pointline(aes(y = vax_raw), color = "orange") +
  annotate("text", x = date("2021-05-25"), y = 0.68, label = "DFP\nUnweighted", color = "orange") +
  scale_y_continuous("Vaccinated", labels = percent_format(accuracy = 1)) +
  expand_limits(y = 0.75) +
  theme_pubclean() +
  labs(
    caption = "As you may know, vaccines for Covid-19 have now been approved by the
    Food and Drug Administration andare being offered to some individuals based on specific criteria.
As of today, have you been vaccinated for Covid-19?
    (1) Yes, I have received at least one Covid-19 vaccination shot
    (2) No, I have not received a Covid-19 vaccination shot"
  )



gg_IP + gg_DFP
fs::dir_create("figures")
ggsave("figures/vax_onlinepolls.pdf", w = 12.5, h = 4)
