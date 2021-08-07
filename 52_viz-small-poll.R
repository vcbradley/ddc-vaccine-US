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
  filter(weighted == "w") %>%
  mutate(se = sqrt(vax * (1 - vax)) / sqrt(n),
         se = replace(se, pollster == "MC", 0.01),
         lb = vax - 2*se,
         ub = vax + 2*se) %>%
  mutate(pollster =
           recode_factor(pollster,
                         Ipsos = "Axios-Ipsos",
                         DFP = "Data for Progress",
                         MC = "Morning Consult",
                         Harris = "Harris Poll")) %>%
  mutate(pct_label = percent(vax, accuracy = 1),
         pct_label = replace(pct_label, pollster %in% c("Morning Consult", "Harris Poll") & wave %% 2 == 0, NA_character_),
         pct_label = replace(pct_label, pollster == "Data for Progress" & wave == 22, NA_character_),
         pct_label = replace(pct_label, pollster == "Axios-Ipsos" & wave == 41, NA_character_)
         )

color_pal <- c("#cf7a30", unname(wa_pal("larch")[c(1, 2, 4)]))

polls %>%
  ggplot(aes(x = date,
             y = vax,
             color = pollster,
             fill = pollster)) +
  geom_line(data = bench,
            aes(x = date, y = vax),
            color = "black",
            alpha = 0.3,
            lwd = 1,
            inherit.aes = FALSE) +
  facet_wrap(~ pollster, nrow = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.75, 0.25),
                     expand = expansion(add = c(0.001, 0.05))) +
  expand_limits(y = 0.75) +
  geom_pointline() +
  geom_ribbon(aes(ymax = ub, ymin = lb),
              color = NA,
              alpha = 0.2) +
  geom_text(aes(label = pct_label), nudge_y = 0.05, size = 3) +
  scale_color_manual(values = color_pal) +
  scale_fill_manual(values = color_pal) +
  theme_pubclean() +
  guides(color = FALSE, fill = FALSE) +
  labs(y = "% Vaccinated (at least 1 dose)",
       x = NULL)

ggsave("figures/vax_onlinepolls.pdf", w = 7.5, h = 2.5)
