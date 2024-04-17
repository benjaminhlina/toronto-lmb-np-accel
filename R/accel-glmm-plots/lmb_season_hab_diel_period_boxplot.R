# ---- Load Packages ----
{
  library(broom.mixed)
  library(data.table)
  library(dplyr)
  library(DHARMa)
  library(emmeans)
  library(fitdistrplus)
  library(factoextra)
  library(here)
  library(ggplot2)
  library(ggeffects)
  library(glmmTMB)
  library(MASS)
  library(multcomp)
  library(performance)
  library(qs)
  library(purrr)
  library(stringr)
  library(tidyr)
}

# ---- bring in summary dataframe -----
dat <- qread(here("data-saved",
                  "summary-accel-doy",
                  "lmb_np_summary_accel_1h.qs"))

glimpse(dat)

dat <- dat %>%
  filter(habitat_type != is.na(habitat_type))
# look at data structure

lmb_sum <- lmb %>%
  group_by(season, day_night, habitat_type) %>%
  summarise(
    n = n()
  ) %>%
  ungroup()

lmb_sum %>%
  print(n = 64)

lmb <- dat %>%
  filter(common_name_e == "Largemouth Bass") %>%
  group_by(season, day_night, habitat_type) %>%
  filter(
    n() > 2
  ) %>%
  ungroup() %>%
  mutate(
    season = factor(season,
                    levels = c("Fall", "Winter", "Spring", "Summer")
    ),
    habitat_type = factor(habitat_type)
  )



# tp <- test_predictions(m, terms = c("season", "habitat_type", "day_night"))


p <- ggplot() +
  geom_boxplot(data = lmb, aes(x = day_night, fill = habitat_type,
                              y = mean_accel),
               outlier.shape = NA) +
  scale_y_continuous(breaks = seq(0, 2.25, 0.25)) +
  coord_cartesian(ylim = c(0, 2.25)) +
  theme_bw() +
  lemon::facet_rep_wrap(.~ season, repeat.tick.labels = TRUE) +
  scale_fill_viridis_d(name = "Habitat Type",
                       option = "D", end = 0.85, alpha = 0.5) +
  scale_colour_viridis_d(name = "Habitat Type",
                         option = "D", end = 0.85) +
  theme_bw(base_size = 15) +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.875)
  ) +
  labs(
    title = "Largemouth Bass",
    x = "Season",
    y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )


p
qs::qsave(p, file = here("data-saved",
                         "predicted-plots",
                         "lmb_season_hab_dp_plot_boxplot.qs"))


#
# p1 <- ggplot() +
#   geom_violin(data = np %>%
#                 filter(!(season == "Fall" &
#                            day_night == "Dawn")),
#               aes(x = day_night, fill = habitat_type,
#                   y = mean_accel),
#               # outlier.shape = NA
#   ) +
#   stat_summary(data = np, aes(x = day_night,
#                               group = habitat_type,
#                               y = mean_accel),
#                geom = "errorbar",
#                fun.data = mean_se, width = 0.1,
#                position = position_dodge(width = 0.9)) +
#   stat_summary(data = np, aes(x = day_night,
#                               y = mean_accel,
#                               group = habitat_type),
#                geom = "point",
#                fun = mean,
#                size = 2,
#                position = position_dodge(width = 0.9)) +
#   # scale_y_continuous(breaks = seq(0, 0.6, 0.1)) +
#   # coord_cartesian(ylim = c(0, 0.6)) +
#   theme_bw() +
#   lemon::facet_rep_wrap(.~ season, repeat.tick.labels = TRUE) +
#   scale_fill_viridis_d(name = "Habitat Type",
#                        option = "D", end = 0.85, alpha = 0.5) +
#   scale_colour_viridis_d(name = "Habitat Type",
#                          option = "D", end = 0.85) +
#   theme_bw(base_size = 15) +
#   theme(
#     strip.background = element_blank(),
#     panel.grid = element_blank(),
#     legend.background = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "inside",
#     legend.position.inside = c(0.9, 0.875)
#   ) +
#   labs(
#     title = "Northern Pike",
#     x = "Season",
#     y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
#   )
# p1
#
#
