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
# look at data structure


np <- dat %>%
  filter(common_name_e == "Northern Pike") %>%
  mutate(
    season = factor(season,
                    levels = c("Fall", "Winter", "Spring", "Summer")
    ),
    habitat_type = factor(habitat_type)
  )


hist(np$mean_accel)
summary(np$mean_accel)

descdist(np$mean_accel)

fit_lognorm <- fitdist(np$mean_accel, distr = "lnorm", method = "mme")
plot(fit_lognorm)


np <- np %>%
  filter(habitat_type != is.na(habitat_type) &
           season != is.na(season))


np %>%
  distinct(day_night)

glimpse(np)

# ---- start our models for np ----

m <- glmmTMB(mean_accel ~ habitat_type * season  * day_night +
               (1 | animal_id) +
               ar1(season + 0 | animal_id),
             data = np,
             family = lognormal(link = "log"),
             control = glmmTMBControl(optimizer = optim,
                                      optArgs = list(method = "BFGS"))
)


# # ---- ggeeffects ----

pres <- predict_response(m, terms = c("season", "habitat_type", "day_night") )

unique(pres$group)

pres <- as_tibble(pres) %>%
  mutate(
    habitat_type = factor(case_when(
      group == "Deep/Low SAV" ~ "Deep un-vegetated",
      group == "Exposed/Low SAV" ~ "Exposed",
      group == "Mod/Dense SAV" ~ "Coastal vegetated",
      group == "Shallow/Dense SAV" ~ "Wetlands",
      group == "Shallow/Low SAV" ~ "Shallow un-vegetated",

    ), level = c(
      "Wetlands", "Coastal vegetated", "Shallow un-vegetated",
      "Deep un-vegetated", "Exposed"
    )
    )
  )
pres


# tp <- test_predictions(m, terms = c("season", "habitat_type", "day_night"))

p <- pres %>%
  ggplot() +
  geom_linerange(aes(colour = habitat_type,
                     x = facet, y = predicted,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_dodge(width = 0.5)) +
  geom_point(shape = 21, colour = "black", size = 3,
             aes(y = predicted, x = facet, fill = habitat_type),
             position = position_dodge(width = 0.5)) +

  scale_y_continuous(breaks = seq(0, 0.6, 0.1)) +
  coord_cartesian(ylim = c(0.1, 0.6)) +
  theme_bw() +
  lemon::facet_rep_wrap(.~ x, repeat.tick.labels = TRUE) +
  scale_fill_viridis_d(name = "Habitat Type",
                       option = "D", end = 0.85) +
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
    title = "Northern Pike",
    x = "Season",
    y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )


p

qs::qsave(p, file = here("data-saved",
                         "predicted-plots",
                         "np_season_hab_dp_plot.qs"))
# ggsave(plot = p, filename = here::here("Plots",
#                                        "predicted-results",
#                                        "preliminary_northern_pike_sea_hab_day_bh_version_update.png"), width = 11,
#        height = 8.5)
# summary(m5)
# pres_1 <- predict_response(m5, terms = c("season", "habitat_type"))
# pres_1
# tp <- test_predictions(m5, terms = c("season", "habitat_type"))
# p1 <- as_tibble(pres_1) %>%
#   ggplot() +
#   geom_linerange(aes(colour = group,
#                      x = x, y = predicted,
#                      ymin = conf.low,
#                      ymax = conf.high),
#                  position = position_dodge(width = 0.5)) +
#   geom_point(shape = 21, colour = "black", size = 3,
#              aes(y = predicted, x = x, fill = group),
#              position = position_dodge(width = 0.5)) +
#
#   scale_y_continuous(breaks = seq(0, 0.6, 0.1)) +
#   coord_cartesian(ylim = c(0.1, 0.3)) +
#   theme_bw() +
#   # lemon::facet_rep_wrap(.~ facet, repeat.tick.labels = TRUE) +
#   scale_fill_viridis_d(name = "Habitat Type",
#                        option = "D", end = 0.85) +
#   scale_colour_viridis_d(name = "Habitat Type",
#                          option = "D", end = 0.85) +
#   theme(
#     strip.background = element_blank(),
#     panel.grid = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "inside",
#     legend.position.inside = c(0.1, 0.89)
#   ) +
#   labs(
#     title = "Northern Pike",
#     x = "Season",
#     y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
#   )
# p1
# ggsave(plot = p1, filename = here::here("Plots",
#                                         "predicted-results",
#                                         "preliminary_northern_pike_sea_hab_bh_version.png"), width = 11,
#        height = 8.5)
# pres_2 <- predict_response(m7, terms = c("day_night", "habitat_type"))
# pres_2
# p2 <- as_tibble(pres_2) %>%
#   ggplot() +
#   geom_linerange(aes(colour = group,
#                      x = x, y = predicted,
#                      ymin = conf.low,
#                      ymax = conf.high),
#                  position = position_dodge(width = 0.5)) +
#   geom_point(shape = 21, colour = "black", size = 3,
#              aes(y = predicted, x = x, fill = group),
#              position = position_dodge(width = 0.5)) +
#
#   scale_y_continuous(breaks = seq(0, 0.6, 0.1)) +
#   coord_cartesian(ylim = c(0.1, 0.3)) +
#   theme_bw() +
#   # lemon::facet_rep_wrap(.~ facet, repeat.tick.labels = TRUE) +
#   scale_fill_viridis_d(name = "Habitat Type",
#                        option = "D", end = 0.85) +
#   scale_colour_viridis_d(name = "Habitat Type",
#                          option = "D", end = 0.85) +
#   theme(
#     strip.background = element_blank(),
#     panel.grid = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "inside",
#     legend.position.inside = c(0.1, 0.89)
#   ) +
#   labs(
#     title = "Northern Pike",
#     x = "Diel Period",
#     y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
#   )
# p2
#
#
# ggsave(plot = p2, filename = here::here("Plots",
#                                         "predicted-results",
#                                         "preliminary_northern_pike_hab_day_night_bh_version.png"), width = 11,
#        height = 8.5)
