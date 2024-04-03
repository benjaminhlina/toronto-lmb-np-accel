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


lmb <- dat %>%
  filter(common_name_e == "Largemouth Bass") %>%
  mutate(
    season = factor(season,
                    levels = c("Fall", "Winter", "Spring", "Summer")
    ),
    habitat_type = factor(habitat_type)
  ) %>%
  filter(habitat_type != is.na(habitat_type) &
           season != is.na(season))
hist(lmb$mean_accel)

lognor <- fitdist(lmb$mean_accel, distr = "lnorm", method = "mme")

plot(lognor)
gammas <- fitdist(lmb$mean_accel, distr = "gamma", method = "mme")

plot(gammas)

# ---- start our models for LMB ----
glimpse(lmb)
m <- glmmTMB(mean_accel ~ habitat_type * season * day_night +
               (1 | animal_id) +
               ar1(season + 0 | animal_id),
             data = lmb,
             family = lognormal(link = "log")
)

# ---- prediticed means
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

  # scale_y_continuous(breaks = seq(0, 0.6, 0.1)) +
  # coord_cartesian(ylim = c(0.1, 0.6)) +
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
    title = "Largemouth Bass",
    x = "Season",
    y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )
# p

qs::qsave(p, file = here("data-saved",
                         "predicted-plots",
                         "lmb_season_hab_dp_plot.qs"))
# ggsave(plot = p, filename = here::here("Plots",
#                                        "predicted-results",
#                                        "lmb",
#                                        "preliminary_lmb_sea_hab_day_bh_version_update.png"), width = 11,
#        height = 8.5)
