# ---- Load Packages ----
{
  library(data.table)
  library(dplyr)
  library(fitdistrplus)
  library(ggplot2)
  library(gratia)
  library(here)
  library(itsadug)
  library(lubridate)
  library(mgcv)
  library(qs)
}

# ---- bring in summary dataframe -----

dat <- qread(here("data-saved",
                  "summary-accel-doy",
                  "lmb_np_summary_accel_1h.qs"))

glimpse(dat)


# ---- look at distrubtion ----

ggplot(data = dat, aes(x = mean_accel)) +
  geom_histogram() +
  facet_wrap(. ~ common_name_e)


# ---- create hr column ----

dat <- dat %>%
  mutate(
    tod = hour(time_bin_1h)
  )


glimpse(dat)


lmb <- dat %>%
  filter(common_name_e %in% "Largemouth Bass" &
           habitat_type != is.na(habitat_type)) %>%
  mutate(
    season = as.factor(season),
    animal_id = as.factor(animal_id),
    year = as.factor(year)
  )

# ---- model ----
m <- bam(mean_accel ~ season +
           s(tod, by = season, k = 4,
             bs = "cc") +
           s(animal_id, bs = "re") +
           s(year, bs = "re"),
         family = gaussian(link = "log"),
         select = TRUE,
         data = lmb)

anova.gam(m)
# draw(m)
# appraise(m)

# --- predict ----
lmb_1 <- lmb %>%
  mutate(
    animal_id = "a",
    year = "0"
  )

fits <- predict.bam(m, newdata = lmb_1, se.fit = TRUE,
                    exclude = c("s(animal_id)",
                                "s(year)"))
preds <- data.frame(lmb_1, fits) %>%
  mutate(
    lower = exp(1) ^ (fit - 1.98 * se.fit),
    upper = exp(1) ^ (fit + 1.98 * se.fit),
    fit_t  = exp(1) ^ fit
  )
glimpse(preds)
# ---- plot ----


#
#
p <- ggplot(data = dat, aes(x = factor(tod), y = mean_accel)) +
  geom_boxplot(aes(fill = season)) +
  facet_wrap(. ~ common_name_e) +
  theme_bw(
    base_size = 15
  ) +
  scale_fill_viridis_d(end = 0.85, name = "Season",
                       alpha = 0.5) +
  theme(
    strip.background = element_blank(),
  ) +
  labs(
    x = "Hour",
    y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )

p

lmb_sum <- lmb %>%
  group_by(season, tod) %>%
  summarise(
    mean_ac = mean(mean_accel),
    sem = sd(mean_accel) / sqrt(n())
  ) %>%
  ungroup()

p1 <- ggplot() +
  geom_errorbar(data = lmb_sum, width = 0.15,
                aes(group = season,
                    x = tod, y = mean_ac,
                    ymin = mean_ac - sem,
                    ymax = mean_ac + sem),
                position = position_jitter(width = 0.2,
                                           seed = 1)
  ) +
  geom_point(data = lmb_sum,
             shape = 21,
             aes(x = tod,
                 y = mean_ac,
                 fill = season),
             size = 2,
             position = position_jitter(width = 0.2, seed = 1)) +
  geom_line(data = preds, aes(x = tod,
                              y = fit_t, colour = season)) +
  geom_ribbon(data = preds, aes(x = tod,
                                y = fit_t, fill = season,
                                ymin = lower,
                                ymax = upper),
              alpha = 0.2) +
  # facet_wrap(. ~ common_name_e) +
  scale_fill_viridis_d(end = 0.85, name = "Season",
                       alpha = 0.5) +
  scale_colour_viridis_d(end = 0.85, name = "Season"
  ) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    x = "Hour",
    y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )


ggsave(here("plots",
            "hour-gamm-accel",
            "gamm_hour_season.png"), plot = p1,
       width = 11, height = 8.5)


