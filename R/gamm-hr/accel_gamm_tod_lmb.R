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

# ---- filter out lmb and fix column classes for gamming -----
lmb <- dat %>%
  filter(common_name_e %in% "Largemouth Bass" &
           habitat_type != is.na(habitat_type)) %>%
  mutate(
    season = factor(season,
                       levels = c("Fall", "Winter",
                                  "Spring", "Summer")),
    animal_id = as.factor(animal_id),
    year = as.factor(year),
    habitat_type = as.factor(habitat_type),
    hab_season = paste(season, habitat_type, sep = " ")
  )

glimpse(lmb)



# ---- look at habitat type season sample numbers -----

lmb_smp <- lmb %>%
  group_by(season, habitat_type, hab_season) %>%
  summarise(
    n_det = n_distinct(animal_id),
    n_hr = n_distinct(tod)
  ) %>%
  ungroup()

lmb_smp_f <- lmb_smp %>%
  filter(n_hr > 23)

lmb_smp_f
lmb_1 <- lmb %>%
  filter(hab_season %in% lmb_smp_f$hab_season &
           hab_season != "Summer Mod/Dense SAV")


# ---- add in start column for autocorrelation -----
lmb_1 <- lmb_1 %>%
  arrange(animal_id, year, time_bin_1h) %>%
  group_by(animal_id) %>%
  mutate(start_event = if_else(time_bin_1h == min(time_bin_1h), true = TRUE,
                               false = FALSE)) %>%
  ungroup() %>%
  arrange(time_bin_1h, start_event)


# ---- check distribution -----
ac <- lmb_1$mean_accel

descdist(ac)

fit_gamma <- fitdist(data = ac, distr = "gamma", method = "mme")

plot(fit_gamma)

glimpse(lmb_1)
ggplot(data = lmb, aes(x = tod, y = mean_accel)) +
  geom_jitter(aes(colour = season))


# ---- model ----
m <- bam(mean_accel ~ season * habitat_type +
           s(tod,
             by = season,
             # by = interaction(season, habitat_type),
             k = 4,
             bs = "cc") +
           s(tod, by = habitat_type, k = 4,
             bs = "cc") +
           s(animal_id, bs = "re") +
           s(year, bs = "re"),
         family = Gamma(link = "log"),
         select = TRUE,
         data = lmb_1)

# anova.gam(m)
draw(m)
appraise(m)

r1 <- start_value_rho(m, plot = TRUE)
r1

m1 <- update(m,
             select = TRUE,
             discrete = TRUE,
             rho = r1,
             AR.start = start_event)

ac
draw(m1)
appraise(m1)
acf_resid(m1)
# --- predict ----
lmb_2 <- lmb_1 %>%
  mutate(
    animal_id = "a",
    year = "0"
  )

fits <- predict.gam(m1, newdata = lmb_2, se.fit = TRUE,
                    exclude = c("s(animal_id)",
                                "s(year)"))
preds <- data.frame(lmb_2, fits) %>%
  mutate(
    lower = exp(1) ^ (fit - 1.98 * se.fit),
    upper = exp(1) ^ (fit + 1.98 * se.fit),
    fit_t  = exp(1) ^ fit
  )
glimpse(preds)
# ---- plot ----


#
#
# p <- ggplot(data = dat, aes(x = factor(tod), y = mean_accel)) +
#   geom_boxplot(aes(fill = season)) +
#   facet_wrap(. ~ common_name_e) +
#   theme_bw(
#     base_size = 15
#   ) +
#   scale_fill_viridis_d(end = 0.85, name = "Season",
#                        alpha = 0.5) +
#   theme(
#     strip.background = element_blank(),
#   ) +
#   labs(
#     x = "Hour",
#     y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
#   )
#
# p

lmb_sum <- lmb_1 %>%
  group_by(season, habitat_type, tod) %>%
  summarise(
    n_tags = n_distinct(animal_id),
    mean_ac = mean(mean_accel),
    sem = sd(mean_accel) / sqrt(n())
  ) %>%
  ungroup()


# lmb_sum_sm <- lmb_sum %>%
#   filter(season == "Summer" & habitat_type == "Mod/Dense SAV")
#
# lmb_sum_sm
#
# ggplot() +
#   geom_errorbar(data = lmb_sum_sm, width = 0.15,
#                 aes(group = habitat_type,
#                     x = tod, y = mean_ac,
#                     ymin = mean_ac - sem,
#                     ymax = mean_ac + sem),
#                 position = position_jitter(width = 0.3,
#                                            seed = 1)
#   ) +
#   geom_point(data = lmb_sum_sm,
#              shape = 21,
#              aes(x = tod,
#                  y = mean_ac,
#                  fill = habitat_type),
#              size = 3, stroke = 0.5,
#              position = position_jitter(width = 0.3, seed = 1)
#              )
#

p1 <- ggplot() +
  # geom_errorbar(data = lmb_sum, width = 0.15,
  #               aes(group = habitat_type,
  #                   x = tod, y = mean_ac,
  #                   ymin = mean_ac - sem,
  #                   ymax = mean_ac + sem),
  #               position = position_jitter(width = 0.2,
  #                                          seed = 1)
  # ) +
  geom_point(data = lmb_sum,
             shape = 21,
             aes(x = tod,
                 y = mean_ac,
                 fill = habitat_type),
             size = 3, stroke = 0.5, alpha = 0.5,
             position = position_jitter(width = 0.2, seed = 1)) +
  # geom_line(data = lmb_sum,
  #            # shape = 21,
  #            aes(x = tod,
  #                y = mean_ac,
  #                colour = habitat_type),
  #            # size = 3, stroke = 0.5,
  #            # position = position_jitter(width = 0.2, seed = 1)
  #           ) +
  geom_line(data = preds, aes(x = tod,
                              y = fit_t, colour = habitat_type)) +
  geom_ribbon(data = preds, aes(x = tod,
                                y = fit_t, fill = habitat_type,
                                ymin = lower,
                                ymax = upper),
              alpha = 0.2) +
  lemon::facet_rep_wrap(. ~ season, repeat.tick.labels = TRUE) +
  scale_fill_viridis_d(end = 0.85, name = "Habitat Type",
                       alpha = 0.5) +
  scale_colour_viridis_d(end = 0.85, name = "Habitat Type"
  ) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.92, 0.94)
  ) +
  labs(
    x = "Hour",
    y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )

# p1
ggsave(here("plots",
            "hour-gamm-accel",
            "gamm_hour_season_habitat_revised_no_errorbars.png"), plot = p1,
       width = 11 * 1.5, height = 8.5 * 1.5)


