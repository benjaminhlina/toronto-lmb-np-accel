# ---- Load Packages ----
{
  library(broom.mixed)
  library(data.table)
  library(dplyr)
  library(fitdistrplus)
  library(here)
  library(ggplot2)
  library(gratia)
  library(itsadug)
  library(lubridate)
  library(mgcv)
  library(qs)
  library(purrr)
  library(readr)
}

# ---- bring in summary dataframe -----
dat <- qread(here("data-saved",
                  "summary-accel-doy",
                  "lmb_np_summary_accel_doy.qs"))

glimpse(dat)
# look at data structure
# ---- split by species ----
dat_sp <- dat %>%
  split(.$common_name_e)

# ---- look at distibution for each species ----
dat_sp %>%
  map(~ descdist(.x$mean_accel))

# ---- plot distirubtion for each species ----
dat_sp %>%
  map(~ ggplot(data = .x, aes(x = mean_accel)) +
        geom_histogram()
  )
# both look ver gamma distributed

fit_gamma <- dat_sp %>%
  map(~ fitdist(.x$mean_accel, dist = "gamma", method = "mme")
  )
fit_norm <- dat_sp %>%
  map(~ fitdist(.x$mean_accel, dist = "norm", method = "mle")
  )

# plot distribution against gamma distribution
fit_gamma %>%
  map(~ plot(.x))

# plot distribution against normal distribution
fit_norm %>%
  map(~ plot(.x))

# gamma distribution makes the most sense
glimpse(dat)

# ---- prep data for gamm ----
dat_sp <- dat_sp %>%
  map(~
        .x %>%
        mutate(
          habitat_type = factor(habitat_type),
          year = factor(year),
          animal_id = factor(animal_id)
        )

  )

lmb <- dat_sp$`Largemouth Bass`
np <- dat_sp$`Northern Pike`

glimpse(lmb)

ggplot(data = lmb %>%
         filter(habitat_type != is.na(habitat_type)),
       aes(x = habitat_type, y = mean_accel,
                       fill = season)) +
  geom_boxplot()
ggplot(data = lmb %>%
         filter(habitat_type != is.na(habitat_type)),
       aes(x = habitat_type, y = mean_accel,
                       # fill = season
           )) +
  geom_boxplot()

lmb_sum <-
# need to create start and to from colum for autocorrelation
# ----- Create DOY Model ----

# cant' do this all together need to do seperate. LMB bang on model NP not so much
#
lmb_m <- bam(
  mean_accel ~  habitat_type +
    s(doy, by = habitat_type, bs = "cc", k = 13) +
    s(year, bs = "re") +
    s(animal_id, bs = "re"),
  data = lmb,
  select = TRUE,
  method = "fREML",
  family = Gamma(link = "log")
)



appraise(lmb_m)

draw(lmb_m)


#
# dat_1 <- lmb %>%
#   mutate(
#     year = "0",
#     animal_id = "a"
#   )
#
# predict.gam()
#
# pred <- augment(lmb_m, newdata = dat_1, sexclude = c("s(year)", "s(animal_id)"))
#
# pred <- pred %>%
#   mutate(
#     lower = exp(1) ^ (.fitted - 1.96 * .se.fit),
#     upper = exp(1) ^ (.fitted + 1.96 * .se.fit),
#     .fitted = exp(1) ^ .fitted
#   )
#
#
# ggplot() +
#   geom_line(data = pred, aes(x = doy, y = .fitted, colour = habitat_type)) +
#   geom_ribbon(data = pred, aes(x = doy, y = .fitted,
#                                ymin = lower, ymax = upper,
#                                fill = habitat_type),
#               alpha = 0.2)
#
#
# glimpse(lmb)
ggplot(data = lmb, aes(x = season, y = mean_accel,
                       fill = habitat_type)) +
  geom_boxplot()

