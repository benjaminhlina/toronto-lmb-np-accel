# ---- Load Packages ----
{
  library(broom.mixed)
  library(data.table)
  library(dplyr)
  library(DHARMa)
  library(emmeans)
  library(fitdistrplus)
  library(here)
  library(ggplot2)
  library(glmmTMB)
  library(multcomp)
  library(qs)
  library(purrr)
}

# ---- bring in summary dataframe -----
dat <- qread(here("data-saved",
                  "summary-accel-doy",
                  "lmb_np_summary_accel_1h.qs"))

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
# fit_norm <- dat_sp %>%
#   map(~ fitdist(.x$mean_accel, dist = "norm", method = "mle")
#   )

# plot distribution against gamma distribution
fit_gamma %>%
  map(~ plot(.x))

# plot distribution against normal distribution
# fit_norm %>%
#   map(~ plot(.x))

# gamma distribution makes the most sense
glimpse(dat)

# ---- prep data for gamm ----
dat_sp <- dat_sp %>%
  map(~
        .x %>%
        mutate(
          habitat_type = factor(habitat_type),
          year = factor(year),
          animal_id = factor(animal_id),
          # season = factor(season, levels = c("Winter",
          #                                     "Spring",
          #                                     "Summer",
          #                                     "Fall"))
        )

  )
