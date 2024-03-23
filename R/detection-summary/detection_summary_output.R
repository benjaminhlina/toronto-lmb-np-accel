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
  library(stringr)
  library(tidyr)
}

# ---- bring in summary dataframe -----
dat <- qread(here("data-saved",
                  "cleaned-telemetry-accel-th",
                  "lmb_np_cleaned_accel_th_after_abacus.qs"))

glimpse(dat)
# look at data structure


# lmb <- dat %>%
#   filter(common_name_e == "Largemouth Bass") %>%
#   mutate(
#     season = factor(season,
#                     levels = c("Fall", "Winter", "Spring", "Summer")
#     ),
#     habitat_type = factor(habitat_type)
#   )
#
#
# np <- dat %>%
#   filter(common_name_e == "Northern Pike") %>%
#   mutate(
#     season = factor(season,
#                     levels = c("Fall", "Winter", "Spring", "Summer")
#     ),
#     habitat_type = factor(habitat_type)
#   )

summary_table <- dat %>%
  group_by(common_name_e) %>%
  arrange(common_name_e, detection_timestamp_est) %>%
  summarise(
    n = n(),
    first_det = first(detection_timestamp_est),
    last_det = last(detection_timestamp_est),
  ) %>%
  ungroup() %>%
  mutate(
    total = nrow(dat),
    perc_dec = n / total
  )

summary_table


glimpse(dat)
days_heard <- dat %>%
  group_by(common_name_e, animal_id) %>%
  arrange(common_name_e, animal_id, detection_timestamp_est) %>%
  summarise(
    number_det = n(),
    n_rec = n_distinct(name),
    first_det = first(detection_timestamp_est),
    last_det = last(detection_timestamp_est),
    n_det = n_distinct(date),
    tracking_window = as.numeric(difftime(last_det, first_det)),
    days_percent = round((n_det / tracking_window) * 100, digits = 1)
  ) %>%
  ungroup() %>%
  arrange(
    common_name_e,
    days_percent
  )


days_heard
summary(days_heard)
