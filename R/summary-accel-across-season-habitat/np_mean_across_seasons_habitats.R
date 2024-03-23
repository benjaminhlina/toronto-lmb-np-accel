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



movement_summary <- dat %>%
  group_by(common_name_e, season, habitat_type) %>%
  summarise(
    accel = round(mean(mean_accel), digits = 2),
    sem = round(sd(mean_accel) / sqrt(n()), digits = 3)
  ) %>%
  ungroup()

openxlsx::write.xlsx(movement_summary, here("results",
                                            "summary-means",
                                            "mean_accel_season_habitat.xlsx"))



movement_summary_hab <- dat %>%
  group_by(common_name_e, habitat_type) %>%
  summarise(
    accel = round(mean(mean_accel), digits = 2),
    sem = round(sd(mean_accel) / sqrt(n()), digits = 3)
  ) %>%
  ungroup()

openxlsx::write.xlsx(movement_summary_hab, here("results",
                                            "summary-means",
                                            "mean_accel_habitat.xlsx"))
