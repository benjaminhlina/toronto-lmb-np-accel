# ---- Load Packages ----
{
  library(data.table)
  library(dplyr)
  library(here)
  library(lubridate)
  library(qs)
  library(readr)
}

# ---- bring in cleaned data ----

dat_accel  <- qread(here("data-saved",
                         "cleaned-telemetry-accel-th",
                         "lmb_np_cleaned_accel_th.qs"))

glimpse(dat_accel)

# remove sensor_values that are lesthan or equal to 8
# as the fish is resting.
# look at the n of less then 8.
# ---- create summarized dataframe based on DOY -----
accel_sum <- dat_accel[, .(
  n = (.N), # number of dets heard per day
  n_det = uniqueN(station_no), # unique # of receviers heard on in DOY
  mean_accel = mean(Sensor.Val),
  sd_accel = sd(Sensor.Val),
  sem_sensor = sd(Sensor.Val) / sqrt((.N))
),
keyby =
  .(spp, animal_id, sex, length, weight,
    doy, month, month_abb, season, year)

]

# ----- export summary dataframe for GAMM analysis ----

qsave(accel_sum, here("data-saved",
                      "summary-accel-doy",
                      "lmb_np_summary_accel_doy.qs"))

