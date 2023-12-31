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


# # remove sensor_values that are lesthan or equal to 8
# # as the fish is resting.
# dat_accel_1 <- dat_accel[sensor_val > 8]

summary(dat_accel$sensor_val)
# look at the n of less then 8.
nrow(dat_accel) - nrow(dat_accel_1)
# if we filter less than 8 we loose 197,439 detections probably don't want
# to do this...ADC values > 8 in lake trout were considered in active
# by Cruz-font et al. 2016

# ---- add in 2 hr time bin ----
glimpse(dat_accel)

dat_accel <- dat_accel[, c("time_bin_1h", "time_bin_2h") := list(
  floor_date(detection_timestamp_EST, unit = "1 h"),
  floor_date(detection_timestamp_EST, unit = "2 h")
)
]

glimpse(dat_accel)
# ---- create summarized dataframe based on 1 and 2 -----
accel_sum_1h <- dat_accel[, .(
  n = (.N), # number of dets heard per day
  n_det = uniqueN(station_no), # unique # of receviers heard on in DOY
  mean_accel = mean(convert_accel),
  sd_accel = sd(convert_accel),
  sem_sensor = sd(convert_accel) / sqrt((.N))
),
keyby =
  .(spp, animal_id, sx, length, wt, time_bin_1h,
    doy, day, week, month, month_abb, season, year, habitat_type, cluster)

]

glimpse(accel_sum_1h)

# 2 hours
accel_sum_2h <- dat_accel[, .(
  n = (.N), # number of dets heard per day
  n_det = uniqueN(station_no), # unique # of receviers heard on in DOY
  mean_accel = mean(convert_accel),
  sd_accel = sd(convert_accel),
  sem_sensor = sd(convert_accel) / sqrt((.N))
),
keyby =
  .(spp, animal_id, sx, length, wt, time_bin_2h,
    doy, day, week, month, month_abb, season, year, habitat_type, cluster)

]

glimpse(accel_sum_2h)
# ----- export summary dataframe for GAMM analysis ----

qsave(accel_sum_1h, here("data-saved",
                      "summary-accel-doy",
                      "lmb_np_summary_accel_1h.qs"))
qsave(accel_sum_2h, here("data-saved",
                      "summary-accel-doy",
                      "lmb_np_summary_accel_2h.qs"))

