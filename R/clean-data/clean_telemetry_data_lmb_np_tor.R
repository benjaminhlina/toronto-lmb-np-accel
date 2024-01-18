# ---- Load Packages ----
{
  library(data.table)
  library(dplyr)
  library(here)
  library(lubridate)
  library(qs)
  library(readr)
}
# ---- Bring in non summarized lmb and smb data ----

dat <- qread(here("data-raw",
                  "detection-data",
                  "raw-lmb-np-correct.qs"))
glimpse(dat)



# ---- bring in tag metadata -----

# fish_tag <- read_csv(here("data-raw",
#                           "fish-tagging-data",
#                           "Fish Data Master List_Nov2022.csv")) %>%
#   janitor::clean_names() %>%
#   mutate(
#     sn = as.character(sn)
#   )
#
# glimpse(fish_tag)
#
# fish_tag <- setDT(fish_tag)

# ---- bring in habitat data ----

hab_rec<- read_csv(here("data-raw",
                         "toronto-harbour-habitat-data",
                         "detection_station_id_detected.csv")) %>%
  janitor::clean_names()

glimpse(hab_rec)

hab_rec <- setDT(hab_rec)

# ---- bring in tagg metadata slopes from innovseas ----
# bring in first set
inno_17 <- read_csv(here("data-raw",
                         "transmitter-info",
                         "17456_toronto_&_region_conservation_clean.csv")) %>%
  janitor::clean_names()

glimpse(inno_17)

inno_17 <- setDT(inno_17)
# bring in second set
inno_19 <- read_csv(here("data-raw",
                         "transmitter-info",
                         "19671_toronto_&_region_conservation_clean.csv")) %>%
  janitor::clean_names()

glimpse(inno_19)

inno_19 <- setDT(inno_19)

# combine them
innovasea_combine <- bind_rows(inno_17, inno_19)
innovasea_combine
# ---- bring in season timing dataframe ----
seasons <- read_csv(here("data-raw",
                         "season-info",
                         "TH_season_restdates.csv")) %>%
  janitor::clean_names()

glimpse(seasons)
seasons <- setDT(seasons)
# ---- calculate day of year and month abbreviation ----
# data.table is far more powerfull than dplyr

dat[, detection_timestamp_est := with_tz(detection_timestamp_utc, "EST")]
dat[, c("date", "doy", "month", "month_abb", "year") := list(
  floor_date(detection_timestamp_est, unit = "day"),
  yday(detection_timestamp_est),
  month(detection_timestamp_est),
  month(detection_timestamp_est,
        label = TRUE, abbr = TRUE),
  year(detection_timestamp_est)
)
]

# ---- filter out just accell data ----
# # first set key which is how we filter in data.table
#
# setkey(dat, sensor_type)
#
# # create sybset of just acceleration data
# dat_accel <- dat[sensor_type %in% c("Accel")]
#
# # create just accel id detected
# accel_id <- dat_accel %>%
#   distinct(transmitter_id)
#
# # ---- filter metadata ----
#
# th_accel <- fish_tag %>%
#   filter(printed_id %in% accel_id$transmitter_id) %>%
#   dplyr::select(sn:transmitter_model, id_or_p_sensor_id,
#                 pit_code, total_length, weight, sex,
#                 date_tagged, location) %>%
#   rename(
#     sx = sex,
#     wt = weight
#   )
#
# glimpse(th_accel)
#
# # look at what years to confirm we have the right years
# dat_accel %>%
#   distinct(year)

# look at what projects to confirm we have the right project
dat %>%
  distinct(glatos_project_receiver)
dat_accel <- dat[glatos_project_receiver %in% "THFHA"]

rm(dat)
gc()
# look at accell data overall
glimpse(dat_accel)




# ---- merge habitat we wneed to fix this  ----
station_id <- dat_accel %>%
  distinct(station_no, station, glatos_array) %>%
  arrange(glatos_array)
station_id
# openxlsx::write.xlsx(station_id, here("data-raw",
#                           "toronto-harbour-habitat-data",
#                           "detection_station_id_detected.xlsx"))
glimpse(hab_rec)
# glimpse(dat_accel)
# hab_rec[, station_no := as.character(station_no)]
#
setkey(dat_accel, glatos_array, station_no, station)
setkey(hab_rec, glatos_array, station_no, station)
#
#
dat_accel <- dat_accel[hab_rec, ]

# ---- Added in seasonal time points -----

glimpse(dat_accel)

seasons <- seasons %>%
  filter(year %in% dat_accel$year) %>%
  select(-c("jd", "year"))

glimpse(dat_accel)
glimpse(seasons)

# setkey(dat_accel, date)
# setkey(seasons, ymd)

dat_accel <- dat_accel %>%
  left_join(seasons, by = c("date" = "ymd", "month" = "month"))



# ---- merge tag slope and intercept ----
glimpse(dat_accel)
glimpse(innovasea_combine)

# select columns we want to merge
innovasea_combine <- innovasea_combine %>%
  select(serial_no, id_code, vue_tag_id_freq_space_id, freq_k_hz, range,
         units, slope, intercept, transmit_ratio) %>%
  rename(
    sensor_range = range,
    sensor_units = units
  )
glimpse(innovasea_combine)
glimpse(dat_accel)



# then merge

dat_accel <- dat_accel %>%
  left_join(innovasea_combine, by = c("tag_serial_number" = "serial_no",
                                      "transmitter_id" = "id_code"))

# ----   convert acceleration data to  m/s2 ----

dat_accel <- dat_accel %>%
  mutate(
    convert_accel = ((slope * sensor_value) + intercept)
  )

glimpse(dat_accel)

# class(dat_accel)

# ----- export cleaned dataframe for futher analysis analysis ----

qsave(dat_accel, here("data-saved",
                      "cleaned-telemetry-accel-th",
                      "lmb_np_cleaned_accel_th.qs"))

