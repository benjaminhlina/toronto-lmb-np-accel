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
                  "raw-lmb-np.qs"))
glimpse(dat)


# ---- bring in tag metadata -----

fish_tag <- read_csv(here("data-raw",
                          "fish-tagging-data",
                          "Fish Data Master List_Nov2022.csv")) %>%
  janitor::clean_names() %>%
  mutate(
    sn = as.character(sn)
  )

glimpse(fish_tag)

fish_tag <- setDT(fish_tag)

# ---- bring in habitat data ----

hab_rec <- read_csv(here("data-raw",
                         "toronto-harbour-habitat-data",
                         "TH_Telemetry_ReceiverGroupHabitatCluster_July2019.csv")) %>%
  janitor::clean_names()

glimpse(hab_rec)

hab_rec <- setDT(hab_rec)
# ---- bring receiver codes
rec_codes <- read_csv(here("data-raw",
                         "toronto-harbour-habitat-data",
                         "TH_rec_codes.csv")) %>%
  janitor::clean_names()

glimpse(rec_codes)

rec_codes <- setDT(rec_codes)

# ---- merge habitat data and rec-codes ----
setkey(hab_rec, site)
setkey(rec_codes, station_group)

hab_rec <- hab_rec[rec_codes, ]

glimpse(hab_rec)
# ---- calculate day of year and month abbreviation ----
# data.table is far more powerfull than dplyr
dat[, c("doy", "month_abb") := list(
  yday(detection_timestamp_EST),
  month(detection_timestamp_EST,
        label = TRUE, abbr = TRUE)
)
]


# ---- filter out just accell data ----
# first set key which is how we filter in data.table

setkey(dat, sensorv.type)

# create sybset of just acceleration data
dat_accel <- dat[sensorv.type %in% c("Accel")]

# create just accel id detected
accel_id <- dat_accel %>%
  distinct(transmitter_id)

# ---- filter metadata ----

th_accel <- fish_tag %>%
  filter(printed_id %in% accel_id$transmitter_id) %>%
  dplyr::select(sn:transmitter_model, id_or_p_sensor_id,
                pit_code, total_length, weight, sex,
                date_tagged, location) %>%
  rename(
    sx = sex,
    wt = weight
  )

glimpse(th_accel)

# look at what years to confirm we have the right years
dat_accel %>%
  distinct(year)

# look at what projects to confirm we have the right project
dat_accel %>%
  distinct(glatos_project_receiver)


# look at accell data overall
glimpse(dat_accel)
glimpse(th_accel)

# convert printed_id to character for merging

th_accel[, printed_id := as.character(printed_id)]

# ---- merge detection data with metadata

setkey(dat_accel, transmitter_id)
setkey(th_accel, printed_id)
dat_accel <- dat_accel[th_accel, ]

glimpse(dat_accel)

# ---- merge habitat data ----
dat_accel %>%
  distinct(station_no, station, glatos_array)

glimpse(hab_rec)
glimpse(hab_rec)

setkey(dat_accel, glatos_array)
setkey(hab_rec, code)




# ---- merge tag slope and intercept ----


# ----   convert acceleration data to  m/s2 ----



# ---- create summary dataframe of doy of year with sem and sd ect

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

