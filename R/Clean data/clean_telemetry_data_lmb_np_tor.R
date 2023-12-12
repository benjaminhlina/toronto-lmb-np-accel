# ---- Load Packages ----
{
  library(data.table)
  library(here)
  library(ggplot2)
  library(qs)
  library(sf)
  library(tidyr)
}
# ---- Bring in non summarized lmb and smb data ----

dat <- qread(here("data-raw",
                  "raw-lmb-np.qs"))
glimpse(dat)

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

