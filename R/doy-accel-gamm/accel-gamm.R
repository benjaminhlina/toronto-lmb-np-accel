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
                  "lmb_np_summary_accel_doy.qs"))

glimpse(dat)


# ---- look at distrubtion ----

ggplot(data = dat, aes(x = mean_accel)) +
  geom_histogram()
