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

# ---- calculate daily metrics for accel ----
