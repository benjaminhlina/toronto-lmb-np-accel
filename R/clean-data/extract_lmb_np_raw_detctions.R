# ---- Load Packages ----
{
  library(data.table)
  library(here)
  library(qs)
}

# --- bring in files ----

det_loc <- fread(here("data-raw",
                      "detection-data-mlp",
                      "THFHA_detectionsWithLocs_20230926_140917.csv"))

# ---- subset out largemouth and np ----
accel <- det_loc[common_name_e %in% c("Largemouth Bass", "Northern Pike")]

# --- remove big dataframe to clear up RAM ----
# rm(det_loc)
gc()

# ---- filter out accel data ----
dplyr::glimpse(accel)
unique(accel$tag_model)
unique(accel$common_name_e)
gc()
accel_lmb_np <- accel[tag_model %in% "V13A-1x"]

gc()
# ---- view accel data and save ----
dplyr::glimpse(accel_lmb_np)

qsave(accel_lmb_np, here("data-raw",
                         "detection-data",
                         "raw-lmb-np-correct.qs"))
qsave(det_loc, here("data-raw",
                         "detection-data",
                         "raw-to-det.qs"))
