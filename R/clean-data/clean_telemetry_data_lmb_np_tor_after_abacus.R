# ---- bring in packages ----
{
  library(dplyr)
  library(here)
  library(qs)
}
# ---- bring in cleaned data ----

det <- qread(here("data-saved",
            "cleaned-telemetry-accel-th",
            "lmb_np_cleaned_accel_th.qs"))

glimpse(det)
# ---- remove dead fish ----
det_filter <- det %>%
  filter(!(animal_id %in% c("Bass 334", "Bass 403b",
                          "Pike 434", "Pike 584",
                          "Pike 615", "Pike 646"))
  )


# ---- save data ----
qsave(det_filter, here(
  "data-saved",
  "cleaned-telemetry-accel-th",
  "lmb_np_cleaned_accel_th_after_abacus.qs"
))
