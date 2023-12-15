# ---- Load Packages ----
{
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(here)
  library(lubridate)
  library(purrr)
  library(qs)
  library(readr)
}
# ---- Bring in non summarized lmb and smb data ----

dat <- qread(here("data-raw",
                  "raw-lmb-np.qs"))
glimpse(dat)


# ---- filter out just accell data ----
# first set key which is how we filter in data.table

setkey(dat, sensorv.type)

# create sybset of just acceleration data
dat_accel <- dat[sensorv.type %in% c("Accel")]

dat_accel[, station_no := factor(station_no)]
unique(dat_accel$station)
glimpse(dat_accel)
# ---- use mapp to loop through and export abacus plots -----
dat_accel %>%
  split(.$animal_id) %>%
  map(~ ggsave(
    filename = here("plots",
                    "abacus-plots",
                    paste0(unique(.$animal_id),'.png')),
    height = 7,
    width = 11,
    plot =
      ggplot(data = ., aes(x = detection_timestamp_EST, y = station)) +
      geom_point(aes(fill = station), shape = 21, size = 3, alpha = 0.5) +
      geom_line(aes(group = 1)) + # we can remove line if it's distracting
      scale_fill_viridis_d(begin = 0.25, end = 0.75,
                           option = "D", name = "Station") +
      theme_bw(
        base_size = 15
      ) +
      theme(panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = paste("Fish ID:", unique(.$animal_id), sep = " "),
        x = "Date",
        y = "Station")
  )
  )

