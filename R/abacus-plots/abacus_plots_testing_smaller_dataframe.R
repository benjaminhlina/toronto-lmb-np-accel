# ---- load packages ----

{
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(here)
  library(qs)
  library(purrr)
}

# ---- bring in data -----

det <- qread(here("data-raw",
                  "detection-data",
                  "raw-lmb-np.qs"))
glimpse(det)

# ---- rename ----

dat <- det %>%
  rename(
    detection_timestamp_est = detection_timestamp_EST,
    sensor_val = Sensor.Val,
    sensor_type = sensorv.type,
    id_name = id.name
  )

unique(det$sensor_type)
# ---- remove everything that isn't accel data ----
dat <- dat %>%
  filter(sensor_type %in% c("Accel"))


# glimpse(dat)
#
# dat %>%
#   distinct(animal_id)


# ---- change station name into factor ----

dat[, station_no := factor(station_no)]
dat[, station := factor(station)]
unique(dat$station)
glimpse(dat)

unique(is.na(dat$station_no))

dat <- dat %>%
  mutate(
    change_rec = if_else(station != lag(station), true = "1", false = "0")
  )

# ---- use mapp to loop through and export abacus plots -----

dat %>%
  split(.$animal_id) %>%
  map(~ ggsave(
    filename = here("plots",
                    "abacus-plots",
                    "regular",
                    paste0(unique(.$animal_id), '_a.png')),
    height = 7,
    width = 11,
    plot =
      ggplot(data = ., aes(x = detection_timestamp_est, y = station)) +
      geom_line(aes(group = 1, colour = change_rec)) + # we can remove line if it's distracting
      geom_point(aes(fill = station), shape = 21, size = 3,
                 alpha = 0.50) +
      scale_fill_viridis_d(begin = 0.25, end = 0.75,
                           option = "D", name = "Station") +
      scale_colour_manual(name = "Changed Receiver",
                          values = c("black", "red")
      ) +
      theme_bw(
        base_size = 15
      ) +
      theme(panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = paste("Fish ID:", unique(.$animal_id), "A", sep = " "),
        x = "Date",
        y = "Station")
  )
  )
