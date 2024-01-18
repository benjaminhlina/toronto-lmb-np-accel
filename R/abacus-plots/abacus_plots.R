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

dat <- qread(here("data-saved",
                  "cleaned-telemetry-accel-th",
                  "lmb_np_cleaned_accel_th.qs"))
glimpse(dat)


# ---- change station name into factor ----

dat[, station_no := factor(station_no)]
dat[, station := factor(station)]
unique(dat$station)
glimpse(dat)

unique(is.na(dat$station_no))

dat <- dat %>%
  mutate(
    change_rec = if_else(station_no != lag(station_no), true = "1", false = "0")
  )

# ---- use mapp to loop through and export abacus plots -----

dat %>%
  split(.$animal_id) %>%
  map(~ ggsave(
    filename = here("plots",
                    "abacus-plots",
                    "regular",
                    paste0(unique(.$animal_id),'.png')),
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
        title = paste("Fish ID:", unique(.$animal_id), sep = " "),
        x = "Date",
        y = "Station")
  )
  )
# ---- jittered ----
dat %>%
  split(.$animal_id) %>%
  map(~ ggsave(
    filename = here("plots",
                    "abacus-plots",
                    "jittered",
                    paste0(unique(.$animal_id),'_jitter.png')),
    height = 7,
    width = 11,
    plot =
      ggplot(data = ., aes(x = detection_timestamp_est, y = station)) +
      geom_line(aes(group = 1, colour = change_rec)) + # we can remove line if it's distracting
      geom_jitter(aes(fill = station), shape = 21, size = 3,
                  alpha = 0.50, height = 0.1) +
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
        title = paste("Fish ID:", unique(.$animal_id), sep = " "),
        x = "Date",
        y = "Station")
  )
  )
