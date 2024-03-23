# ---- Load Packages ----
{
  library(data.table)
  library(dplyr)
  library(here)
  library(ggplot2)
  library(ggtext)
  library(qs)
  library(purrr)
  library(patchwork)
  library(readr)
  library(stringr)
  library(tidyr)
}

# ---- bring in summary dataframe -----
dat <- qread(here("data-saved",
                  "summary-accel-doy",
                  "lmb_np_summary_accel_1h.qs")) %>%
  filter(habitat_type != is.na(habitat_type))
glimpse(dat)


dat <- dat %>%
  mutate(
    season = factor(season,
                    levels = c("Fall", "Winter", "Spring", "Summer"),
    ),
    habitat_type = factor(
      habitat_type, levels =
        c("Deep/Low SAV", "Exposed/Low SAV",
          "Mod/Dense SAV",
          "Shallow/Dense SAV",
          "Shallow/Low SAV"
        )
    )
  )



# ---- np ----

dp <- ggplot(data = dat, aes(x = day_night, y = mean_accel,
                             # fill = habitat_type
                             )) +
  geom_boxplot(width = 0.25, outlier.shape = NA) +
  lemon::facet_rep_wrap(. ~ common_name_e, repeat.tick.labels = TRUE) +
  scale_fill_viridis_d(end = 0.85, name = "Habitat Type",
                       alpha = 0.5) +
  theme_bw(
    base_size = 15
  ) +
  coord_cartesian(ylim = c(0, 2)) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = c(0.92, 0.75),
    legend.box.background = element_blank(),
    legend.background = element_blank(),
    # strip.text = element_blank(),
  ) +
  labs(x = "Diel Period",
       y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )

dp

dp_hab <- ggplot(data = dat, aes(x = day_night, y = mean_accel,
                             fill = habitat_type)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  lemon::facet_rep_wrap(. ~ common_name_e, repeat.tick.labels = TRUE) +
  scale_fill_viridis_d(end = 0.85, name = "Habitat Type",
                       alpha = 0.5) +
  theme_bw(
    base_size = 15
  ) +
  coord_cartesian(ylim = c(0, 2)) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = c(0.92, 0.75),
    legend.box.background = element_blank(),
    legend.background = element_blank(),
    strip.text = element_blank(),
  ) +
  labs(x = "Diel Period",
       y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )

dp_hab


p <- (dp +

          theme(
            # axis.text.x = element_blank(),
            axis.title.x = element_blank()
          )) / (dp_hab) +
  plot_annotation(tag_levels = "a", tag_suffix = ")")

p


ggsave(filename = here("plots",
                       "boxplot",
                       "Publication Plots",
                       paste("hab_diel_period_spp_boxplot_labelled_no_outlier_",
                             Sys.Date(), ".png", sep = "")),
       width = 14, height = 11, plot = p)
