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

    ),
    day_night = factor(day_night, levels = c("Dawn", "Day",
                                             "Dusk", "Night"))
  )



dat <- dat %>%
  mutate(
    habitat_type = factor(case_when(
      habitat_type == "Deep/Low SAV" ~ "Deep un-vegetated",
      habitat_type == "Exposed/Low SAV" ~ "Exposed",
      habitat_type == "Mod/Dense SAV" ~ "Coastal vegetated",
      habitat_type == "Shallow/Dense SAV" ~ "Wetlands",
      habitat_type == "Shallow/Low SAV" ~ "Shallow un-vegetated",

    ), level = c(
      "Deep un-vegetated", "Exposed",  "Coastal vegetated",
      "Wetlands",
      "Shallow un-vegetated"


    )
    )
  )
# ---- bring in among diel letters ----

dp_letters <- read_csv(here("results",
                            "comparison-letters-final",
                            "among_diel_period_lmb_np.csv"))


dp_long <- dp_letters %>%
  pivot_longer(cols = -common_name_e,
               names_to = "day_night",
               values_to = "letters")


# ---- letter prep ----
sig_let_dp <-  tibble(
  common_name_e = dp_long$common_name_e,
  letter = dp_long$letters,
  x = dp_long$day_night,
    # rep(seq(1, 4, 1), 2),
  y =
    c(
      1.7, 1.45, 1.48, 0.9,
      0.55, 0.45, 0.63, 0.33

    )
)

# ---- np ----

dp <- ggplot() +
  geom_boxplot(data = dat, aes(x = day_night, y = mean_accel,
                               # fill = habitat_type
  ), width = 0.25, outlier.shape = NA) +
  geom_text(data = sig_let_dp, aes(x = x,
                                    y = y,
                                    label = letter),
            size = 5) +

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
# ---- bromg in letters ----
dp_hab_letters <- read_csv(here("results",
                            "comparison-letters-final",
                            "within_diel_period_habitat_compare_lmb_np.csv"))

dp_hab_letters_long <- dp_hab_letters %>%
  pivot_longer(cols = -c("common_name_e", "dp"),
               names_to = "habitat_type",
               values_to = "letters")

dp_hab_letters_long



# ---- plot -----
dp_hab <- ggplot() +
  geom_boxplot(data = dat, aes(x = day_night, y = mean_accel,
                             fill = habitat_type),
               width = 0.5, outlier.shape = NA) +
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
ggstat <- ggplot_build(dp_hab)$data

sig_let <- tibble(
  common_name_e = dp_hab_letters_long$common_name_e,
  letter = dp_hab_letters_long$letters,
  habitat_type = dp_hab_letters_long$habitat_type,
  x = dp_hab_letters_long$dp,
  #   c(
  #   0.7, 0.85, 1, 1.15, 1.3,
  #   1.7, 1.85, 2, 2.15, 2.3,
  #   2.7, 2.85, 3, 3.15, 3.3,
  #   3.7, 3.85, 4, 4.15, 4.3,
  #   0.7, 0.85, 1, 1.15, 1.3,
  #   1.7, 1.85, 2, 2.15, 2.3,
  #   2.7, 2.85, 3, 3.15, 3.3,
  #   3.7, 3.85, 4, 4.15, 4.3
  # ),
  y = ggstat[[1]]$ymax
  # max_ac$whisk_high
  #   c(
  #   0.75, 0.93, 1.1, 0.85, 1.05,
  #   0.63, 0.4, 0.45,
  #   1.33, 0.85, 1.15, 1.25, 1.1,
  #   1.93, 1.73, 1.75, 1.73, 1.92,
  #   0.4, 0.41, 0.38, 0.48, 0.37,
  #   0.3, 0.47, 0.38, 0.65, 0.27,
  #   0.5, 0.55, 0.5, 0.4, 0.61,
  #   0.55, 0.53, 0.55, 0.47, 0.53
  # )
)

dp_hab_1 <- dp_hab +
  geom_text(data = sig_let, aes(x = x,
                                y = y + 0.1,
                                label = letter,
                                group = habitat_type),
            size = 5,
            position = position_dodge(width = 0.5))
dp_hab_1


p <- (dp +

          theme(
            # axis.text.x = element_blank(),
            axis.title.x = element_blank()
          )) / (dp_hab_1) +
  plot_annotation(tag_levels = "a", tag_suffix = ")")

p


ggsave(filename = here("plots",
                       "boxplot",
                       "Publication Plots",
                       paste("hab_diel_period_spp_boxplot_labelled_no_outlier_",
                             Sys.Date(), ".png", sep = "")),
       width = 14, height = 11, plot = p)

ggsave(filename = here("plots",
                       "boxplot",
                       "Publication Plots",
                       paste("hab_diel_period_spp_boxplot_labelled_no_outlier_",
                             Sys.Date(), ".pdf", sep = "")),
       width = 14, height = 11, plot = p)
