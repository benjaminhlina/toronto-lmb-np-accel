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

# ---- bring in letters ----
within_season <- readr::read_csv(here("Results",
                                      "comparison-letters-final",
                                      "within_season_habitat_compare_lmb_np.csv"))
within_season_long <- within_season %>%
  pivot_longer(cols = -c(common_name_e, season),
               names_to = "habitat_type",
               values_to = "letters") %>%
  drop_na()

within_season_long

# ---- bring in stars ----
among_season <- readr::read_csv(here("Results",
                                     "comparison-letters-final",
                                     "among_season_habitat_compare_lmb_np.csv"))

among_season
among_season_long <- among_season %>%
  pivot_longer(cols = -c(common_name_e, season),
               names_to = "habitat_type",
               values_to = "stars") %>%
  filter(!(common_name_e %in% "Largemouth Bass" &
             season %in% "Winter" &
             habitat_type %in% c("Exposed/Low SAV",
                                 "Mod/Dense SAV"))) %>%
  mutate(
    stars = stars %>%
      str_replace_na() %>%
      str_remove(
        "NA")
  )
among_season_long
among_season_long$common_name_e
unique(dat$common_name_e)


# ----- create boxplots ----
p5 <- ggplot(data = dat,
             aes(x = habitat_type, y = mean_accel)) +
  geom_boxplot(width = 0.25, outlier.shape = NA) +
  facet_wrap(. ~ common_name_e) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "Habitat Type",
       y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )



# m <- dat %>%
#   group_split(season, habitat_type) %>%
#   map(~ ggplot2_boxplot(.x$mean_accel))
#
#
# dat %>%
#   group_split(season, habitat_type) %>%
#   map(~  max(.x$mean_accel[
#     .x$mean_accel <
#       (quantile(.x$mean_accel, probs = 0.75) + 1.5 * (quantile(.x$mean_accel, probs = 0.75) -
#                                                         quantile(.x$mean_accel, probs = 0.25)))])
#   )
# glimpse(m)
# m[[4]]
# quantile()
max_ac <- dat %>%
  group_by(season, habitat_type) %>%
  summarise(
    q1 = as.numeric(quantile(mean_accel, na.rm = TRUE, names = TRUE)[2]),
    q3 = as.numeric(quantile(mean_accel, na.rm = TRUE)[4]),
    iqr = q3 - q1,
    whisk_high = q3 + (1.5 * iqr),
    whisk_low = (1.5 * iqr) - q1
  ) %>%
  ungroup() %>%
  arrange(season, habitat_type) %>%
  filter(!(season %in% "Winter" &
             habitat_type %in% c("Exposed/Low SAV",
                                 "Mod/Dense SAV")))

max_ac

dput(unique(dat$habitat_type))

sig_let <- tibble(
  common_name_e = within_season_long$common_name_e,
  letter = within_season_long$letters,
  x = c(
    0.7, 0.85, 1, 1.15, 1.3,
    1.75, 2, 2.25,
    2.7, 2.85, 3, 3.15, 3.3,
    3.7, 3.85, 4, 4.15, 4.3,
    0.7, 0.85, 1, 1.15, 1.3,
    1.7, 1.85, 2, 2.15, 2.3,
    2.7, 2.85, 3, 3.15, 3.3,
    3.7, 3.85, 4, 4.15, 4.3
  ),
  y = c(
    0.75, 0.93, 1.1, 0.85, 1.05,
    0.63, 0.4, 0.45,
    1.33, 0.85, 1.15, 1.25, 1.1,
    1.93, 1.73, 1.75, 1.73, 1.92,
    0.4, 0.41, 0.38, 0.48, 0.37,
    0.3, 0.47, 0.38, 0.65, 0.27,
    0.5, 0.55, 0.5, 0.4, 0.61,
    0.55, 0.53, 0.55, 0.47, 0.53
  )
)

sig_stars <- tibble(
  common_name_e = among_season_long$common_name_e,
  stars = among_season_long$stars,
  x = c(
    0.7, 0.85, 1, 1.15, 1.3,
    1.75, 2, 2.25,
    2.7, 2.85, 3, 3.15, 3.3,
    3.7, 3.85, 4, 4.15, 4.3,
    0.7, 0.85, 1, 1.15, 1.3,
    1.7, 1.85, 2, 2.15, 2.3,
    2.7, 2.85, 3, 3.15, 3.3,
    3.7, 3.85, 4, 4.15, 4.3
  ),
  y = c(0.75, 0.93, 1.1, 0.85, 1.05,
        0.63, 0.4, 0.45,
        1.33, 0.85, 1.15, 1.25, 1.1,
        1.93, 1.73, 1.75, 1.73, 1.92,
        0.4, 0.41, 0.38, 0.48, 0.37,
        0.3, 0.47, 0.38, 0.65, 0.27,
        0.5, 0.55, 0.5, 0.4, 0.61,
        0.55, 0.53, 0.55, 0.47, 0.53)
)



# dat %>%
#   group_split(season, habitat_type) %>%
#   map(~ boxplot.stats(.x$mean_accel)$stats[c(1, 5)])

p12 <- ggplot() +
  geom_boxplot(data = dat,
               aes(x = season, y = mean_accel,
                   fill = habitat_type), outlier.shape = NA) +
  geom_text(data = sig_let, aes(x = x,
                                y = y,
                                label = letter),
            size = 5) +

  geom_text(data = sig_stars, aes(x = x,
                                  y = y + 0.05,
                                  label = stars),
            size = 5) +
  facet_wrap(. ~ common_name_e) +
  scale_fill_viridis_d(end = 0.85, name = "Habitat Type",
                       alpha = 0.5) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.92, 0.75),
    legend.box.background = element_blank(),
    legend.background = element_blank(),
    strip.text = element_blank(),
  ) +
  labs(x = "Season",
       y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )

# p12
# p12$data
# glimpse(p12)
# p12

p13 <- (p5 +
          coord_cartesian(ylim = c(0, 2)) +
          theme(
            # axis.text.x = element_blank(),
            axis.title.x = element_blank()
          )) / (p12 +  coord_cartesian(ylim = c(0, 2))) +
  plot_annotation(tag_levels = "a", tag_suffix = ")")

# p13
ggsave(filename = here("plots",
                       "boxplot",
                       "hab_season_x_spp_boxplot_labled_no_outlier.png"),
       width = 14, height = 11, plot = p13)

