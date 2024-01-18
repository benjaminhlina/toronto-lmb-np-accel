# ---- Load Packages ----
{
  library(data.table)
  library(dplyr)
  library(here)
  library(ggplot2)
  library(ggtext)
  library(qs)
  library(patchwork)
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
                    levels = c("Fall", "Winter", "Spring", "Summer")
    )
  )

# ----- create boxplots ----

# boxplot regardless of seasons
p <- ggplot(data = dat,
            aes(x = habitat_type, y = mean_accel)) +
  geom_boxplot(width = 0.25) +
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

p

ggsave(filename = here("plots",
                       "boxplot",
                       "habitat_type_spp_boxplot.png"),
       width = 11, height = 8.5, plot = p)

p1 <- ggplot(data = dat,
             aes(x = habitat_type, y = mean_accel,
                 fill = season)) +
  geom_boxplot() +
  facet_wrap(. ~ common_name_e) +
  scale_fill_viridis_d(end = 0.85, name = "Season",
                       alpha = 0.5) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.15, 0.82),
    legend.box.background = element_blank(),
    legend.background = element_blank()
  ) +
  labs(x = "Habitat Type",
       y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )

p1

ggsave(filename = here("plots",
                       "boxplot",
                       "habitat_type_season_spp_boxplot.png"),
       width = 11, height = 8.5, plot = p1)

p2 <- p +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
  )

p3 <- p1 +
  theme(
    strip.text = element_blank(),
  )

p4 <- p2 / p3

ggsave(filename = here("plots",
                       "boxplot",
                       "habitat_type_and_season_spp_boxplot.png"),
       width = 12, height = 9.5, plot = p4)


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


p6 <- ggplot(data = dat,
             aes(x = habitat_type, y = mean_accel,
                 fill = season)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(. ~ common_name_e) +
  scale_fill_viridis_d(end = 0.85, name = "Season",
                       alpha = 0.5) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.95, 0.75),
    legend.box.background = element_blank(),
    legend.background = element_blank(),
    strip.text = element_blank(),
  ) +
  labs(x = "Habitat Type",
       y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )
# p6

p7 <- p5 / p6

ggsave(filename = here("plots",
                       "boxplot",
                       "habitat_type_and_season_spp_boxplot_wo_outliers.png"),
       width = 12, height = 9.5, plot = p7)
# ----- morgan suggestions ----
p8 <- ggplot(data = dat,
             aes(x = habitat_type, y = mean_accel)) +
  geom_boxplot(width = 0.25) +
  facet_wrap(. ~ common_name_e) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  ) +
  # coord_cartesian(ylim = c(0, 2)) +
  labs(x = "Habitat Type",
       y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )
p8


p1

p9 <- p8 / (p1 +
              theme(
                strip.text = element_blank()
              )
)

ggsave(filename = here("plots",
                       "boxplot",
                       "habitat_type_and_season_spp_boxplot_mlp_suggest.png"),
       width = 14, height = 11, plot = p9)
