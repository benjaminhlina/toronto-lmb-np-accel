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
    season = factor(season, levels = c("Fall",
                                       "Winter",
                                       "Spring",
                                       "Summer"))
  )

# ---- create summary dataframes ----

dat_sum_hab <- dat %>%
  group_by(habitat_type, common_name_e) %>%
  summarise(
    accel = mean(mean_accel),
    sem = sd(mean_accel) / sqrt(n())
  ) %>%
  ungroup()

dat_sum_hab


dat_sum_hab_season <- dat %>%
  group_by(common_name_e, season, habitat_type) %>%
  summarise(
    accel = mean(mean_accel),
    sem = sd(mean_accel) / sqrt(n())
  ) %>%
  ungroup()

# ----- create violins ----

# violin regardless of seasons
p <- ggplot(data = dat,
            aes(x = habitat_type, y = mean_accel)) +
  geom_violin(width = 0.25) +
  geom_errorbar(data = dat_sum_hab,
                aes(x = habitat_type, y = accel,
                    ymax = accel + sem,
                    ymin = accel - sem), width = 0.05,
  ) +
  geom_point(data = dat_sum_hab, aes(
    x = habitat_type, y = accel,
  ), size = 3) +
  facet_wrap(. ~ common_name_e) +
  scale_y_continuous(breaks = seq(0, 3.5, 0.5)) +
  coord_cartesian(ylim = c(0, 3.75)) +
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
                       "violin",
                       "habitat_type_spp_violin.png"),
       width = 11, height = 8.5, plot = p)
# ---- violin plot with season invovled ----
glimpse(dat_sum_hab_season)

p1 <- ggplot(data = dat,
             aes(x = season, y = mean_accel,
                 fill = habitat_type)) +
  geom_violin(width = 1,
              position = position_dodge(width = 0.8)
              ) +
  geom_errorbar(data = dat_sum_hab_season,
                aes(x = season , y = accel,
                    ymax = accel + sem,
                    ymin = accel - sem,
                    group = habitat_type), width = 0.05,
                position = position_dodge(width = 0.8)) +
  geom_point(data = dat_sum_hab_season, aes(
    x = season, y = accel,
    group = habitat_type), size = 2,
    position = position_dodge(width = 0.8)) +
  facet_wrap(. ~ common_name_e) +
  scale_fill_viridis_d(end = 0.85, name = "Habitat Type",
                       alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, 3.5, 0.5)) +
  coord_cartesian(ylim = c(0, 4)) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.09, 0.85),
    legend.box.background = element_blank(),
    legend.background = element_blank()
  ) +
  labs(x = "Season",
       y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
  )

# p1

# ggsave(filename = here("plots",
#                        "violin",
#                        "habitat_type_season_spp_violin.png"),
#        width = 11, height = 8.5, plot = p1)
#
# p2 <- p +
#   theme(
#     axis.title.x = element_blank(),
#     axis.text.x = element_blank(),
#     axis.ticks = element_blank()
#   )
#
p3 <- p1 +
  theme(
    strip.text = element_blank(),
  )

p4 <- p / p3 +
  plot_annotation(tag_levels = "a", tag_suffix = ")")


ggsave(filename = here("plots",
                       "violin",
                       "habitat_type_and_season_spp_violin.png"),
       width = 13, height = 14, plot = p4)


