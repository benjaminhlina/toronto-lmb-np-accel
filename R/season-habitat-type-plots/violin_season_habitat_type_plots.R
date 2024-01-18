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


# ----- create violins ----

# violin regardless of seasons
p <- ggplot(data = dat,
            aes(x = habitat_type, y = mean_accel)) +
  geom_violin(width = 0.25) +
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

# p

ggsave(filename = here("plots",
                       "violin",
                       "habitat_type_spp_violin.png"),
       width = 11, height = 8.5, plot = p)

p1 <- ggplot(data = dat,
             aes(x = habitat_type, y = mean_accel,
                 fill = season)) +
  geom_violin() +
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

# p1

ggsave(filename = here("plots",
                       "violin",
                       "habitat_type_season_spp_violin.png"),
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
                       "violin",
                       "habitat_type_and_season_spp_violin.png"),
       width = 12, height = 9.5, plot = p4)


