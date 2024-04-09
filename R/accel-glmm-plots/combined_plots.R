{
  library(here)
  library(ggplot2)
  library(qs)
  library(patchwork)
}


lmb <- qread(here("data-saved",
                  "predicted-plots",
                  "lmb_season_hab_dp_plot.qs"))

lmb

np <- qread(here("data-saved",
                        "predicted-plots",
                        "np_season_hab_dp_plot.qs"))


p <- (lmb +
        theme(
          legend.position = "inside",
          legend.position.inside = c(0.9, 0.87)
        ) +
        labs(x = "")) /
  (np +
     theme(
       legend.position = "none"
     ) +
     labs(x = "Diel Period")) +
  plot_annotation(tag_levels = "a", tag_suffix = ")")

# p


ggsave(here("plots",
            "predicted-results",
            "combined",
            "combined_plot.png"),
       height = 16, width = 12, plot = p)
