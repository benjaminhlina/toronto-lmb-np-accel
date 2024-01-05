# ---- Load Packages ----
{
  library(broom.mixed)
  library(data.table)
  library(dplyr)
  library(DHARMa)
  library(emmeans)
  library(fitdistrplus)
  library(here)
  library(ggplot2)
  library(glmmTMB)
  library(multcomp)
  library(qs)
  library(purrr)
}

# ---- bring in summary dataframe -----
dat <- qread(here("data-saved",
                  "summary-accel-doy",
                  "lmb_np_summary_accel_1h.qs"))

glimpse(dat)
# look at data structure
# ---- split by species ----
dat_sp <- dat %>%
  split(.$spp)

# ---- look at distibution for each species ----
dat_sp %>%
  map(~ descdist(.x$mean_accel))

# ---- plot distirubtion for each species ----
dat_sp %>%
  map(~ ggplot(data = .x, aes(x = mean_accel)) +
        geom_histogram()
  )
# both look ver gamma distributed

fit_gamma <- dat_sp %>%
  map(~ fitdist(.x$mean_accel, dist = "gamma", method = "mme")
  )
fit_norm <- dat_sp %>%
  map(~ fitdist(.x$mean_accel, dist = "norm", method = "mle")
  )

# plot distribution against gamma distribution
fit_gamma %>%
  map(~ plot(.x))

# plot distribution against normal distribution
fit_norm %>%
  map(~ plot(.x))

# gamma distribution makes the most sense
glimpse(dat)

# ---- prep data for gamm ----
dat_sp <- dat_sp %>%
  map(~
        .x %>%
        mutate(
          habitat_type = factor(habitat_type),
          year = factor(year),
          animal_id = factor(animal_id)
        )

  )

lmb <- dat_sp$`Largemouth Bass`
np <- dat_sp$`Northern Pike`



ggplot(data = dat, aes(x = season, y = mean_accel, fill = habitat_type),
       alpha = 0.5) +
  geom_boxplot() +
  facet_wrap(. ~ spp) +
  scale_fill_viridis_d(end = 0.85, name = "Habitat Type",
                       alpha = 0.5) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.1, 0.85),
  ) +
  labs(
    x = "Season",
    y = "Mean Acceleration"
  ) -> p

ggsave(here("plots",
            "preliminary-plots",
            "mean_accel_season_hab_type_boxplot.png"), plot = p,
       height = 8.5, width = 11)

# ---- start our models for LMB ----
glimpse(lmb)
m <- glmmTMB(mean_accel ~ habitat_type * season + (1 | animal_id) +
               ar1(season + 0 | animal_id),
             data = lmb,
             family = Gamma(link = "log")
)
m1 <- update(m, . ~ habitat_type + (1 | animal_id),  REML = FALSE)

m2 <- update(m, . ~ season + (1 | animal_id),  REML = FALSE)




res <- simulateResiduals(m)
plot(res)
residuals(res)

res_m1 <- simulateResiduals(m1)
plot(res_m1)
res_m2 <- simulateResiduals(m2)
plot(res_m2)



# create model list for model selection ------
model_list <- list(m, m1, m2
)
# give the elements useful names
names(model_list) <- c("m",
                       "m1", "m2"
)
glance(m)

# get the summaries using `lapply

summary_list <- lapply(model_list, function(x) tidy(x, parametric = TRUE))
glance_list <- lapply(model_list, glance)

glance_summary <- map_df(glance_list, ~as.data.frame(.x), .id = "id") %>%
  mutate(model = lapply(model_list, formula) %>%
           as.character()
  ) %>%
  dplyr::select(model, id:df.residual) %>%
  arrange(AIC)

# view model selection ------
glance_summary
glance_summary %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR results",
                                  "lmer_model_selection_basin_season.xlsx"))

# create specific stuff for model saving -----
car::Anova(m)
summary(m)

main_effects <- tidy(car::Anova(m))



ind_effects <- tidy(m)


# main_effects %>%
main_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR results",
                                  "lmer_main_effect_m_basin_season.xlsx"))
ind_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "RMR results",
                                  "lmer_ind_effects_m_basin_season.xlsx"))

# multiple comparissions ----

multi_comp <- emmeans(m, pairwise ~ fish_basin * season,
                      adjust = "Tukey", type = "response",
                      pbkrtest.limit = 3353,
                      lmerTest.limit = 3353)
contrast(multi_comp, method = "pairwise", adjust = "bonferroni")





contrast_effects <- contrast(multi_comp, method = "pairwise",
                             adjust = "Tukey")

basin_season_contrast <- tidy(contrast_effects) %>%
  clean_names() %>%
  arrange(adj_p_value, contrast)


basin_season_contrast


print(basin_season_contrast, n = 66)


compares <- cld(
  object = multi_comp,
  adjust = "Tukey",
  Letters = letters,
  alpha = 0.05,
)
compares$.group


basin_season_contrast %>%
  # filter(adj_p_value < 0.05) %>%
  arrange(contrast, adj_p_value) %>%

  openxlsx::write.xlsx(here::here("results",
                                  "RMR results",
                                  "lmer_multi_comp_basin_season.xlsx"))



sums <- ful_rmr %>%
  group_by(season, fish_basin) %>%
  summarise(means = mean(mean_rmr),
            sem = sd(mean_rmr) / sqrt(n())) %>%
  ungroup() %>%
  arrange(season, fish_basin)

sums

# ----- plot ------
ggplot(data = ful_rmr, aes(x = season, y = mean_rmr)) +
  geom_boxplot(alpha = 0.5, width = 0.35
  ) +

  # scale_y_continuous(breaks = seq(15, 20, 2)) +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.92, 0.93),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Season",
       y = expression(paste("Active Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p

p
# ggsave(plot = p, filename = here("plots",
#                                  "gamm_BioE_season_boxplot.png"), width = 11,
#        height = 7)


ggplot(data = ful_rmr, aes(x = season, y = mean_rmr)) +
  geom_boxplot(aes(fill = fish_basin), alpha = 0.25
  ) +
  stat_summary(fun = mean,
               geom = "point",
               size = 3, position = position_dodge(0.75),
               aes(colour = fish_basin,
                   x = season, y = mean_rmr)) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.75),
               aes(x = season, group = fish_basin,
                   y = mean_rmr)) +
  # scale_y_continuous(breaks = seq(15, 20, 2)) +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.92, 0.93),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Season",
       y = expression(paste("Active Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p1

p1



ggplot(data = ful_rmr, aes(x = season, y = mean_rmr)) +
  geom_violin(aes(fill = fish_basin), alpha = 0.5
  ) +
  stat_summary(fun = mean,
               geom = "point",
               size = 2, position = position_dodge(0.9),
               colour = "black",
               aes(group = fish_basin,
                   # colour = fish_basin,

                   x = season, y = mean_rmr)) +
  stat_summary(fun.data = mean_se,
               geom = "errorbar", width = 0.15,
               position = position_dodge(0.9),
               aes(x = season, group = fish_basin,
                   y = mean_rmr)) +
  # scale_y_continuous(breaks = seq(15, 20, 2)) +
  scale_fill_viridis_d(name = "Basin",
                       option = "B", begin = 0.35, end = 0.75) +
  scale_colour_viridis_d(name = "Basin",
                         option = "B", begin = 0.35, end = 0.75) +
  theme_classic(base_size = 15) +
  theme(panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.92, 0.93),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Season",
       y = expression(paste("Active Metabolism (mg",
                            O[2]," ", kg^-1, " ", h^-1, ")"))) -> p2

p2
# ggsave(plot = p1, filename = here("plots",
#                                   "gamm_BioE_season_basin_boxplot.png"),
#        width = 11,
#        height = 7 )
#
#
#


write_rds(p2, here("Plot Objects",
                   "daily_rmr_GLMM_violin_plot.rds"))


ggsave(plot = p2, filename = here("plots",
                                  "violin plots",
                                  "gamm_BioE_season_basin_violin.png"),
       width = 11,
       height = 7 )
