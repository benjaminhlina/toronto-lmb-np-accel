# ---- Load Packages ----
{
  library(broom.mixed)
  library(data.table)
  library(dplyr)
  library(DHARMa)
  library(emmeans)
  library(fitdistrplus)
  library(factoextra)
  library(here)
  library(ggplot2)
  library(ggeffects)
  library(glmmTMB)
  library(MASS)
  library(multcomp)
  library(performance)
  library(qs)
  library(purrr)
  library(stringr)
  library(tidyr)
}

# ---- bring in summary dataframe -----
dat <- qread(here("data-saved",
                  "summary-accel-doy",
                  "lmb_np_summary_accel_1h.qs"))

glimpse(dat)
# look at data structure


np <- dat %>%
  filter(common_name_e == "Northern Pike") %>%
  mutate(
    season = factor(season,
                    levels = c("Fall", "Winter", "Spring", "Summer")
    ),
    habitat_type = factor(habitat_type)
  )


hist(np$mean_accel)
summary(np$mean_accel)

descdist(np$mean_accel)

fit_lognorm <- fitdist(np$mean_accel, distr = "lnorm", method = "mme")
plot(fit_lognorm)


np <- np %>%
  filter(habitat_type != is.na(habitat_type) &
           season != is.na(season))


np %>%
  distinct(day_night)

glimpse(np)

# ---- start our models for np ----
#
# Add in total lenght and diel period
glimpse(np)
m <- glmmTMB(mean_accel ~ habitat_type * season  * day_night +
               (1 | animal_id) +
               ar1(season + 0 | animal_id),
             data = np,
             family = lognormal(link = "log"),
             control = glmmTMBControl(optimizer = optim,
                                      optArgs = list(method = "BFGS"))
)

plot(check_collinearity(m))
check_autocorrelation(m)

acf(resid(m))
m1 <- update(m, . ~ habitat_type +
               (1 | animal_id),  REML = FALSE)

m2 <- update(m, . ~ season +
               (1 | animal_id) ,
             REML = FALSE)
m3 <- update(m, . ~ day_night +
               (1 | animal_id),  REML = FALSE)

m5 <- update(m, . ~ season * habitat_type +
               (1 | animal_id),  REML = FALSE)

m6 <- update(m, . ~ season * day_night +
               (1 | animal_id),  REML = FALSE)

m7 <- update(m, . ~ habitat_type * day_night +
               (1 | animal_id),  REML = FALSE)





# res <- simulateResiduals(m)
# plot(res)
# # resid
#
# par(mfrow = c(1,2))
# plotResiduals(res, interaction(np$habitat_type, np$season))
# plotResiduals(res, np$season)
#
# hist(res)
#
# hist(residuals(m))
#
#
#
# res_m1 <- simulateResiduals(m1)
# plot(res_m1)
# res_m2 <- simulateResiduals(m2)
# plot(res_m2)
# summary(m)

# create model list for model selection ------
model_list <- list(m, m1, m2, m3, m5, m6, m7
)
# give the elements useful names
names(model_list) <- c("m",
                       "m1", "m2",
                       "m3",
                       "m5", "m6", "m7"
)
glance(m)
# drop1(m, test = "Chisq")
# get the summaries using `lapply

summary_list <- lapply(model_list, function(x) tidy(x, parametric = TRUE))
glance_list <- lapply(model_list, glance)


glance_summary <- map_df(glance_list, ~as.data.frame(.x), .id = "id") %>%
  mutate(model = lapply(model_list, formula) %>%
           as.character(),
         family = lapply(model_list, function(x) family(x)$family),
         link = lapply(model_list, function(x) family(x)$link),
  ) %>%
  arrange(AIC) %>%
  mutate(
    delta_AIC = AIC - first(AIC),
    AIC_weight = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC))
  ) %>%
  dplyr::select(family, link, model, id:AIC, delta_AIC, AIC_weight, BIC:df.residual)

# view model selection ------
glance_summary

# glance_summary %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "accel-glmm-results",
#                                   "glmm_model_selection_hab_season_np.xlsx"))

res <- simulateResiduals(m5)
plot(res)
car::Anova(m5, type = "III")
# resid

par(mfrow = c(1,2))
plotResiduals(res, interaction(np$habitat_type, np$day_night))
plotResiduals(res, np$season)

hist(res)

hist(residuals(m))



res_m1 <- simulateResiduals(m1)
plot(res_m1)
res_m2 <- simulateResiduals(m2)
plot(res_m2)
summary(m)
# create specific stuff for model saving -----
car::Anova(m7, type = "3")
summary(m)
attributes(alias(m)$Complete)$dimnames[[1]]
main_effects <- tidy(car::Anova(m,))
main_effects


ind_effects <- tidy(m)
ind_effects


# main_effects %>%
# main_effects %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "accel-glmm-results",
#                                   "habitat-season",
#                                   "np",
#                                   "glmm_main_effects_hab_season_np.xlsx"))
# ind_effects %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "accel-glmm-results",
#                                   "habitat-season",
#                                   "np",
#                                   "glmm_ind_effects_hab_season_np.xlsx"))
#
# summary(m5)
# multiple comparissions ----

multi_comp <- emmeans(m, ~ habitat_type * season,
                      adjust = "bonferroni", type = "response")
# contrast(multi_comp, method = "pairwise", adjust = "bonferroni")





contrast_effects <- contrast(multi_comp, method = "pairwise",
                             adjust = "bonferroni")

hab_season_contrast <- tidy(contrast_effects) %>%
  janitor::clean_names() %>%
  arrange(adj_p_value, contrast)
hab_season_contrast


hab_season_contrast <- hab_season_contrast %>%
  separate(contrast, into = c("con_1", "con_2"), sep = " / ") %>%
  mutate(
    con_1 = str_remove(string = con_1, pattern = "\\("),
    con_1 = str_remove(string = con_1, pattern = "\\)"),
    con_2 = str_remove(string = con_2, pattern = "\\("),
    con_2 = str_remove(string = con_2, pattern = "\\)"),
  ) %>%
  separate(con_1, into = c("hab_1", "veg_1", "season_1"), sep = " ") %>%
  separate(con_2, into = c("hab_2", "veg_2", "season_2"), sep = " ") %>%
  mutate(
    hab_1 = paste(hab_1, veg_1, sep = " "),
    hab_2 = paste(hab_2, veg_2, sep = " ")
  ) %>%
  dplyr::select(-c("veg_1", "veg_2")) %>%
  arrange(season_1, season_2)
hab_season_contrast


# tmp <- expand.grid(habitat_type = unique(np$habitat_type),
#                    season = unique(np$season))
# mm <- model.matrix(~ habitat_type * season, data = tmp)
#
# mm
# glht(m, linfct = mm)
#
# np <- np %>%
#   mutate(
#     habitat_type = factor(habitat_type)
#   )
#
#
# Tukey <- contrMat(table(np$habitat_type), "Tukey")
# K1 <- cbind(Tukey, matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)))
# rownames(K1) <- paste(levels(np$habitat_type)[1], rownames(K1), sep = ":")
# K2 <- cbind(matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), Tukey)
# rownames(K2) <- paste(levels(np$season)[2], rownames(K2), sep = ":")
# K <- rbind(K1, K2)
# colnames(K) <- c(colnames(Tukey), colnames(Tukey))
# K
# summary(glht(m, linfct = mcp(habitat_type = "Tukey")))

# hab_season_contrast %>%
#   arrange(hab_1, hab_2) %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "accel-glmm-results",
#                                   "habitat-season",
#                                   "np",
#                                   "glmm_multi_comp_hab_season_np.xlsx"))
# hab_season_contrast %>%
#   filter(season_1 == season_2) %>%
#   arrange(hab_1, hab_2) %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "accel-glmm-results",
#                                   "habitat-season",
#                                   "np",
#                                   "glmm_multi_comp_hab_within_season_np.xlsx"))

# hab_season_contrast %>%
#   filter(season_1 == season_2) %>%
#   arrange(season_1, season_2) %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "accel-glmm-results",
#                                   "habitat-season",
#                                   "np",
#                                   "glmm_multi_comp_hab_within_season_np_31-Jan-24.xlsx"))
# hab_season_contrast %>%
#   filter(hab_1 == hab_2) %>%
#   arrange(hab_1, hab_2) %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "accel-glmm-results",
#                                   "habitat-season",
#                                   "np",
#                                   "glmm_multi_comp_hab_within_hab_np_31-Jan-24.xlsx"))
# ---- create specific stuff for model saving -----
car::Anova(m1)
summary(m1)

main_effects_m1 <- tidy(car::Anova(m1))



ind_effects_m1 <- tidy(m1)


# main_effects %>%
# main_effects_m1 %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "accel-glmm-results",
#                                   "habitat",
#                                   "np",
#                                   "glmm_main_effects_hab_np.xlsx"))
# ind_effects_m1 %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "accel-glmm-results",
#                                   "habitat",
#                                   "np",
#                                   "glmm_ind_effects_hab_np.xlsx"))

# multiple comparissions ----

multi_comp_m1 <- emmeans(m1, ~ habitat_type,
                         adjust = "Tukey", type = "response")
# contrast(multi_comp, method = "pairwise", adjust = "bonferroni")





contrast_effects_m1 <- contrast(multi_comp_m1, method = "pairwise",
                                adjust = "bonferroni")

cld(contrast_effects_m1,delta = 0.05, Letters = letters, sort = TRUE)

hab_season_contrast_m1 <- tidy(contrast_effects_m1) %>%
  janitor::clean_names() %>%
  arrange(adj_p_value, contrast)
hab_season_contrast_m1




# hab_season_contrast_m1 %>%
#   # filter(adj_p_value < 0.05) %>%
#   arrange(
#     # contrast,
#     adj_p_value) %>%
#
#   openxlsx::write.xlsx(here::here("results",
#                                   "accel-glmm-results",
#                                   "habitat",
#                                   "np",
#                                   "glmm_multi_comp_hab_np.xlsx"))
#
# ---- DIEL Period ----
car::Anova(m3)
car::Anova(m7)

summary(m3)
summary(m7)

res <- simulateResiduals(m3)
plot(res)
hist(res)

res <- simulateResiduals(m7)
plot(res)
hist(res)




# --- check effects ----

main_effects_dp <- tidy(car::Anova(m3))

main_effects_dp

ind_effects_dp <- broom.mixed::tidy(m3)

ind_effects_dp

main_effects_dp %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period",
                                  "np",
                                  "glmm_main_effects_dp_np.xlsx"))
ind_effects_dp %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period",
                                  "np",
                                  "glmm_ind_effects_dp_np.xlsx"))


# ---- multiple comparisons ----
multi_comp_m3 <- emmeans(m3, ~ day_night,
                         adjust = "Tukey", type = "response")


contrast_effects_m3 <- contrast(multi_comp_m3, method = "pairwise",
                                adjust = "bonferroni")


cld(contrast_effects_m3, delta = 0.05, Letters = letters, sort = TRUE)

day_night_contrast_m3 <- tidy(contrast_effects_m3) %>%
  janitor::clean_names() %>%
  arrange(adj_p_value, contrast)
day_night_contrast_m3


day_night_contrast_m3 %>%
  # filter(adj_p_value < 0.05) %>%
  arrange(
    # contrast,
    adj_p_value) %>%

  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period",
                                  "np",
                                  "glmm_multi_comp_diel_period_np.xlsx"))


# ----- contrast habitat and diel period ----

main_effects_dp_hab <- tidy(car::Anova(m7))

main_effects_dp_hab

ind_effects_dp_hab <- broom.mixed::tidy(m7)

ind_effects_dp_hab


main_effects_dp_hab %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period-habitat",
                                  "np",
                                  "glmm_main_effects_dp_hab_np.xlsx"))
ind_effects_dp_hab %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period-habitat",
                                  "np",
                                  "glmm_ind_effects_dp_hab_np.xlsx"))



# ---- multiple comparisons ----
multi_comp_m7 <- emmeans(m7, ~ day_night * habitat_type,
                         adjust = "bonferroni", type = "response")


contrast_effects_m7 <- contrast(multi_comp_m7, method = "pairwise",
                                adjust = "bonferroni")


cld(contrast_effects_m7, delta = 0.05, Letters = letters, sort = TRUE)

day_night_contrast_m7 <- tidy(contrast_effects_m7) %>%
  janitor::clean_names() %>%
  arrange(adj_p_value, contrast)
day_night_contrast_m7


# # ---- ggeeffects ----
#
# pres <- predict_response(m, terms = c("season", "habitat_type", "day_night") )
# pres
# # tp <- test_predictions(m, terms = c("season", "habitat_type", "day_night"))
#
# p <- as_tibble(pres) %>%
#   ggplot() +
#   geom_linerange(aes(colour = group,
#                      x = x, y = predicted,
#                      ymin = conf.low,
#                      ymax = conf.high),
#                  position = position_dodge(width = 0.5)) +
#   geom_point(shape = 21, colour = "black", size = 3,
#              aes(y = predicted, x = x, fill = group),
#              position = position_dodge(width = 0.5)) +
#
#   scale_y_continuous(breaks = seq(0, 0.6, 0.1)) +
#   coord_cartesian(ylim = c(0.1, 0.6)) +
#   theme_bw() +
#   lemon::facet_rep_wrap(.~ facet, repeat.tick.labels = TRUE) +
#   scale_fill_viridis_d(name = "Habitat Type",
#                        option = "D", end = 0.85) +
#   scale_colour_viridis_d(name = "Habitat Type",
#                        option = "D", end = 0.85) +
#   theme(
#     strip.background = element_blank(),
#     panel.grid = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "inside",
#     legend.position.inside = c(0.1, 0.89)
#   ) +
#   labs(
#     title = "Northern Pike",
#     x = "Season",
#     y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
#   )
# # p
# ggsave(plot = p, filename = here::here("Plots",
#                      "predicted-results",
#                      "preliminary_northern_pike_sea_hab_day_bh_version.png"), width = 11,
#        height = 8.5)
# summary(m5)
# pres_1 <- predict_response(m5, terms = c("season", "habitat_type"))
# pres_1
# tp <- test_predictions(m5, terms = c("season", "habitat_type"))
# p1 <- as_tibble(pres_1) %>%
#   ggplot() +
#   geom_linerange(aes(colour = group,
#                      x = x, y = predicted,
#                      ymin = conf.low,
#                      ymax = conf.high),
#                  position = position_dodge(width = 0.5)) +
#   geom_point(shape = 21, colour = "black", size = 3,
#              aes(y = predicted, x = x, fill = group),
#              position = position_dodge(width = 0.5)) +
#
#   scale_y_continuous(breaks = seq(0, 0.6, 0.1)) +
#   coord_cartesian(ylim = c(0.1, 0.3)) +
#   theme_bw() +
#   # lemon::facet_rep_wrap(.~ facet, repeat.tick.labels = TRUE) +
#   scale_fill_viridis_d(name = "Habitat Type",
#                        option = "D", end = 0.85) +
#   scale_colour_viridis_d(name = "Habitat Type",
#                        option = "D", end = 0.85) +
#   theme(
#     strip.background = element_blank(),
#     panel.grid = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "inside",
#     legend.position.inside = c(0.1, 0.89)
#   ) +
#   labs(
#     title = "Northern Pike",
#     x = "Season",
#     y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
#   )
# p1
# ggsave(plot = p1, filename = here::here("Plots",
#                      "predicted-results",
#                      "preliminary_northern_pike_sea_hab_bh_version.png"), width = 11,
#        height = 8.5)
# pres_2 <- predict_response(m7, terms = c("day_night", "habitat_type"))
# pres_2
# p2 <- as_tibble(pres_2) %>%
#   ggplot() +
#   geom_linerange(aes(colour = group,
#                      x = x, y = predicted,
#                      ymin = conf.low,
#                      ymax = conf.high),
#                  position = position_dodge(width = 0.5)) +
#   geom_point(shape = 21, colour = "black", size = 3,
#              aes(y = predicted, x = x, fill = group),
#              position = position_dodge(width = 0.5)) +
#
#   scale_y_continuous(breaks = seq(0, 0.6, 0.1)) +
#   coord_cartesian(ylim = c(0.1, 0.3)) +
#   theme_bw() +
#   # lemon::facet_rep_wrap(.~ facet, repeat.tick.labels = TRUE) +
#   scale_fill_viridis_d(name = "Habitat Type",
#                        option = "D", end = 0.85) +
#   scale_colour_viridis_d(name = "Habitat Type",
#                        option = "D", end = 0.85) +
#   theme(
#     strip.background = element_blank(),
#     panel.grid = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "inside",
#     legend.position.inside = c(0.1, 0.89)
#   ) +
#   labs(
#     title = "Northern Pike",
#     x = "Diel Period",
#     y = expression(paste("Mean Acceleration (m ", s^-2, ")"))
#   )
# p2
#
#
# ggsave(plot = p2, filename = here::here("Plots",
#                      "predicted-results",
#                      "preliminary_northern_pike_hab_day_night_bh_version.png"), width = 11,
#        height = 8.5)
