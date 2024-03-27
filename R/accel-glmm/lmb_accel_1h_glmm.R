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
  library(stringr)
  library(tidyr)
}

# ---- bring in summary dataframe -----
dat <- qread(here("data-saved",
                  "summary-accel-doy",
                  "lmb_np_summary_accel_1h.qs"))

glimpse(dat)
# look at data structure


lmb <- dat %>%
  filter(common_name_e == "Largemouth Bass") %>%
  mutate(
    season = factor(season,
                    levels = c("Fall", "Winter", "Spring", "Summer")
    ),
    habitat_type = factor(habitat_type)
  ) %>%
  filter(habitat_type != is.na(habitat_type) &
           season != is.na(season))
hist(lmb$mean_accel)

lognor <- fitdist(lmb$mean_accel, distr = "lnorm", method = "mme")

plot(lognor)
gammas <- fitdist(lmb$mean_accel, distr = "gamma", method = "mme")

plot(gammas)

# ---- start our models for LMB ----
glimpse(lmb)
m <- glmmTMB(mean_accel ~ habitat_type * season * day_night +
               (1 | animal_id) +
             ar1(season + 0 | animal_id),
             data = lmb,
             family = lognormal(link = "log")
)
m1 <- update(m, . ~ habitat_type + (1 | animal_id),  REML = FALSE)

m2 <- update(m, . ~ season + (1 | animal_id),  REML = FALSE)

m3 <- update(m, . ~ day_night + (1 | animal_id),  REML = FALSE)

m4 <- update(m, . ~ habitat_type * season +
               (1 | animal_id),  REML = FALSE)

m5 <- update(m, . ~ habitat_type * day_night +
               (1 | animal_id),  REML = FALSE)

m6 <- update(m, . ~ habitat_type * day_night +
               (1 | animal_id),  REML = FALSE)

m7 <- update(m, . ~ season * day_night +
               (1 | animal_id),  REML = FALSE)









# create model list for model selection ------
model_list <- list(m, m1, m2, m3, m4, m5, m6, m7
)
# give the elements useful names
names(model_list) <- c("m",
                       "m1", "m2",
                       "m3", "m4", "m5",
                       "m6", "m7"
)
glance(m)

# get the summaries using `lapply

summary_list <- lapply(model_list, function(x) tidy(x, parametric = TRUE))
glance_list <- lapply(model_list, glance)


glance_summary <- map_df(glance_list, ~as.data.frame(.x), .id = "id") %>%
  mutate(model = lapply(model_list, formula) %>%
           as.character(),
         family = lapply(model_list, function(x) family(x)$family),
         link = lapply(model_list, function(x) family(x)$link),
  ) %>%
  mutate(
    delta_AIC = AIC - first(AIC),
    AIC_weight = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC))
  ) %>%
  dplyr::select(family, link, model, id:AIC, delta_AIC, AIC_weight, BIC:df.residual) %>%
  arrange(AIC)

# view model selection ------
glance_summary
glance_summary %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "glmm_model_selection_hab_season_lmb.xlsx"))

# check model ----
res <- simulateResiduals(m4)
plot(res)

par(mfrow = c(1,2))
# plotResiduals(res, interaction(lmb$habitat_type, lmb$season))
plotResiduals(res, lmb$season)
plotResiduals(res, lmb$habitat_type)

hist(res)



hist(residuals(m4))

res_m1 <- simulateResiduals(m1)
plot(res_m1)
res_m2 <- simulateResiduals(m2)
plot(res_m2)


# ---- Model main effects ----

main_effects <- tidy(car::Anova(m))

main_effects

ind_effects <- tidy(m)

ind_effects

# ---- save output ----
main_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat-season-diel-period",
                                  "glmm_main_effects_hab_season_dp_lmb.xlsx"))
ind_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat-season-diel-period",
                                  "glmm_ind_effects_hab_season_dp_lmb.xlsx"))

# create specific stuff for model saving -----
car::Anova(m4)
summary(m4)

main_effects <- tidy(car::Anova(m4))

summary(m4)

ind_effects <- tidy(m4)

main_effects
# main_effects %>%
# main_effects %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "accel-glmm-results",
#                                   "habitat-season",
#                                   "lmb",
#                                   "glmm_main_effects_hab_season_lmb.xlsx"))
# ind_effects %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "accel-glmm-results",
#                                   "habitat-season",
#                                   "lmb",
#                                   "glmm_ind_effects_hab_season_lmb.xlsx"))

# multiple comparissions ----

multi_comp <- emmeans(m4, ~ habitat_type * season,
                      adjust = "bonferroni", type = "response")





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
  arrange(season_1, season_2) %>%
  mutate(
    adj_p_value = round(adj_p_value, digits = 3)
  )


# ---- habat contransts ----
hab_season_contrast %>%
  arrange(hab_1, hab_2) %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat-season",
                                  "lmb",
                                  "glmm_multi_comp_hab_season_LMB.xlsx"))
hab_season_contrast %>%
  filter(season_1 == season_2) %>%
  arrange(hab_1, hab_2) %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat-season",
                                  "lmb",
                                  "glmm_multi_comp_hab_within_season_LMB.xlsx"))
hab_season_contrast %>%
  filter(hab_1 == hab_2) %>%
  arrange(hab_1, hab_2) %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat-season",
                                  "lmb",
                                  "glmm_multi_comp_hab_within_hab_LMB.xlsx"))



# ----- create specific stuff for model saving -----
car::Anova(m1)
summary(m1)

main_effects_m1 <- tidy(car::Anova(m1))



ind_effects_m1 <- tidy(m1)


# main_effects %>%
main_effects_m1 %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat",
                                  "lmb",
                                  "glmm_main_effects_hab_lmb.xlsx"))
ind_effects_m1 %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat",
                                  "lmb",
                                  "glmm_ind_effects_hab_season_lmb.xlsx"))

# multiple comparissions ----

multi_comp_m1 <- emmeans(m1, ~ habitat_type,
                         adjust = "bonferroni", type = "response")
# contrast(multi_comp, method = "pairwise", adjust = "bonferroni")





contrast_effects_m1 <- contrast(multi_comp_m1, method = "pairwise",
                                adjust = "bonferroni")

hab_season_contrast_m1 <- tidy(contrast_effects_m1) %>%
  janitor::clean_names() %>%
  arrange(adj_p_value, contrast)





hab_season_contrast_m1 %>%
  arrange(
    adj_p_value) %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat",
                                  "lmb",
                                  "glmm_multi_comp_hab_LMB.xlsx"))



# ---- diel period -----
res <- simulateResiduals(m3)
plot(res)
hist(res)



main_effects_m3 <- tidy(car::Anova(m3))
main_effects_m3

ind_effects_m3 <- tidy(m3)

main_effects_m3 %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period",
                                  "lmb",
                                  "glmm_main_effects_dp_lmb.xlsx"))
ind_effects_m3 %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period",
                                  "lmb",
                                  "glmm_ind_effects_db_hab_lmb.xlsx"))



# multicomparisons

multi_comp_m3 <- emmeans(m3, ~ day_night,
                         adjust = "bonferroni", type = "response")


contrast_effects_m3 <- contrast(multi_comp_m3, method = "pairwise",
                                adjust = "bonferroni")

hab_season_contrast_m3 <- tidy(contrast_effects_m3) %>%
  janitor::clean_names() %>%
  arrange(adj_p_value, contrast)




hab_season_contrast_m3 %>%
  arrange(
    # contrast,
    adj_p_value) %>%

  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period",
                                  "lmb",
                                  "glmm_multi_comp_dp_LMB.xlsx"))


# ---- diel period and habitat ----

res <- simulateResiduals(m6)
plot(res)
hist(res)


main_effects_m6 <- tidy(car::Anova(m6))

main_effects_m6
ind_effects_m6 <- tidy(m6)

main_effects_m6 %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period-habitat",
                                  "lmb",
                                  "glmm_main_effects_dp_hab_lmb.xlsx"))
ind_effects_m6 %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period-habitat",
                                  "lmb",
                                  "glmm_ind_effects_db_hab_lmb.xlsx"))



# ---- multicomparisons -----

multi_comp_m6 <- emmeans(m6, ~ day_night * habitat_type,
                         adjust = "bonferroni", type = "response")


contrast_effects_m6 <- contrast(multi_comp_m6, method = "pairwise",
                                adjust = "bonferroni")


day_night_contrast_m6 <- tidy(contrast_effects_m6) %>%
  janitor::clean_names() %>%
  arrange(adj_p_value, contrast)
day_night_contrast_m6


day_night_season_contrast <- day_night_contrast_m6 %>%
  separate(contrast, into = c("con_1", "con_2"), sep = " / ") %>%
  mutate(
    con_1 = str_remove(string = con_1, pattern = "\\("),
    con_1 = str_remove(string = con_1, pattern = "\\)"),
    con_2 = str_remove(string = con_2, pattern = "\\("),
    con_2 = str_remove(string = con_2, pattern = "\\)"),
  ) %>%
  separate_wider_delim(con_1, names = c("dp_1", "veg_1", "sav_1"),
                              delim = " ") %>%
  separate_wider_delim(con_2, names = c("dp_2", "veg_2", "sav_2"),
                              delim = " ") %>%
  mutate(
    veg_1 = paste(veg_1, sav_1, sep = " "),
    veg_2 = paste(veg_2, sav_2, sep = " ")
  ) %>%
  dplyr::select(-c("sav_1", "sav_2")) %>%
  arrange(dp_1, dp_2)
day_night_season_contrast



day_night_season_contrast %>%
  arrange(veg_1, veg_2) %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period-habitat",
                                  "lmb",
                                  "glmm_multi_comp_hab_season_lmb.xlsx"))
day_night_season_contrast %>%
  filter(dp_1 == dp_2) %>%
  arrange(veg_1, veg_2) %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period-habitat",
                                  "lmb",
                                  "glmm_multi_comp_hab_within_dp_lmb.xlsx"))

# hab_season_contrast %>%
#   filter(season_1 == season_2) %>%
#   arrange(season_1, season_2) %>%
#   openxlsx::write.xlsx(here::here("results",
#                                   "accel-glmm-results",
#                                   "habitat-season",
#                                   "np",
#                                   "glmm_multi_comp_hab_within_season_np_31-Jan-24.xlsx"))
day_night_season_contrast %>%
  filter(veg_1 == veg_2) %>%
  arrange(veg_1, veg_2) %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period-habitat",
                                  "lmb",
                                  "glmm_multi_comp_hab_within_hab_lmb.xlsx"))
# ---- seasons -----

car::Anova(m2)


multi_comp_m2 <- emmeans(m2, ~  season,
                         adjust = "bonferroni", type = "response")


contrast_effects_m2 <- contrast(multi_comp_m2, method = "pairwise",
                                adjust = "bonferroni")

contrast_m2 <- tidy(contrast_effects_m2) %>%
  janitor::clean_names() %>%
  arrange(adj_p_value, contrast)
contrast_m2

