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

m <- glmmTMB(mean_accel ~ habitat_type * season  * day_night +
               (1 | animal_id) +
               ar1(season + 0 | animal_id),
             data = np,
             family = lognormal(link = "log"),
             control = glmmTMBControl(optimizer = optim,
                                      optArgs = list(method = "BFGS"))
)


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

# ---- create model list for model selection ------
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

# ---- view model selection ------
glance_summary

glance_summary %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "glmm_model_selection_hab_season_np.xlsx"))





# ---- check collinearity, auto co -----
plot(check_collinearity(m))
check_autocorrelation(m)

acf(resid(m))
res <- simulateResiduals(m)
plot(res)
car::Anova(m5, type = "III")
# resid

par(mfrow = c(1,2))
plotResiduals(res, interaction(np$habitat_type, np$day_night))
plotResiduals(res, np$season)

hist(res)

hist(residuals(m))


# ---- extract main model effects -----



# ---- simulated resideuals ----
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


main_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat-season",
                                  "np",
                                  "glmm_main_effects_hab_season_np.xlsx"))
ind_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat-season",
                                  "np",
                                  "glmm_ind_effects_hab_season_np.xlsx"))

# summary(m5)
# multiple comparissions ----

multi_comp <- emmeans(m5, ~ habitat_type * season,
                      adjust = "holm",
                      type = "response")
# contrast(multi_comp, method = "pairwise", adjust = "bonferroni")





contrast_effects <- contrast(multi_comp, method = "pairwise",
                             adjust = "holm")

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
hab_season_contrast %>%
  filter(season_1 == "Winter" & season_2 == "Winter")




hab_season_contrast %>%
  filter(season_1 == season_2) %>%
  arrange(season_1, season_2) %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat-season",
                                  "np",
                                  "glmm_multi_comp_hab_within_season_np.xlsx"))
hab_season_contrast %>%
  filter(hab_1 == hab_2) %>%
  arrange(hab_1, hab_2) %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat-season",
                                  "np",
                                  "glmm_multi_comp_hab_within_hab_np.xlsx"))
# ---- create specific stuff for model saving -----
car::Anova(m1)
summary(m1)

main_effects_m1 <- tidy(car::Anova(m1))



ind_effects_m1 <- tidy(m1)


main_effects_m1 %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat",
                                  "np",
                                  "glmm_main_effects_hab_np.xlsx"))
ind_effects_m1 %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat",
                                  "np",
                                  "glmm_ind_effects_hab_np.xlsx"))

# multiple comparissions ----

multi_comp_m1 <- emmeans(m1, ~ habitat_type,
                         adjust = "Tukey", type = "response")
# contrast(multi_comp, method = "pairwise", adjust = "bonferroni")





contrast_effects_m1 <- contrast(multi_comp_m1, method = "pairwise",
                                adjust = "bonferroni")

# cld(contrast_effects_m1,delta = 0.05, Letters = letters, sort = TRUE)

hab_season_contrast_m1 <- tidy(contrast_effects_m1) %>%
  janitor::clean_names() %>%
  arrange(adj_p_value, contrast)
hab_season_contrast_m1




hab_season_contrast_m1 %>%
  arrange(
    adj_p_value) %>%

  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat",
                                  "np",
                                  "glmm_multi_comp_hab_np.xlsx"))

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


# cld(contrast_effects_m7, delta = 0.05, Letters = letters, sort = TRUE)

day_night_contrast_m7 <- tidy(contrast_effects_m7) %>%
  janitor::clean_names() %>%
  arrange(adj_p_value, contrast)
day_night_contrast_m7


day_night_season_contrast <- day_night_contrast_m7 %>%
  separate(contrast, into = c("con_1", "con_2"), sep = " / ") %>%
  mutate(
    con_1 = str_remove(string = con_1, pattern = "\\("),
    con_1 = str_remove(string = con_1, pattern = "\\)"),
    con_2 = str_remove(string = con_2, pattern = "\\("),
    con_2 = str_remove(string = con_2, pattern = "\\)"),
  ) %>%
  separate_wider_delim(con_1, names = c("dp_1", "veg_1", "sav_1"), delim = " ") %>%
  separate_wider_delim(con_2, names = c("dp_2", "veg_2", "sav_2"), delim = " ") %>%
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
                                  "np",
                                  "glmm_multi_comp_hab_season_np.xlsx"))
day_night_season_contrast %>%
  filter(dp_1 == dp_2) %>%
  arrange(veg_1, veg_2) %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period-habitat",
                                  "np",
                                  "glmm_multi_comp_hab_within_dp_np.xlsx"))

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
                                  "np",
                                  "glmm_multi_comp_hab_within_hab_np.xlsx"))
# ---- look at season -----
car::Anova(m2)

res <- simulateResiduals(m2)
plot(res)

car::Anova(m2)

multi_comp_m2 <- emmeans(m2, ~ season,
                         adjust = "bonferroni", type = "response")


contrast_effects_m2 <- contrast(multi_comp_m2, method = "pairwise",
                                adjust = "bonferroni")


# cld(contrast_effects_m7, delta = 0.05, Letters = letters, sort = TRUE)

contrast_m2 <- tidy(contrast_effects_m2) %>%
  janitor::clean_names() %>%
  arrange(adj_p_value, contrast)
contrast_m2


contrast_m2 <- contrast_m2 %>%
  separate(contrast, into = c("con_1", "con_2"), sep = " / ")
  # mutate(
  #   con_1 = str_remove(string = con_1, pattern = "\\("),
  #   con_1 = str_remove(string = con_1, pattern = "\\)"),
  #   con_2 = str_remove(string = con_2, pattern = "\\("),
  #   con_2 = str_remove(string = con_2, pattern = "\\)"),
  # ) %>%
  # separate_wider_delim(con_1, names = c("dp_1", "veg_1", "sav_1"), delim = " ") %>%
  # separate_wider_delim(con_2, names = c("dp_2", "veg_2", "sav_2"), delim = " ") %>%
  # mutate(
  #   veg_1 = paste(veg_1, sav_1, sep = " "),
  #   veg_2 = paste(veg_2, sav_2, sep = " ")
  # ) %>%
  # dplyr::select(-c("sav_1", "sav_2")) %>%
  # arrange(dp_1, dp_2)
day_night_season_contrast



day_night_season_contrast %>%
  arrange(veg_1, veg_2) %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "diel-period-habitat",
                                  "np",
                                  "glmm_multi_comp_hab_season_np.xlsx"))


