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


np <- dat %>%
  filter(common_name_e == "Northern Pike") %>%
  mutate(
    season = factor(season,
                    levels = c("Fall", "Winter", "Spring", "Summer")
    )
  )



# ---- start our models for np ----
glimpse(np)
m <- glmmTMB(mean_accel ~ habitat_type * season + (1 | animal_id),
             # ar1(season + 0 | animal_id),
             data = np,
             family = Gamma(link = "log")
)
m1 <- update(m, . ~ habitat_type + (1 | animal_id),  REML = FALSE)

m2 <- update(m, . ~ season + (1 | animal_id),  REML = FALSE)



res <- simulateResiduals(m)
plot(res)



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

glance_summary %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "glmm_model_selection_hab_season_np.xlsx"))

# create specific stuff for model saving -----
car::Anova(m)
summary(m)

main_effects <- tidy(car::Anova(m))



ind_effects <- tidy(m)


# main_effects %>%
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

# multiple comparissions ----

multi_comp <- emmeans(m, ~ habitat_type * season,
                      adjust = "Tukey", type = "response",
                      pbkrtest.limit = 3353,
                      lmerTest.limit = 3353)
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


hab_season_contrast %>%
  arrange(hab_1, hab_2) %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat-season",
                                  "np",
                                  "glmm_multi_comp_hab_season_np.xlsx"))
hab_season_contrast %>%
  filter(season_1 == season_2) %>%
  arrange(hab_1, hab_2) %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat-season",
                                  "np",
                                  "glmm_multi_comp_hab_within_season_np.xlsx"))


# ---- create specific stuff for model saving -----
car::Anova(m1)
summary(m1)

main_effects_m1 <- tidy(car::Anova(m1))



ind_effects_m1 <- tidy(m1)


# main_effects %>%
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

hab_season_contrast_m1 <- tidy(contrast_effects_m1) %>%
  janitor::clean_names() %>%
  arrange(adj_p_value, contrast)
hab_season_contrast_m1




hab_season_contrast_m1 %>%
  # filter(adj_p_value < 0.05) %>%
  arrange(
    # contrast,
    adj_p_value) %>%

  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "habitat",
                                  "np",
                                  "glmm_multi_comp_hab_np.xlsx"))
