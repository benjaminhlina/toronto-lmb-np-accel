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


lmb <- dat %>%
  filter(common_name_e == "Northern Pike")


# ---- start our models for LMB ----
glimpse(lmb)
m <- glmmTMB(mean_accel ~ habitat_type * season + (1 | animal_id),
             # ar1(season + 0 | animal_id),
             data = lmb,
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

# create specific stuff for model saving -----
car::Anova(m)
summary(m)

main_effects <- tidy(car::Anova(m))



ind_effects <- tidy(m)


# main_effects %>%
main_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "glmm_main_effects_hab_season_lmb.xlsx"))
ind_effects %>%
  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "glmm_ind_effects_hab_season_lmb.xlsx"))

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





hab_season_contrast %>%
  # filter(adj_p_value < 0.05) %>%
  arrange(
    # contrast,
    adj_p_value) %>%

  openxlsx::write.xlsx(here::here("results",
                                  "accel-glmm-results",
                                  "glmm_multi_comp_hab_season_LMB.xlsx"))



