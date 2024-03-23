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
m <- glmmTMB(mean_accel ~ habitat_type * season  * length +
               (1 | animal_id),
               # ar1(season + 0 | animal_id),
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
m3 <- update(m, . ~ length +
               (1 | animal_id),  REML = FALSE)

m5 <- update(m, . ~ season * habitat_type +
               (1 | animal_id),  REML = FALSE)

m6 <- update(m, . ~ season * length +
               (1 | animal_id),  REML = FALSE)

m7 <- update(m, . ~ habitat_type * length +
               (1 | animal_id),  REML = FALSE)
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

pres <- predict_response(m7, terms = c("habitat_type", "length") )
pres
# tp <- test_predictions(m, terms = c("season", "habitat_type", "day_night"))

plot(pres)

glimpse(dat)
ggplot(data = np, aes(x = time_bin_1h, y = mean_accel,
                       colour = habitat_type)) +
  # geom_point() +
  geom_smooth(method = "gam", ) +
  # coord_cartesian(ylim = c(0, 3)) +
  theme_ggeffects()




