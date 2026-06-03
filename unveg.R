# =========================================================
# 1. PACKAGES
# =========================================================
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(mgcv)
library(purrr)
library(broom)
library(tidyr)
library(patchwork)
library(officer)
library(flextable)
library(readxl)
library(lme4)
library(glmmTMB)
library(car)
library(DHARMa)
library(mgcViz)
library(ordbetareg)
library(bayesplot)
install.packages('data.table')
library(data.table)
library(marginaleffects)
update.packages('data.table')

# =========================================================
# 2. LOAD DATA
# =========================================================
veg_master <- read_excel("O:/all_common/Research/SWMP/BIOMONITORING/EMERGENT VEGETATION/Emerg Veg Data/Masters/VEG/CDMO Masters (2012-2025) marsh edge.xlsx")

veg_master$Cover <- as.numeric(veg_master$Cover)

no.na.veg <- veg_master %>%
  mutate(
    Cover = ifelse(is.na(Cover), 100, Cover),
    Species = ifelse(is.na(Species) | trimws(Species) == "" | Species %in% c("NA", "N/A"),
                     "Unvegetated", Species)
  )

unveg <- no.na.veg %>%
  filter(Species == "Unvegetated" & Season == "Fall")

# =========================================================
# 3. RESPONSE VARIABLE (beta prep)
# =========================================================
unveg$Cover <- as.numeric(unveg$Cover)
unveg$Cover_prop <- unveg$Cover / 100

n <- nrow(unveg)
unveg$Cover_adj <- (unveg$Cover_prop * (n - 1) + 0.5) / n

unveg$StationUID <- interaction(unveg$SiteID, unveg$TransectID)
unveg$PlotUID <- interaction(unveg$SiteID,
                             unveg$TransectID,
                             unveg$PlotID)
length(unique(unveg$PlotUID))
# =========================================================
# 4. FACTORS + SCALING
# =========================================================
unveg$SiteID <- as.factor(unveg$SiteID)
unveg$TransectID <- as.factor(unveg$TransectID)
unveg$distance_f <- as.factor(unveg$Distance)
unveg$year_f = as.factor(unveg$Year)
unveg$year_c = unveg$Year - min(unveg$Year)
unveg$Year_sc <- scale(unveg$Year)
unveg$Distance_sc <- scale(unveg$Distance)

# =========================================================
# 5. MODELS
# =========================================================

m0 <- glmmTMB(
  Cover_adj ~ year_c * Distance +
    (1 | SiteID/TransectID),
  family = beta_family(),
  data = unveg
)

m1 <- glmmTMB(
  Cover_prop ~ Year_sc + Distance_f +
    (1 | SiteID/StationUID/PlotUID),
  family = beta_family(),
  data = unveg
)

m2 <- glmmTMB(
  Cover_prop ~ Year_sc + factor(Distance) +
    (1 + Year_sc | PlotUID),
  family = beta_family(),
  data = unveg
)

m3 <- ordbetareg(
  formula = bf(Cover_prop ~ year_c * Distance + (1|SiteID/TransectID)),
  data = unveg,
  cores = 4, chains = 4
)

summary(m3)

m4 <- ordbetareg(
  formula = bf(Cover_prop ~ year_c * distance_f + (1|SiteID/TransectID)),
  data = unveg,
  cores = 4, chains = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 12)
)

summary(m4)

m5 <- update(m4, formula = . ~ . + ar(time = year_c, gr = TransectID, p = 1),                
             control = list(adapt_delta = 0.99))    

m5 <- add_criterion(m5, "loo")    

loo_compare(m4, m5)

m6 <- ordbetareg(
  formula = bf(Cover_prop ~ year_c * distance_f +
                 (year_c | SiteID) +
                 (1 | SiteID:TransectID)),
  data = unveg,
  cores = 4, chains = 4,
  control = list(adapt_delta = 0.99)
)



# =========================================================
# 6. MODEL COMPARISON
# =========================================================
anova(m0, m1)
AIC(m0, m1)

m3 = add_criterion(m3, "loo")
m4 = add_criterion(m4, "loo")
m6 = add_criterion(m6, "loo")
loo_compare(m3, m4, m6)
# =========================================================
# 7. DIAGNOSTICS
# =========================================================
sim_res <- simulateResiduals(model_final, n = 1000)
plot(sim_res)
testDispersion(sim_res)
testUniformity(sim_res)

mcmc_pairs(m4, np = nuts_params(m3),  
              pars = c("b_Intercept", "b_year_c", "sd_SiteID__Intercept", "sd_SiteID:TransectID__Intercept"),
              off_diag_args = list(size = 0.5) )

# =========================================================
# 8. MODEL OUTPUT
# =========================================================
summary(model_final)
Anova(model_final)

# =========================================================
# 9. SITE-SPECIFIC RATES OF CHANGE
# =========================================================

fixed_slope <- fixef(model_final)$cond["Year_sc"]
site_re <- as.data.frame(ranef(model_final)$cond$SiteID)

site_slopes_sc <- fixed_slope + site_re$Year_sc
site_ids <- rownames(site_re)

year_sd <- sd(unveg$Year)
site_slopes_year <- site_slopes_sc / year_sd

p_mean <- mean(unveg$Cover_prop)
site_slopes_percent <- site_slopes_year * p_mean * (1 - p_mean) * 100

site_rates <- data.frame(
  SiteID = site_ids,
  slope_logit_per_year = site_slopes_year,
  slope_percent_per_year = site_slopes_percent
)

site_rates <- site_rates[order(site_rates$slope_percent_per_year), ]
site_rates

site_trends <- avg_slopes(
  m6,
  variables = "year_c",
  by = "SiteID",
  newdata = datagrid(SiteID = unique(unveg$SiteID), distance_f = "0")
)
site_trends

overall_trend <- avg_slopes(
  m6,
  variables = "year_c")

overall_trend
summary(m6)

# =========================================================
# 10. PREDICTIONS + PLOT
# =========================================================
newdat <- expand.grid(
  Year_sc = seq(min(unveg$Year_sc), max(unveg$Year_sc), length = 100),
  Distance_sc = 0,
  SiteID = unique(unveg$SiteID)
)

newdat$pred <- predict(model_final, newdat, type = "response")

ggplot(newdat, aes(Year_sc, pred, color = SiteID)) +
  geom_line() +
  theme_minimal()

# =========================================================
# 11. FIGURING OUT DIAGNOSTICS
# =========================================================
sim <- simulateResiduals(m1)

plot(sim)
testUniformity(sim)
testDispersion(sim)
testOutliers(sim)

plotResiduals(sim, unveg$Year_sc)
plotResiduals(sim, unveg$Distance_sc)

res <- residuals(m1, type = "pearson")
head(unveg[order(abs(res), decreasing = TRUE), ])

hist(unveg$Cover_prop)

sum(unveg$Cover_prop == 1)
mean(unveg$Cover_prop == 1)

plotResiduals(sim, unveg$Year_sc)
plotResiduals(sim, unveg$Distance_sc)
plotResiduals(sim, predict(m1))

Cover_prop ~ poly(Year_sc, 2) + Distance_sc +
  (1 + Year_sc | SiteID)

# =========================================================
# 11. HEIGHT DYNAMICS
# =========================================================

hist(veg_master$`Maximum Canopy Height`)
