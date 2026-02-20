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
install.packages("mgcViz")
library(mgcViz)

#####loading in dataset-----
veg_master <- read_excel("O:/all_common/Research/SWMP/BIOMONITORING/EMERGENT VEGETATION/Emerg Veg Data/Masters/VEG/CDMO Masters (2012-2025) marsh edge.xlsx")

#making 'eroded' plots 100% unvegetated 
no.na.veg <- veg_master %>%
  mutate(
    Cover = ifelse(is.na(Cover), 100, Cover),
    Species = ifelse(is.na(Species), "Unvegetated", Species)
  )

#subsetting unvegetated data 
unveg <- no.na.veg %>%
  filter(Species == "Unvegetated" & Season == "Fall")


unveg$Cover <- as.numeric(unveg$Cover)
unveg$Cover_prop <- unveg$Cover / 100
n <- nrow(unveg)
unveg$Cover_prop <- (unveg$Cover_prop * (n - 1) + 0.5) / n
unveg$Cover_prop <- (unveg$Cover_prop * (n - 1) + 0.5) / n

unveg$SiteID <- as.factor(unveg$SiteID)
unveg$TransectID <- as.factor(unveg$TransectID)
unveg$Distance <- as.factor(unveg$Distance)

model <- glmmTMB(
  Cover_prop ~ Year + Distance + (1 | SiteID/TransectID),
  data = unveg,
  family = beta_family()
)

summary(unveg$Cover_prop)

model_test <- glmmTMB(
  Cover_prop ~ Year + Distance + (1 | SiteID),
  data = unveg,
  family = beta_family()
)


model_test2 <- glmmTMB(
  Cover_prop ~ Year + Distance +SiteID,
  data = unveg,
  family = beta_family()
)

summary(model_test)

unveg$SiteTransect <- interaction(unveg$SiteID, unveg$TransectID)

model <- glmmTMB(
  Cover_prop ~ Year + Distance +
    (1 | SiteTransect),
  data = unveg,
  family = beta_family()
)

model_fixed <- glmmTMB(
  Cover_prop ~ Year + Distance + (1|SiteID/TransectID),
  data = unveg,
  family = beta_family()
)
summary(model_fixed)

predict(model_fixed, type="response")

Anova(model_fixed)

model_qb <- glm(
  Cover_prop ~ Year + Distance + SiteID,
  data = unveg,
  family = quasibinomial(link = "logit")
)



sim_res <- simulateResiduals(model_fixed, n = 1000)
plot(sim_res)

testDispersion(sim_res)   # checks over/under-dispersion
testUniformity(sim_res)   # checks if residuals follow uniform distribution

gam_model <- gam(
  Cover_prop ~ s(Year) + Distance + SiteID,
  family = betar(link = "logit"),
  data = unveg,
  method = "REML"
)


summary(gam_model)

sim_res <- simulateResiduals(gam_model, n = 1000)
plot(sim_res)
testDispersion(sim_res)
testUniformity(sim_res)

gam.check(gam_model)

AIC(gam_model, model_fixed, model_qb, model_test, model, model_test2)

library(DHARMa)
sim_res <- simulateResiduals(model_test, n = 1000)
plot(sim_res)
testDispersion(sim_res)
testUniformity(sim_res)


 # 1️⃣ Make a new data frame for prediction
   # Include all levels of Distance, leave SiteID as NA to marginalize over random effect
   pred_2030 <- expand.grid(
       Year = 2060,
      Distance = levels(unveg$Distance),
       SiteID = levels(unveg$SiteID)
      )
 
   # 2️⃣ Predict proportion (0–1) using the model
   pred_2030$Cover_pred <- predict(model_test, newdata = pred_2030, type = "response")
 
   # 3️⃣ Convert to % cover
   pred_2030$Cover_pred_pct <- pred_2030$Cover_pred * 100
 
   # 4️⃣ View predictions
   pred_2030
   
   
   years_seq <- seq(2023, 2100, by = 1)
   
   future_pred <- expand.grid(
     Year = years_seq,
     Distance = levels(unveg$Distance),
     SiteID = levels(unveg$SiteID)
   )
   
   future_pred$Cover_pred <- predict(model_test,
                                     newdata = future_pred,
                                     type = "response")
   
   future_pred$Cover_pct <- future_pred$Cover_pred * 100
   
   loss_years <- future_pred %>%
     group_by(SiteID, Distance) %>%
     arrange(Year) %>%
     filter(Cover_pct > 70) %>%
     slice(1) %>%
     ungroup()
loss_years
