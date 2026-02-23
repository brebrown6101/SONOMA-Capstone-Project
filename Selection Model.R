#Socio-demographic and clinical factors impacting NOM selection

library(dplyr)
library(tidyr)
library(lme4)        
library(broom.mixed) 
library(ggplot2)     
library(stringr)     

#data pre
aim1_data <- clean_s %>%
  mutate(
    
    nom_indicator = ifelse(plan == "NOM", 1, 0),
    
    
    Age_Categories = as.factor(Age_Categories),
    Sex = as.factor(Sex),
    ESL = as.factor(ESL),
    RaceEthnicity = as.factor(RaceEthnicity),
    ccioutcomes = as.factor(ccioutcomes),
    any_lith = as.factor(any_lith),
    abnormal_vital_indicator = as.factor(abnormal_vital_indicator),
    decision_aidoutcomes = as.factor(decision_aidoutcomes),
    site_name = as.factor(site_name)
  ) %>%
  drop_na(nom_indicator, Age_Categories, Sex, ESL, RaceEthnicity, 
          any_lith, nlr, ccioutcomes, largest_size, bmi, symptoms, 
          abnormal_vital_indicator, decision_aidoutcomes, site_name)

#Fit the Updated Mixed-Effects Model
model_aim1 <- glmer(
  nom_indicator ~ Age_Categories + Sex + ESL + RaceEthnicity + 
    any_lith + nlr + ccioutcomes + largest_size + 
    bmi + symptoms + abnormal_vital_indicator + decision_aidoutcomes +
    (1 | site_name), 
  data = aim1_data,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)


summary(model_aim1)


# =========================================================================


# Unadjusted NOM Rate by Site
site_nom_rates <- aim1_data %>%
  group_by(site_name) %>%
  summarise(
    total = n(),
    nom_count = sum(nom_indicator),
    nom_rate = nom_count / total,
    se = sqrt((nom_rate * (1 - nom_rate)) / total),
    ci_low = pmax(0, nom_rate - 1.96 * se),
    ci_high = pmin(1, nom_rate + 1.96 * se),
    .groups = "drop"
  )

p1 <- ggplot(site_nom_rates, aes(x = nom_rate, y = reorder(site_name, nom_rate))) +
  geom_point(size = 3, color = "#2c3e50") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2, color = "#2c3e50") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Unadjusted NOM Rate by Site",
       x = "NOM Rate (with 95% CI)",
       y = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

print(p1)


# Forest Plot of Fixed Effects - Odds Ratios

fixed_results <- tidy(model_aim1, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>% 
  mutate(
    
    clean_term = str_replace_all(term, "Age_Categories", "Age: "),
    clean_term = str_replace_all(clean_term, "RaceEthnicity", "Race/Eth: "),
    clean_term = str_replace_all(clean_term, "Sex", "Sex: "),
    clean_term = str_replace_all(clean_term, "ccioutcomes", "CCI: "),
    clean_term = str_replace_all(clean_term, "TRUE", " (Yes)"),
    clean_term = str_replace_all(clean_term, "1", " (Yes)")
  )

p2 <- ggplot(fixed_results, aes(x = estimate, y = reorder(clean_term, estimate))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "#0072B2") +
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10)) + # 对于 OR，使用对数坐标轴是黄金标准
  labs(title = "Predictors of Non-Operative Management (NOM)",
       subtitle = "Fixed Effects (Odds Ratios & 95% CI)",
       x = "Odds Ratio (Log Scale)",
       y = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

print(p2)


#Site-Level Variation in NOM Selection
ranef_results <- tidy(model_aim1, effects = "ran_vals", conf.int = TRUE) %>%
  mutate(
    
    OR = exp(estimate),
    ci_low = exp(conf.low),
    ci_high = exp(conf.high)
  )

p3 <- ggplot(ranef_results, aes(x = OR, y = reorder(level, OR))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_point(size = 3, color = "#D55E00") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2, color = "#D55E00") +
  scale_x_log10(breaks = c(0.2, 0.5, 1, 2, 5)) +
  labs(title = "Site-Level Variation in NOM Selection",
       subtitle = "Random Effects (Hospital-specific Odds Ratios)",
       x = "Odds Ratio (Relative to Average Site)",
       y = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

print(p3)