#Socio-demographic and clinical factors impacting NOM selection

library(dplyr)
library(tidyr)
library(lme4)        
library(broom.mixed) 
library(ggplot2)     
library(stringr)     

data_path = ""
clean_s = read.csv(data_path, check.names = FALSE, row.names=1) 

#data pre
aim1_data <- clean_s %>%
  mutate(
    nom_indicator = ifelse(plan == "NOM", 1, 0),
    
    # ===== Collapse age into 3 groups =====
    Age_3group = case_when(
      Age_Categories %in% c("18-24", "25-34", "35-44") ~ "18-44",
      Age_Categories %in% c("45-54", "55-64") ~ "45-64",
      Age_Categories %in% c("65-74", "75+") ~ "65+",
      TRUE ~ NA_character_
    ),
    
    Age_3group = factor(Age_3group, levels = c("18-44", "45-64", "65+")),
    
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
          any_lith, wbc, ccioutcomes, largest_size, bmi, symptoms, 
          abnormal_vital_indicator, decision_aidoutcomes, site_name)

# Fit the Updated Mixed-Effects Model
model_aim1 <- glmer(
  nom_indicator ~ Age_3group + Sex + ESL + RaceEthnicity +
    any_lith + wbc + ccioutcomes + largest_size +
    bmi + symptoms + abnormal_vital_indicator +
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


#__________________
fixed_results <- tidy(model_aim1, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    clean_term = case_when(
      term == "Age_3group45-64" ~ "Age: 45–64 vs 18–44",
      term == "Age_3group65+" ~ "Age: 65+ vs 18–44",
      
      term == "SexMale" ~ "Sex: Male vs Female",
      term == "ESLYes" ~ "English as Second Language: Yes vs No",
      
      term == "RaceEthnicityBlack/African American" ~ "Race/Ethnicity: Black/African American",
      term == "RaceEthnicityHispanic/Latino/Spanish Origin" ~ "Race/Ethnicity: Hispanic/Latino/Spanish Origin",
      term == "RaceEthnicityMultiracial" ~ "Race/Ethnicity: Multiracial",
      term == "RaceEthnicityNon-Hispanic White" ~ "Race/Ethnicity: Non-Hispanic White",
      term == "RaceEthnicityOther/Not Specified" ~ "Race/Ethnicity: Other/Not Specified",
      
      term == "ccioutcomes0" ~ "CCI: 0 vs reference",
      term == "ccioutcomes1-3" ~ "CCI: 1–3 vs reference",
      
      term == "any_lithTRUE" ~ "Any Lithotripsy: Yes vs No",
      term == "largest_size" ~ "Largest Stone Size",
      term == "bmi" ~ "BMI",
      term == "wbc" ~ "White Blood Cell Count",
      term == "symptoms" ~ "Symptom Score",
      term == "abnormal_vital_indicatorYes" ~ "Abnormal Vital Signs: Yes vs No",
      
      TRUE ~ term
    ),
    group = case_when(
      clean_term %in% c("Age: 65+ vs 18–44", "Age: 45–64 vs 18–44") ~ "Age",
      clean_term %in% c("Sex: Male vs Female", "English as Second Language: Yes vs No") ~ "Demographic",
      str_detect(clean_term, "^Race/Ethnicity:") ~ "Race",
      str_detect(clean_term, "^CCI:") ~ "CCI",
      TRUE ~ "Clinical"
    )
  )

term_order <- c(
  "Age: 65+ vs 18–44",
  "Age: 45–64 vs 18–44",
  "Sex: Male vs Female",
  "English as Second Language: Yes vs No",
  "Race/Ethnicity: Black/African American",
  "Race/Ethnicity: Hispanic/Latino/Spanish Origin",
  "Race/Ethnicity: Multiracial",
  "Race/Ethnicity: Non-Hispanic White",
  "Race/Ethnicity: Other/Not Specified",
  "CCI: 0 vs reference",
  "CCI: 1–3 vs reference",
  "Any Lithotripsy: Yes vs No",
  "Largest Stone Size",
  "BMI",
  "White Blood Cell Count",
  "Symptom Score",
  "Abnormal Vital Signs: Yes vs No"
)

fixed_results <- fixed_results %>%
  mutate(clean_term = factor(clean_term, levels = rev(term_order)))

plot_df <- fixed_results %>%
  mutate(ypos = seq_len(n()))

group_breaks <- plot_df %>%
  arrange(ypos) %>%
  group_by(group) %>%
  summarise(max_y = max(ypos), .groups = "drop") %>%
  slice(-n()) %>%
  mutate(yintercept = max_y + 0.5)

p2 <- ggplot(plot_df, aes(x = estimate, y = clean_term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "#0072B2") +
  geom_hline(
    yintercept = c(15.5, 13.5, 8.5, 6.5),
    color = "grey70",
    linewidth = 0.6
  ) +
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10)) +
  labs(
    title = "Predictors of Non-Operative Management",
    subtitle = "Fixed Effects (Odds Ratios and 95% Confidence Intervals)",
    x = "Odds Ratio (log scale)",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

print(p2)


# =========================================================================



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
