library(dplyr)
library(tidyr)
library(lme4)
library(broom.mixed)
library(ggplot2)
library(stringr)

# ===============================
# Load Data

data_path = ""
clean_s = read.csv(data_path, check.names = FALSE, row.names = 1)

# ===============================
# Data preprocessing

aim1_data <- clean_s %>%
  
  mutate(
    
    # Outcome
    nom_indicator = as.numeric(plan == "NOM"),
    
    # Collapse age
    Age_3group = case_when(
      Age_Categories %in% c("18-24","25-34","35-44") ~ "18-44",
      Age_Categories %in% c("45-54","55-64") ~ "45-64",
      Age_Categories %in% c("65-74","75+") ~ "65+",
      TRUE ~ NA_character_
    ),
    
    Age_3group = factor(Age_3group,
                        levels = c("18-44","45-64","65+")),
    
    Sex = as.factor(Sex),
    ESL = as.factor(ESL),
    
    # Merge Multiracial into Other
    RaceEthnicity = ifelse(
      RaceEthnicity == "Multiracial",
      "Other/Not Specified",
      RaceEthnicity
    ),
    
    RaceEthnicity = as.factor(RaceEthnicity),
    
    # CCI baseline = 0
    ccioutcomes = factor(ccioutcomes,
                         levels = c("0","1-3",">3")),
    
    ccioutcomes = relevel(ccioutcomes, ref = "0"),
    
    appendicolithoutcomes = as.factor(appendicolithoutcomes),
    
    site_name = as.factor(site_name)
    
  ) %>%
  
  drop_na(
    nom_indicator,
    Age_3group,
    Sex,
    ESL,
    RaceEthnicity,
    wbc,
    ccioutcomes,
    appendicolithoutcomes,
    site_name
  )

# ===============================
# Mixed model

model_aim1 <- glmer(
  
  nom_indicator ~
    Age_3group +
    Sex +
    ESL +
    RaceEthnicity +
    wbc +
    ccioutcomes +
    appendicolithoutcomes +
    (1 | site_name),
  
  data = aim1_data,
  
  family = binomial(link = "logit"),
  
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 1e5)
  )
)

summary(model_aim1)

# ===============================
# Unadjusted site NOM rate

site_nom_rates <- aim1_data %>%
  
  group_by(site_name) %>%
  
  summarise(
    
    total = n(),
    nom_count = sum(nom_indicator),
    nom_rate = nom_count / total,
    
    se = sqrt((nom_rate*(1-nom_rate))/total),
    
    ci_low = pmax(0, nom_rate - 1.96*se),
    ci_high = pmin(1, nom_rate + 1.96*se),
    
    .groups = "drop"
  )

p1 <- ggplot(
  site_nom_rates,
  aes(x = nom_rate,
      y = reorder(site_name, nom_rate))
) +
  
  geom_point(size = 3, color = "#2c3e50") +
  
  geom_errorbarh(
    aes(xmin = ci_low,
        xmax = ci_high),
    height = 0.2
  ) +
  
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  labs(
    title = "Unadjusted NOM Rate by Site",
    x = "NOM Rate (95% CI)",
    y = NULL
  ) +
  
  theme_minimal(base_size = 14)

print(p1)

# ===============================
# Extract fixed effects

fixed_results <- tidy(
  model_aim1,
  effects = "fixed",
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  
  filter(term != "(Intercept)") %>%
  
  mutate(
    
    clean_term = case_when(
      
      term == "Age_3group45-64" ~ "Age: 45–64 vs 18–44",
      term == "Age_3group65+" ~ "Age: 65+ vs 18–44",
      
      term == "SexMale" ~ "Sex: Male vs Female",
      
      term == "ESLYes" ~ "English as Second Language",
      
      str_detect(term,"RaceEthnicityBlack") ~ "Race: Black/African American",
      str_detect(term,"RaceEthnicityHispanic") ~ "Race: Hispanic/Latino",
      str_detect(term,"RaceEthnicityNon-Hispanic White") ~ "Race: Non-Hispanic White",
      str_detect(term,"RaceEthnicityOther") ~ "Race: Other/Not Specified",
      
      term == "ccioutcomes1-3" ~ "CCI: 1–3 vs 0",
      term == "ccioutcomes>3" ~ "CCI: >3 vs 0",
      
      str_detect(term,"appendicolithoutcomes") ~ "Appendicolith",
      
      term == "wbc" ~ "White Blood Cell Count",
      
      TRUE ~ term
    )
  )

# ===============================
# Plot order

term_order <- c(
  
  "Age: 65+ vs 18–44",
  "Age: 45–64 vs 18–44",
  
  "Sex: Male vs Female",
  
  "English as Second Language",
  
  "Race: Black/African American",
  "Race: Hispanic/Latino",
  "Race: Non-Hispanic White",
  "Race: Other/Not Specified",
  
  "CCI: 1–3 vs 0",
  "CCI: >3 vs 0",
  
  "Appendicolith",
  
  "White Blood Cell Count"
)

fixed_results <- fixed_results %>%
  
  filter(clean_term %in% term_order) %>%
  
  mutate(
    clean_term = factor(
      clean_term,
      levels = rev(term_order)
    )
  )

# ===============================
# Forest plot

p2 <- ggplot(
  fixed_results,
  aes(x = estimate,
      y = clean_term)
) +
  
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "red"
  ) +
  
  geom_point(
             color = "#0072B2") +
  
  geom_errorbarh(
    aes(xmin = conf.low,
        xmax = conf.high),
    height = 0.2, color = "#0072B2"
  ) +
  
  scale_x_log10(
    breaks = c(0.2,0.5,1,2,5)
  ) +
  
  labs(
    title = "Predictors of Non-Operative Management",
    subtitle = "Odds Ratios with 95% Confidence Intervals",
    x = "Odds Ratio (log scale)",
    y = NULL
  ) +
  
  theme_minimal(base_size = 14)

print(p2)

# ===============================
# Site random effects

ranef_results <- tidy(
  model_aim1,
  effects = "ran_vals",
  conf.int = TRUE
) %>%
  
  mutate(
    OR = exp(estimate),
    ci_low = exp(conf.low),
    ci_high = exp(conf.high)
  )

p3 <- ggplot(
  ranef_results,
  aes(x = OR,
      y = reorder(level, OR))
) +
  
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "red"
  ) +
  
  geom_point(size = 3,
             color = "#D55E00") +
  
  geom_errorbarh(
    aes(xmin = ci_low,
        xmax = ci_high),
    height = 0.2
  ) +
  
  scale_x_log10(
    breaks = c(0.2,0.5,1,2,5)
  ) +
  
  labs(
    title = "Site-Level Variation in NOM Selection",
    subtitle = "Hospital-specific Odds Ratios",
    x = "Odds Ratio",
    y = NULL
  ) +
  
  theme_minimal(base_size = 14)

print(p3)