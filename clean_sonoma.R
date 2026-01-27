#### Script to clean SONOMA csv data ####



#### SETTINGS ####

# Put your path to the SONOMA csv here 
#(e.g. //biostat-fs2-s/users/sherold/Documents/SONOMA-Data/sonoma_raw.csv)
#Bre: "//biostat-fs2-s/users/bbrown34/Desktop/SONOMA_Interim0.csv"
data_path = "//biostat-fs2-s/users/bbrown34/Desktop/SONOMA_Interim0.csv"

# Put your path to where you want to save the cleaned data 
#(e.g. //biostat-fs2-s/users/sherold/Documents/SONOMA-Data/sonoma_cleaned.csv)
#Bre: "C:/Bre/SONOMA/Sonoma_cleaned.csv"
output_path = "C:/Bre/SONOMA/Sonoma_cleaned.csv"




#### LOAD ####
df = read.csv(data_path) 
library(dplyr)




#### CLEAN (COMMENT ON EVERY LINE PLEASE) ####

eligible <- df %>% filter(eligiblefull == 1)

# ----- CLEANING/CONDENSING DEMOGRAPHICS -------

#Race Condensing
clean_s <- eligible %>%
  mutate(
    race_ct = select( #creates variable called race_ct that sums up row counts for each race outcome
      .,
      raceoutcomes___1,
      raceoutcomes___2,
      raceoutcomes___3,
      raceoutcomes___4,
      raceoutcomes___5,
      raceoutcomes___6,
      raceoutcomes___7,
      raceoutcomes___8,
      raceoutcomes___9,
      raceoutcomes___10,
      raceoutcomes___11,
      raceoutcomes___12,
      raceoutcomes___13,
      raceoutcomes___14,
      raceoutcomes___88
    ) %>%
      rowSums(na.rm = TRUE),
    Race = case_when(
      race_ct > 1 ~ "Multiracial",
      raceoutcomes___1 == 1 ~ "Black/African American",
      raceoutcomes___2 == 1 ~ "White",
      raceoutcomes___3 == 1 ~ "AI/AN", #American Indian or Alaskan Native
      raceoutcomes___4 == 1 ~ "Asian Indian",
      raceoutcomes___5 |
        raceoutcomes___6 |
        raceoutcomes___7 |
        raceoutcomes___8 |
        raceoutcomes___9 | raceoutcomes___10 == 1 ~ "Asian", 
      raceoutcomes___11 |
        raceoutcomes___12 |
        raceoutcomes___13 | raceoutcomes___14 ==  1 ~ "NHPI", #Native Hawaiian or Pacific Islander
      raceoutcomes___88 == 1 ~ "Other/Not Specified"
    )
  ) %>%
  
  select( #removes original raceoutcomes cols
    -raceoutcomes___1,
    -raceoutcomes___2,
    -raceoutcomes___3,
    -raceoutcomes___4,
    -raceoutcomes___5,
    -raceoutcomes___6,
    -raceoutcomes___7,
    -raceoutcomes___8,
    -raceoutcomes___9,
    -raceoutcomes___10,
    -raceoutcomes___11,
    -raceoutcomes___12,
    -raceoutcomes___13,
    -raceoutcomes___14,
    -raceoutcomes___88,
    -other_raceoutcomes
  )

#Sex condensing
clean_s <- clean_s %>%
  mutate(
    Sex = case_when(
      sexoutcomes == 1 ~ "Male",
      sexoutcomes == 2 ~ "Female",
      sexoutcomes == 3 ~ "Intersex",
      sexoutcomes == 4 | sexoutcomes == 5 ~ "Other/Unknown"
    )
  ) %>%
  select(-sexoutcomes,-genderidother, -sex_other) #removes unneeded sex/gender columns

#Ethnicity Condensing
clean_s <- clean_s %>%
  mutate(
    Ethnicity = case_when(
      ethnicityoutcomes == 1 ~ "Not Hispanic",
      ethnicityoutcomes == 2 |
        ethnicityoutcomes == 3 |
        ethnicityoutcomes == 4 |
        ethnicityoutcomes == 5 ~ "Hispanic/Latino/Spanish Origin",
      ethnicityoutcomes == 88 ~ "Other/Unknown"
    )
  ) %>% select(-ethnicityoutcomes) #removes original ethnicityoutcomes


#Combining Race and Ethnicity - EDA showed trend of individuals putting "other" for race but checking hispanic, leading to overinflated Other/unspecified for Race variable

clean_s <- clean_s %>%
  mutate(
    RaceEthnicity = case_when(
      #White or Other/Unspecified → use Hispanic value (Can change this distinction)
      Race == "White" & Ethnicity == "Hispanic/Latino/Spanish Origin" ~ "Hispanic/Latino/Spanish Origin",
      Race == "Other/Not Specified" & Ethnicity == "Hispanic/Latino/Spanish Origin" ~ "Hispanic/Latino/Spanish Origin",
      Race == "White" & Ethnicity != "Hispanic/Latino/Spanish Origin" ~ "Non-Hispanic White",
      #Any non‑white race AND Hispanic → Multiracial
      Ethnicity == "Hispanic/Latino/Spanish Origin"  ~ "Multiracial",
      
      #Otherwise keep race
      TRUE ~ Race
    )
  )

clean_s <- clean_s %>%
  mutate(
    RaceEthnicity = case_when( #condensing races that had <30 participants into Other/Not Specified
      Race == "AI/AN" | Race == "Asian Indian" | Race == "NHPI" ~ "Other/Not Specified",
      #all other race distinctions stay the same
      TRUE ~ RaceEthnicity
    )
  )
      
      



#Indicator of ESL (Yes = ESL, No = Eng First Lang)
clean_s <- clean_s %>%
  mutate(
    ESL = case_when(
      esloutcomes == 0 ~ "No",
      esloutcomes == 1 | #any other language as first lang. means ESL -> yes
        esloutcomes == 2 | 
        esloutcomes == 3 |
        esloutcomes == 4 |
        esloutcomes == 5 |
        esloutcomes == 6 |
        esloutcomes == 7 |
        esloutcomes == 8 | esloutcomes == 9 | esloutcomes == 88 ~ "Yes"
    )
  )

#Primary Language (English/Spanish/Other)
clean_s <- clean_s %>%
  mutate(
    primary_language = case_when(
      esloutcomes == 0 ~ "English",
      esloutcomes == 1 ~ "Spanish",
      esloutcomes == 2 |  #all other languages are categorized as other
        esloutcomes == 3 |
        esloutcomes == 4 |
        esloutcomes == 5 |
        esloutcomes == 6 |
        esloutcomes == 7 |
        esloutcomes == 8 | esloutcomes == 9 | esloutcomes == 88 ~ "Other"
    )
  ) %>% select(-esloutcomes,-esl_otheroutcomes)

#Removing site abstractor as we don't need it for analysis
clean_s <- clean_s %>%
  select(
    -sitesite_abstractor,
    -atrium_site_specificsite_abstractor,
    -uw_site_specificsite_abstractor,
    -uth_lbj_subsite
  )

#Age Categories
clean_s <- clean_s %>%
  mutate(
    Age_Categories = case_when(
      ageoutcomes >= 18 & ageoutcomes <= 24 ~ "18-24",
      ageoutcomes >= 25 & ageoutcomes <= 34 ~ "25-34",
      ageoutcomes >= 35 & ageoutcomes <= 44 ~ "35-44",
      ageoutcomes >= 45 & ageoutcomes <= 54 ~ "45-54",
      ageoutcomes >= 55 & ageoutcomes <= 64 ~ "55-64",
      ageoutcomes >= 65 & ageoutcomes <= 74 ~ "65-74",
      ageoutcomes >= 75  ~ "75+"
  ))


##abnormal vitals
#create a binary text indicator for abnormal vitals (Fever, Tachycardia, Hypotension)
clean_s <- clean_s %>%
  mutate(
    abnormal_vital_indicator = if_any(
      c(
        abnormal_vitalsoutcomes___1, # Fever: Temp > 38 C 
        abnormal_vitalsoutcomes___2, # Tachycardia: HR > 100 bpm 
        abnormal_vitalsoutcomes___3, # Hypotension: SBP < 90 / DBP < 60 
        abnormal_vitalsoutcomes___4, # Tachypnea: RR > 20 bpm 
        abnormal_vitalsoutcomes___5  # Bradycardia: HR < 50 bpm 
      ),
      ~ .x == 1
    ),
    abnormal_vital_indicator = if_else(abnormal_vital_indicator, "Yes", "No", missing = "No")
  )

##Imaging Centered
#consolidate imaging findings from both CT and Ultrasound into patient-level text indicators
clean_s <- clean_s %>%
  mutate(
    # Phlegmon (Inflammatory Mass) 
    # Merge findings: 1=Yes is positive; 0=No/2=Not Mentioned are negative 
    phlegmon_combined = case_when(
      phlegmonoutcomes == 1 | us_phlegoutcomes == 1 ~ "Yes",
      phlegmonoutcomes %in% c(0, 2) & (us_phlegoutcomes %in% c(0, 2) | is.na(us_phlegoutcomes)) ~ "No",
      phlegmonoutcomes == 3 | us_phlegoutcomes == 3 ~ NA_character_, # Mark "Ambiguous" as NA
      TRUE ~ "No"
    ),
    
    #  Perforation
    # Merge findings: 1=Yes/2=Micro-perf are positive; 0=No/3=Not Mentioned are negative 
    perf_combined = case_when(
      perfoutcomes %in% c(1, 2) | us_perfoutcomes == 1 ~ "Yes",
      perfoutcomes %in% c(0, 3) & (us_perfoutcomes %in% c(0, 2) | is.na(us_perfoutcomes)) ~ "No",
      perfoutcomes == 4 | us_perfoutcomes == 3 ~ NA_character_, 
      TRUE ~ "No"
    ),
    
    #Abscess
    # Merge findings: 1=Yes is positive; 0=No/2=Not Mentioned are negative 
    abscess_combined = case_when(
      abscessoutcomes == 1 | us_abscoutcomes == 1 ~ "Yes",
      abscessoutcomes %in% c(0, 2) & (us_abscoutcomes %in% c(0, 2) | is.na(us_abscoutcomes)) ~ "No",
      abscessoutcomes == 3 | us_abscoutcomes == 3 ~ NA_character_, 
      TRUE ~ "No"
    )
  )

###Relocates new columns to front
clean_s <- clean_s %>%
  relocate(record_id, 
           Race, Sex, 
           Ethnicity, 
           RaceEthnicity,
           ESL, 
           primary_language, 
           Age_Categories,
           abnormal_vital_indicator,        
           phlegmon_combined,               
           perf_combined,                 
           abscess_combined
           )

#### SAVE ####

write.csv(clean_s, output_path)
