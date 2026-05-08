#### Script to clean SONOMA csv data ####



#### SETTINGS ####

# Put your path to the SONOMA csv here 
#(e.g. //biostat-fs2-s/users/sherold/Documents/SONOMA-Data/sonoma_raw.csv)
#Bre: "//biostat-fs2-s/users/bbrown34/Desktop/SONOMA_Interim0.csv"
data_path = ""
# Put your path to where you want to save the cleaned data 
#(e.g. //biostat-fs2-s/users/sherold/Documents/SONOMA-Data/sonoma_cleaned.csv)
#Bre: "C:/Bre/SONOMA/Sonoma_cleaned.csv"
output_path = ""




#### LOAD ####
df = read.csv(data_path) 
library(dplyr)
library(tidyverse)
library(tidyr)




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


#Expand sub-sites (see codebook) and add site names
clean_s = clean_s %>% 
  mutate(
    site_name = case_when(sitesite_abstractor == 9 & uw_site_specificsite_abstractor == 1 ~ "Harborview Medical Center",
                          sitesite_abstractor == 9 & uw_site_specificsite_abstractor == 2 ~ "University of Washington-Montlake",
                          sitesite_abstractor == 9 & uw_site_specificsite_abstractor == 3 ~ "Northwest Hospital-University of Washington",
                          sitesite_abstractor == 1 & atrium_site_specificsite_abstractor == 1 ~ "Atrium Health",
                          sitesite_abstractor == 1 & atrium_site_specificsite_abstractor == 2 ~ "Atrium Health",
                          sitesite_abstractor == 5 & uth_lbj_subsite == 1 ~ "Lyndon B Johnson Hospital",
                          sitesite_abstractor == 5 & uth_lbj_subsite == 2 ~ "UT Health Science Center",
                          sitesite_abstractor == 2 ~ "Boston Medical Center",
                          sitesite_abstractor == 3 ~ "Columbia University Medical Center",
                          sitesite_abstractor == 4 ~ "Grady Health System",
                          sitesite_abstractor == 6 ~ "Michigan Medicine",
                          sitesite_abstractor == 7 ~ "Northwestern Medicine",
                          sitesite_abstractor == 8 ~ "University of Iowa Hospital and Clinics",
                          sitesite_abstractor == 10 ~ "Medical University of South Carolina",
                          sitesite_abstractor == 11 ~ "Kaiser Permanente"))

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


#CCI will be grouped into 0, 1-3, and >3
clean_s$ccioutcomes <- round(as.numeric(clean_s$ccioutcomes))

clean_s <- clean_s %>% 
  mutate(
  ccioutcomes = case_when(
    ccioutcomes > 3 ~ ">3", 
    ccioutcomes == 1 | ccioutcomes == 2 | ccioutcomes == 3 ~ "1-3", 
    ccioutcomes == 0 ~ "0"
  )
)

#merge responses 0 and 2 into 0 for appendicolith outcomes
clean_s$appendicolithoutcomes <- ifelse(
  clean_s$appendicolithoutcomes == 1, 
  1,
  0
)

#additional factors
clean_s <- clean_s %>% mutate(plan = factor(initial_planoutcomes, levels = c(1, 2), labels = c("Appendectomy", "NOM")),
                            complicated_in_or = case_when(
                              or_upgradeoutcomes == 0 ~ "No Upgrade",
                              or_upgradeoutcomes > 0 ~ "Complicated/Possible Complicated Appendicitis in OR",
                              TRUE ~ NA),
                            more_than_minimal = case_when(
                              ct_fluidoutcomes > 1 | us_fluidoutcomes > 1 ~ TRUE,
                              TRUE ~ FALSE),
                            minimal_or_more = case_when(
                              ct_fluidoutcomes >= 1 | us_fluidoutcomes >= 1 ~ TRUE,
                              TRUE ~ FALSE),
                            coda_site_nw = case_when(
                              site_name == "Atrium Health" | site_name == "Grady Health System" | site_name == "Northwest Hospital-University of Washington" ~ FALSE,
                              TRUE ~ TRUE),
                            coda_site = case_when(
                              site_name == "Atrium Health" | site_name == "Grady Health System" ~ FALSE,
                              TRUE ~ TRUE),
                            appy_tta = difftime(as.Date(clean_s$appy_dateoutcomes), as.Date(clean_s$firstdxoutcomes), units = 'days'),
                            nom_tta = difftime(as.Date(clean_s$appy_int_dateoutcomes), as.Date(clean_s$firstdxoutcomes), units = 'days'),
                            time_of_day = format(as.POSIXct(clean_s$time_of_dxoutcomes, format = "%Y-%m-%d %H:%M"), "%H:%M"),
                            hour = hour(hm(time_of_day)),
                            night = case_when(
                              hour >= 18 | hour < 6 ~ TRUE,
                              hour < 18 & hour >= 6 ~ FALSE),
                            preference = case_when(
                              surg_teamoutcomes == 1 ~ "Antibiotics",
                              surg_teamoutcomes == 2 ~ "Appendectomy",
                              surg_teamoutcomes == 0 | surg_teamoutcomes == 3 ~ "No/Unclear Preference"),
                            symptom_driven_appy = case_when(
                              reason_electiveoutcomes___1 == 1 | reason_electiveoutcomes___2 == 1 | 
                                reason_urgentoutcomes___1 == 1 | reason_urgentoutcomes___2 == 1 ~ TRUE,
                              TRUE ~ FALSE),
                            symptom_driven_appy_nom = case_when(
                              symptom_driven_appy == TRUE & nom_appyoutcomes == TRUE ~ TRUE,
                              symptom_driven_appy == FALSE & nom_appyoutcomes == TRUE ~ FALSE,
                              TRUE ~ NA),
                            ed_reasonoutcomes_f = case_when(
                              ed_reasonoutcomes <= 2 ~ "Recurrent/Continued Appendicitis",
                              TRUE ~ "Other Reasons"),
                            early = case_when(
                              nom_tta <= 30 ~ TRUE,
                              TRUE ~ FALSE),
                            mid = case_when(
                              nom_tta <= 90 ~ TRUE,
                              TRUE ~ FALSE),
                            largest_size = pmax(appy_sizeoutcomes, us_diameteroutcomes, na.rm = TRUE),
                            any_lith = case_when(
                              appendicolithoutcomes == 1 | us_lithoutcomes == 1 ~ TRUE,
                              TRUE ~ FALSE),
                            male = Sex == 'Male',
                            symptoms = as.numeric(symptomsoutcomes),
                            #nlr = as.numeric(nlroutcomes), change to 
                            nlr = readr::parse_number(stringr::str_replace(nlroutcomes, ",", ".")),
                            wbc = as.numeric(wbcoutcomes),
                            bmi = as.numeric(bmioutcomes),
                            nlr_range = case_when(
                              nlr < 5 ~ "0-5",
                              nlr >= 5 & nlr < 10 ~ "5-10",
                              nlr >= 10 & nlr < 20 ~ "10-20",
                              nlr >= 20 ~ "20+",
                              TRUE ~ NA),
                            low_nlr = nlr <= 10)


clean_s <- clean_s %>%
  rename(RecordID = record_id) #when I download a new dataset, it switches record_id to this for some reason - 
#please comment out if it doesn't reply, and also change rename to correct record id indicator to make it easier to switch


###Relocates new columns to front
clean_s <- clean_s %>%
  relocate(RecordID, 
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

