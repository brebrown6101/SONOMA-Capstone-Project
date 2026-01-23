#### Script to clean SONOMA csv data ####



#### SETTINGS ####

# Put your path to the SONOMA csv here 
#(e.g. //biostat-fs2-s/users/sherold/Documents/SONOMA-Data/sonoma_raw.csv)
#Bre: "//biostat-fs2-s/users/bbrown34/Desktop/SONOMA_Interim0.csv"
data_path = 

# Put your path to where you want to save the cleaned data 
#(e.g. //biostat-fs2-s/users/sherold/Documents/SONOMA-Data/sonoma_cleaned.csv)
#Bre: "C:/Bre/SONOMA/Sonoma_cleaned.csv"
output_path = 




#### LOAD ####
df = read.csv(data_path) 
library(dplyr)




#### CLEAN (COMMENT ON EVERY LINE PLEASE) ####

eligible <- df %>% filter(eligiblefull == 1)

# ----- CLEANING/CONDENSING DEMOGRAPHICS -------

#Race Condensing
clean_s <- eligible %>%
  mutate(
    race_ct = select(
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

#Indicator of ESL (Yes = ESL, No = Eng First Lang)
clean_s <- clean_s %>%
  mutate(
    ESL = case_when(
      esloutcomes == 0 ~ "No",
      esloutcomes == 1 |
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
      esloutcomes == 2 |
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

#Relocates new columns to front
clean_s <- clean_s %>%
  relocate(record_id, Race, Sex, Ethnicity, ESL, primary_language, Age_Categories)

#### SAVE ####

write.csv(clean_s, output_path)
