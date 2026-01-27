library(survival)
library(survminer)
library(survMisc)
library(ggplot2)
library(dplyr)
library(lubridate)

#subsetting data to just NOM patients
NOM_patients <- clean_s[clean_s$initial_planoutcomes == 2,] 


#POTENTIALLY NEEDED VARIABLES

# NOM_patients$time_of_dxoutcomes #date/time of  patients dx of appendicitis
# NOM_patients$nom_appyoutcomes #indicator if patient received appendectomy afterNOM yes(1), no (0)
# NOM_patients$interval_typeoutcomes #indicator of elective (1) or urgent/emergent (2) appy after NOM
# NOM_patients$ed_return_365outcomes #indicator if patient returned to ED w/ appendicitis symptoms yes(1) no(0)
# NOM_patients$ed_ret_dateoutcomes #date of first return to ED w/ appendicitis symptoms




#time until recurrence of appendicitis symptoms after NOM

# Filter for 1s and remove NA values in nom_appyoutcomes. Further subsetting to just patients who received appy after NOM
appy_after_nom_pts <- NOM_patients[NOM_patients$nom_appyoutcomes == 1 & !is.na(NOM_patients$nom_appyoutcomes), ]


appy_after_nom_pts$time_of_dxoutcomes #date/time of  patients dx of appendicitis
sum(is.na(appy_after_nom_pts$time_of_dxoutcomes)) #looking for NAs
date_index <- gsub(" .*","", appy_after_nom_pts$time_of_dxoutcomes) #removing time part of date/time
sum(is.na(appy_after_nom_pts$time_of_dxoutcomes)) #checking we have the same amount of NAs
#converting date_index to date object
date_index <- as.Date(appy_after_nom_pts$time_of_dxoutcomes, format = "%m/%d/%Y")
date_index

#converting date of appendectomy after initial primary treatment with antibiotics to date object
appy_time <- as.Date(appy_after_nom_pts$appy_int_dateoutcomes, format = "%m/%d/%Y") 
appy_time <- appy_time[!is.na(appy_time)]
appy_time #LET GRIFFEN KNOW -> 2015-05-01, also time where appendectomy was recorded of happening before NOM appointment
#505 (1951-103) and 1655 (1960-5)

#creating variable that is time in days from NOM decision to appy
appy_after_nom_pts$time_to_appy <- appy_time - date_index
appy_after_nom_pts$time_to_appy








# dat_surv <- Surv(time = data$time, event = data$event)
# km_fit <- survfit(dat_surv ~ 1, data = data, conf.type = "log-log")
# 
# ggsurvplot(km_fit, data = dat_surv, conf.int = T, ggtheme = theme_survminer(),
#            title = "Kaplan-Meier Survival Curve", xlab = "Time", ylab = "Survival Probability", palette = "lightblue")
# 
# na_fit <- survfit(dat_surv ~ 1, data = data, type = "aalen")
# 
# 
# #aalen-johansen estimator
# data$event <- factor(data$event, levels = c(0:2), 
#                      labels = c("Elective Appendectomy", "Non-elective appendectomy"))
# 
# km_fit <- survfit(dat_surv ~ 1, data = data, conf.type = "log-log")
# 
