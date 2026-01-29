library(survival)
library(survminer)
library(survMisc)
library(ggplot2)
library(dplyr)
library(lubridate)


data_path = "C:/Bre/SONOMA/Sonoma_cleaned.csv"

clean_s = read.csv(data_path, check.names = FALSE, row.names=1) 

#subsetting data to just NOM patients
NOM_patients <- clean_s[clean_s$initial_planoutcomes == 2,] 
NOM_patients$time_of_dxoutcomes

NOM_patients <- NOM_patients[!is.na(NOM_patients$time_of_dxoutcomes),] #removing NA values for date/time of patients dx of appendicitis
sum(is.na(NOM_patients$time_of_dxoutcomes)) #looking for NAs
NOM_patients$time_of_dxoutcomes <- gsub(" .*","", NOM_patients$time_of_dxoutcomes) #removing time part of date/time
sum(is.na(NOM_patients$time_of_dxoutcomes)) #checking for NAs again

#converting date_index to date object
NOM_patients$time_of_dxoutcomes <- as.Date(
  NOM_patients$time_of_dxoutcomes,
  format = "%m/%d/%Y"
)


#converting date of appendectomy after NOM to date object, converting blanks to NAs
NOM_patients$appy_int_dateoutcomes[
  NOM_patients$appy_int_dateoutcomes == ""
] <- NA

NOM_patients$appy_int_dateoutcomes <- as.Date(
  NOM_patients$appy_int_dateoutcomes,
  format = "%m/%d/%Y"
)


#creating variable that is time in days from NOM decision to appy
NOM_patients$time_to_appy <- NOM_patients$appy_int_dateoutcomes - NOM_patients$time_of_dxoutcomes
#I thought they were only followed for a year after, but many of these times look way past 365 days so what time for censored individuals?

#DONT NEED THIS IF WE GET NEW TIMES FOR TWO ERRORS
NOM_patients <- NOM_patients[NOM_patients$time_to_appy > 0 | is.na(NOM_patients$time_to_appy), ] #DONT NEED THIS IF WE GET NEW TIMES


#setting NA values to 0 for censored, creating status variable for survival
NOM_patients$status <- ifelse(
  is.na(NOM_patients$interval_typeoutcomes),
  0,   # censored
  NOM_patients$interval_typeoutcomes
)



NOM_patients$status <- as.numeric(NOM_patients$status)



#creating time_to_event for all NOM patients, making it 365 for censored individuals
NOM_patients$time_to_event <- ifelse(
  is.na(NOM_patients$time_to_appy),
  365,
  NOM_patients$time_to_appy
)
#administratively censoring those beyond 1 year
NOM_patients$status365 <- ifelse(
  NOM_patients$time_to_event > 365, 
  0, #administratively censored beyond 1 year
  NOM_patients$status
)

#changing time to event to 365 days for censored beyond 1 year individuals
NOM_patients$time_to_event <- ifelse(
  NOM_patients$status365 == 0, 
  365, 
  NOM_patients$time_to_event
)

NOM_patients$status <- factor(NOM_patients$status, 0:2, c("None", "Elective", "Urgent/Emergent"))

#time until recurrence of appendicitis symptoms after NOM

# #aalen-johansen estimator

library(mstate)
NOM_patients$event <- factor(NOM_patients$status365, 0:2, c("Censored", "Elective", "Urgent/Emergent"))
fit <- survfit(Surv(time_to_event, event) ~1, data = NOM_patients)
fit$transitions
plot(fit, xmax = 900, col = 1:2, lwd = 2,
     xlab = "Days from NOM", ylab = "Probability")
legend(8, .2, c("Elective", "Urgent/Emergent"), lty = 1, lwd = 2, col = 1:2, bty = 'n')




# km_fit <- survfit(dat_surv ~ 1, data = data, conf.type = "log-log")
# 
# ggsurvplot(km_fit, data = dat_surv, conf.int = T, ggtheme = theme_survminer(),
#            title = "Kaplan-Meier Survival Curve", xlab = "Time", ylab = "Survival Probability", palette = "lightblue")
# 
# na_fit <- survfit(dat_surv ~ 1, data = data, type = "aalen")



#POTENTIALLY NEEDED VARIABLES

# NOM_patients$time_of_dxoutcomes #date/time of  patients dx of appendicitis
# NOM_patients$nom_appyoutcomes #indicator if patient received appendectomy afterNOM yes(1), no (0)
# NOM_patients$interval_typeoutcomes #indicator of elective (1) or urgent/emergent (2) appy after NOM
# NOM_patients$ed_return_365outcomes #indicator if patient returned to ED w/ appendicitis symptoms yes(1) no(0)
# NOM_patients$ed_ret_dateoutcomes #date of first return to ED w/ appendicitis symptoms

