#QUESTION 1

library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)
library(joineR)
library(MASS)
library(lattice)
library(geepack)
library(multcomp)
library(msm)
library(lme4)

#Using GLMM for random intercept of site

# model1 <- glmer(
#   response ~ age + sex + language + race + ethnicity + appendicolith + NLR + CCI + diameter +  (1 | site),
#   data = data,
#   family = binomial
# )

#Forest plot

# coefs <- tidy(model1, conf.int = TRUE, exponentiate = TRUE)
# 
# 
# plot1 <- ggplot(coefs, aes(x = estimate, y = term)) +
#   geom_point(size = 2) +
#   geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
#   geom_vline(xintercept = 1, linetype = "dashed") +
#   scale_x_log10() +
#   labs(
#     x = "Odds ratio (exp(coef))",
#     y = NULL,
#     title = "Insert Title"
#   ) +
#   theme_minimal(base_size = 14)






#QUESTION 2


# library(foreign)
# library(nnet)
# library(ggplot2)
# library(reshape2)
# 
# data$outcome <- relevel(data$outcome, ref = "no_appendectomy")
# 
# 
# model2 <- multinom(outcome ~ age + sex + language + race + ethnicity + appendicolith + NLR + CCI + diameter + site, data = data)



#QUESTION 3
library(dplyr)
library(lubridate) #can use exact dates
library(MASS)

#create month index variable. (1,2,3..etc months into study for each site)

# months_since_appy = as.numeric(interval(appy_imp_date, date), "months")
#date is the date t (do we have a column that is the running monthly date?)


#glm.nb(NOM_total ~ time + site + total_cases_per_month + months_since_appy)

#QUESTION 4

library(pROC)
library(ggplot2)

# roc_analysis <- roc(response = complication_status, predictor = NLR, data = data)
# auc_value <- auc(roc_analysis)
# print(auc_value)
# 
# #bootstrapped CI for AUC
# ci_bootstrap <- ci.auc(roc_analysis, method = "bootstrap", boot.n = 2000)
# print(ci_bootstrap)
# 
# 
# ggroc(roc_analysis, legacy.axes = TRUE) +
#   labs(x = 'False-positive rate', y = 'True-positive rate', title = 'ROC Curve') +
#   annotate('text', x = .5, y = .5, label = paste0('AUC: ', round(auc_value, digits = 4)))
# 
# #threshold value
# optimal_cutoff <- coords(roc_analysis, ret = "threshold", transpose = FALSE) #default best.method is youden
# print(optimal_cutoff)
# 
# #sensitivity and specificity levels for that threshold value
# optimal_coords <- coords(roc_analysis, ret = c("threshold", "specificity", "sensitivity"), transpose = FALSE)
# print(optimal_coords)
# 
# cutoff_model <- glm(complication_status ~ optimal_cutoff, family = "binomial")
# 
# 4.5b) 
# 
# generalized_model <- glm(appendectomy_status ~ optimal_cutoff, family = "binomial")



#QUESTION 5

library(survival)
library(survminer)
library(survMisc)
library(ggplot2)
library(dplyr)

#time until recurrence of appendicitis symptoms after NOM
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
# 


