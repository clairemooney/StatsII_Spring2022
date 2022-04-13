# set up with packages for replication
library(knitr)
library(pander)
library(bookdown)
library(captioner)
library(sandwich)
library(lmtest)
library(kableExtra)
library(ggplot2)
library(tidyverse)
library(plyr)
library(ebal)

#the original code for the replication was in an r markdown file so I had
#to change it into an R file and run the code 

#setwd("")
data <- read.csv("abortion_canvass_data.csv", stringsAsFactors = FALSE)

#Additional Survey details 
#This is respresentativeness of experiment at each stage
assess.representativeness <- function(df) c(mean(df$vf_female), mean(df$vf_age),
                                            mean(df$vf_white), nrow(df))

starting.universe <- t(c("Starting", assess.representativeness(data)))
t0.sample <- t(c("Baseline Resp.", assess.representativeness(subset(data, data$t0_respondent == 1))))
t1.sample <- t(c("2nd Baseline Resp.", assess.representativeness(subset(data, data$t1_respondent == 1))))
canvassed.sample <- t(c("Canvassed", assess.representativeness(subset(data, data$canvassed == 1))))
t2.sample <- t(c("1 Wk Resp.", assess.representativeness(subset(data, data$t2_respondent == 1))))
t3.sample <- t(c("1 Mo Resp.", assess.representativeness(subset(data, data$t3_respondent == 1))))
t4.sample <- t(c("3 Mo Resp.", assess.representativeness(subset(data, data$t4_respondent == 1))))

representativeness <- rbind(starting.universe, t0.sample, t1.sample, canvassed.sample, 
                            t2.sample, t3.sample, t4.sample)
colnames(representativeness) <- c("Sample", "Female", "Age", "White", "N")
representativeness[, 2:4] <- round(as.numeric(representativeness[, 2:4]), 2)

kable(representativeness, caption = "Representativeness of Experiment at Each Stage")  %>%
  kable_styling(latex_options = c("striped", "hold_position"))


### Average Treatment Effects

t0.covariate.names <- c('t0_ideology', 't0_pid7',
                        't0_allow_6weeks', 't0_allow_trimester',
                        't0_allow_no_birthcontrol', 't0_allow_partner',
                        't0_allow_already', 't0_allow_anyreason',
                        't0_law_appointment', 't0_law_private_insur',
                        't0_law_counsel', 't0_law_public_insur',
                        't0_abortion_something_wrong', 't0_abortion_badly',
                        't0_abortion_consider', 't0_abortion_birthcontrol',
                        't0_abortion_nothing_wrong', 't0_act_volunteer_clinic',
                        't0_act_accompany', 't0_act_protest_clinic',
                        't0_act_congress_support', 't0_act_congress_opposition',
                        't0_ballot_abortion', 't0_therm_pp',
                        't0_therm_women_abort', 't1_law_appointment', 't1_law_public_insur',
                        't1_abortion_badly', 't1_abortion_consider', 't1_abortion_birthcontrol',
                        't1_ballot_abortion', 't1_abortion_always_legal',
                        't1_college_educ', 't1_relig_very_important',
                        't1_pid7', 't1_ideology',
                        't0_therm_lgbt', 't0_therm_pharma',
                        't0_therm_nra', 't0_therm_afam',
                        't0_therm_gun_owners', 'vf_female',
                        'vf_age', 't1_respondent')

x <- data[,c(t0.covariate.names)]
x <- as.matrix(x, dimnames = list(NULL, names(x)))

  
  # Tests of Design Assumptions
  
  ### Covariate Balance among All Subjects, Compliers, and Reporters
  

  balance.vars <- c('t0_abortion_factor', 't0_pid7', 't0_therm_pp',
                    't0_therm_women_abort', 't0_therm_afam',
                    'vf_female', 'vf_age',
                    't1_respondent')
  balance.vars.names <- c("Baseline Abortion Factor",
                          "Party ID",
                          "Planned Parenthood Therm",
                          "Women Who Had Abortions Therm",
                          "African American Therm",
                          "Female",
                          "Age",
                          "2nd Baseline Respondent")
  
  make.balance.table <- function(subset, varlist, names, caption){
    file <- subset(data, subset)
    balance <- matrix(ncol=2, nrow=length(varlist)+1)
    for(i in 1:length(varlist)){
      balance[i,1] <- mean(file[file$treat == 0,balance.vars[i]])
      balance[i,2] <- mean(file[file$treat == 1,balance.vars[i]])
    }
    balance[length(varlist)+1,1] <- nrow(file[file$treat==0,])
    balance[length(varlist)+1,2] <- nrow(file[file$treat==1,])
    balance <- data.frame(round(balance, digits=2))
    rownames(balance) <- c(names, "N")
    anova.test.vector <- matrix(ncol=1, nrow=nrow(balance))
    for(i in 1:length(varlist)){
      anova.test.vector[i,1] <- round(summary(aov(file[,varlist[i]] ~ 
                                                    as.factor(treat), data = file))[[1]][["Pr(>F)"]][[1]], 2)
    }
    anova.test.vector[nrow(balance),1] <- "-"
    balance <- cbind(balance,anova.test.vector)
    colnames(balance) <- c("Placebo", "Treat", "p-value")
    return(kable(balance, caption = caption)  %>%
             kable_styling(latex_options = c("striped", "HOLD_position")))
  } 
  
#Covariate Balance among Pre-Survey Respondents.
  make.balance.table(data$t0_respondent == 1 & !is.na(data$treat), balance.vars, balance.vars.names,
                     "Covariate Balance among Pre-Survey Respondents.")
  
#Covariate Balance among Compliers.
  
  make.balance.table(data$canvassed == 1, balance.vars, balance.vars.names,
                     "Covariate Balance among Compliers.")
  
  #Covariate Balance among 1st Post-Survey Respondents.
  
  make.balance.table(data$t2_respondent == 1, balance.vars, balance.vars.names,
                     "Covariate Balance among 1st Post-Survey Respondents.")
  
  #Covariate Balance among 2nd Post-Survey Respondents
  
  make.balance.table(data$t3_respondent == 1, balance.vars, balance.vars.names,
                     "Covariate Balance among 2nd Post-Survey Respondents.")
  
#Covariate Balance among 3rd Post-Survey Respondents.
  
  make.balance.table(data$t4_respondent == 1, balance.vars, balance.vars.names,
                     "Covariate Balance among 3rd Post-Survey Respondents.")
  
#Replication to force the data to age for females under 50 
#the average age is now 39.18 
#this changes the values from the 3rd post survey respondents 

  
  below50 <- filter(data, vf_age < 50)
  
  make.balance.table <- function(subset, varlist, names, caption){
    file <- subset(below50, subset)
    balance <- matrix(ncol=2, nrow=length(varlist)+1)
    for(i in 1:length(varlist)){
      balance[i,1] <- mean(file[file$treat == 0,balance.vars[i]])
      balance[i,2] <- mean(file[file$treat == 1,balance.vars[i]])
    }
    balance[length(varlist)+1,1] <- nrow(file[file$treat==0,])
    balance[length(varlist)+1,2] <- nrow(file[file$treat==1,])
    balance <- data.frame(round(balance, digits=2))
    rownames(balance) <- c(names, "N")
    anova.test.vector <- matrix(ncol=1, nrow=nrow(balance))
    for(i in 1:length(varlist)){
      anova.test.vector[i,1] <- round(summary(aov(file[,varlist[i]] ~ 
                                                    as.factor(treat), data = file))[[1]][["Pr(>F)"]][[1]], 2)
    }
    anova.test.vector[nrow(balance),1] <- "-"
    balance <- cbind(balance,anova.test.vector)
    colnames(balance) <- c("Placebo", "Treat", "p-value")
    return(kable(balance, caption = caption)  %>%
             kable_styling(latex_options = c("striped", "HOLD_position")))
  } 

  make.balance.table(below50$t4_respondent == 1, balance.vars, balance.vars.names,
                     "Covariate Balance among 3rd Post-Survey Respondents.")
#Survey Attrition 
  
  t2.respondents <- summary(lm(t2_respondent ~ treat, data = data))$coef[2,]
  t3.respondents <- summary(lm(t3_respondent ~ treat, data = data))$coef[2,]
  t4.respondents <- summary(lm(t4_respondent ~ treat, data = data))$coef[2,]
  
  overall.attrition <- data.frame(round(rbind(t2.respondents, t3.respondents, t4.respondents), 3))
  names(overall.attrition) <- c("Effect", "SE", "t.stat", "p")
  overall.attrition <- as.matrix(overall.attrition)
  rownames(overall.attrition) <- rep(c("Treat"), 3)
  
  
  kable(overall.attrition, digits=2, caption = "Test for differential attrition") %>%
    pack_rows("1 Week", 1, 1) %>%
    pack_rows("1 Month", 2, 2) %>%
    pack_rows("3 Months", 3, 3) %>%
    kable_styling(latex_options = c("striped", "HOLD_position"))
  

  

