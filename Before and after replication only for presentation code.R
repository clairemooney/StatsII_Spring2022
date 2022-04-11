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
library(dplyr)

#I have replicated the entire paper and discussed different models
#this was the one aspect and model I chose to discuss 
#I discussed in the presentation and the code for it
#I will upload the full replication

#setwd("")
data <- read.csv("abortion_canvass_data (2).csv", stringsAsFactors = FALSE)



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

make.balance.table(data$t4_respondent == 1, balance.vars, balance.vars.names,
                   "Covariate Balance among 3rd Post-Survey Respondents.")

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