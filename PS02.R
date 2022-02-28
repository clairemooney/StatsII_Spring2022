#climatesupport <- read.csv("datasets/climateSupport.RData")

install.packages("ggplot2")
library("ggplot2")

climateSupport <- load(url("https://github.com/ASDS-TCD/StatsII_Spring2022/blob/main/datasets/climateSupport.RData?raw=true"))

load(url("https://github.com/ASDS-TCD/StatsII_Spring2022/blob/main/datasets/climateSupport.RData?raw=true"))

#Question 1          

summary(climateSupport)

# Another option: coercing hsgrad from a character vector to a logical vector
climateSupport$choice <- as.numeric(as.factor(climateSupport$choice))-1
climateSupport$sanctions <- as.numeric(as.factor(climateSupport$sanctions))-1
climateSupport$countries <- as.numeric(as.factor(climateSupport$countries))-1

climateSupport$choice

#sanctions line one is 15%
#0 means 
#1 means 
#2 
climateSupport

model_1 <- glm(choice ~.,  #period functions as omnibus selector# (kitchen sink additive model)
               data = climateSupport, 
               family = "binomial"(link = "logit")))
summary(model_1)
#-0,14458 intercept
#countries 0.32436
#-0.12353 sanctions 

plot(model_1)

exp(coef(model_1))
#returns 
#intercept = 0.8653845
#countries = 1.03831405
#sanctions = 0.8837921



#countries 
#0 20
#1 80
#2 160

#sanctions
# 0 is 5 
#1 15
#2 20 



#model_null <- glm(as.factor(choice) ~ 1, data = climateSupport, family = "binomial") # 1 = fit an intercept only (i.e. sort of a "mean")
anova(model_1, test = "Chisq")
#running an anova of model one 

anova(model_1)

exp(confint(model_1)) # Transform to odds ratio using exp()

# An option for making a data.frame of conf ints and coefficients
conf_reg <- data.frame(cbind(lower = exp(confint(model_1)[,1]), 
                             coefs = exp(coef(model_1)), 
                             upper = exp(confint(model_1)[,2])))

ggplot(data = conf_reg, mapping = aes(x = row.names(conf_reg), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") + 
  coord_flip()

#A
#above 160 use number 2 
#5 use 1 
#15 use 2 
Y = bO + b1x1 + b1X2 + X3 
Y = -0.14458 + 0.32436*countries + -0.12353*sanctions 
Y <- -0.14458 + 0.032436*2 + -0.12353*1
Y1 <- -0.14458 + 0.032436*2 + -0.12353*2
DIFF <- Y1 - Y
DIFF
#0.12353 Y - Y1 = 0.12353
#-0.12353 when diff = Y1 - Y
exp(0.12353)
#exp(0.12353) = 1.131484
#for the policy for when nearly all countries participate and increasing sanctions
#from 5 to 15% percent it changes the log odds of support for the policy by 1.131484
#from the basline odds ratio 
#this suggests that the more countries that participate the more the log odds
#are affected

#B
#20 OF 192 
#5-15 use 1 

Y2 = bO + b1x1 + b1X2 + X3 
# -0.14458 + 0.32436*countries + -0.12353*sanctions
Y2 <- -0.14458 + 0.032436*0 + -0.12353*1
Y3 <- -0.14458 + 0.032436*0 + -0.12353*2
DIFF2 <- Y3 - Y2
DIFF2
#DIFF Y3 -Y2 = -0.12353 
#DIFF2 Y2 - Y3 = 0.012353
exp(-0.12353)
#exp(-0.12353) = 0.8837951
#for the policy for when very few countries participate and there is the same 
#increase in policy from 5 to 15 percent it changes the log odds that they will 
#support the policy to 0.8837951 from the basline odds ratio. 
#This may suggest that sampling fewer countries perhaps if they are all of a certain economic status may
#have an impact on support

#C 
#Y = bO + b1x1 + b1X2 + X3 
# -0.14458 + 0.32436*countries + -0.12353*sanctions + choice*countries*sanctions
Y8 <- -0.14458 + 0.32436*1 + -0.12353*0 + 1*1
Y8
#Y8 is equal to 1.17978
exp(1.17978)
#exp(1.17978) = 3.253658
#the estimated probability that if there is no account taken for sanctions 
#that an individual will support policy has a log odds that is much higher
#of 3.252658 this suggests that sanctions do play a part in peoples support 
#for policy 



#D 
#Y = -0.14458 + 0.32436*countries + -0.12353*sanctions + choice*countries*sanctions
Y4 <- -0.14458 + 0.032436*2 + -0.12353*1 + 0*2*1
Y5 <- -0.14458 + 0.032436*2 + -0.12353*2 + 1*2*1
DIFF3 <- Y5 - Y4
DIFF3
#1.87647
exp(1.87647)
#exp 1.18647 = 6.530412

#Y = -0.14458 + 0.32436*countries + -0.12353*sanctions + choice*countries*sanctions
Y6 <- -0.14458 + 0.032436*0 + -0.12353*1 + 0*0*1
Y7 <- -0.14458 + 0.032436*0 + -0.12353*2 + 1*0*2
DIFF4 <- Y7 - Y6
DIFF4
#-0.12353
#exp(-0.12353) = 0.8837951


#difference 3 changed to the initial 2a however difference 4 did not change to 
#2B thing was because when the interaction terms were included when choice is 
#equal to zero

plot1 <- interaction.plot(model_1, pred = countries, modx = sanctions)

predicted_data <- with(climateSupport, expand.grid(choice = unique(choice),
                                               countries = unique(countries),
                                               sanctions = unique(sanctions)))

predicted_data <- cbind(predicted_data, predict(model_1, 
                                                newdata = predicted_data,
                                                type = "response",
                                                se = TRUE))

predicted_data


predicted_data <- within(predicted_data,
                         {
                           PredictedProb <- plogis(fit)
                           LL <- plogis(fit - (1.96 * se.fit))
                           UL <- plogis(fit + (1.96 * se.fit))
                         })

predicted_data