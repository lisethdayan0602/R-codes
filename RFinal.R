install.packages('mlbench')
library(mlbench)

library(Hmisc)
library(ggm)
library(polycor)
library(tidyverse)
library(lm.beta)
data(Zoo)
force(Zoo)
View(Zoo)
head(Zoo)

unique(Zoo$tail)

contrasts(Zoo$tail)

tail <- t.test(legs ~ tail, data = Zoo) ## What do you find?
tail

ZooModel1 <- aov(legs ~ tail, data = Zoo)
summary(ZooModel1)

ZooReg <- lm(legs ~ tail, data = Zoo) 
summary(ZooReg)


ZooReg2 <- glm(predator ~ 1, binomial(), data = Zoo) 
summary(ZooReg2)


contrasts(Zoo$domestic)

ZooReg3 <- glm(predator ~ domestic, binomial(), data = Zoo)
summary(ZooReg3) 



exp(summary(ZooReg3)$coefficients[ ,1][1]) ## Result 1.588

1.588/(1+1.588) ## Result (0.6136012)

1/(1+exp(-(0.4626-(2.1674*1)))) ## Result (0.1538394)

0.153/(1-0.153) ## Result (0.1806375)

0.180/1.588 ## Result (0.1133501)

log(0.1133) ## Result (-2.177716)

ZooReg4 <- glm(predator ~ domestic + feathers, binomial(), data = Zoo)
summary(ZooReg4) 

 

install.packages('corrgram')
library(corrgram)
data(baseball)
force(baseball)
View(baseball)
head(baseball)

install.packages('olsrr')
library(olsrr)

baseball <- baseball %>%
  select(Salary, League, Atbat, Hits, Homer, Runs, RBI, Errors)


Model1 <- lm(Salary ~ League, data = baseball)  
summary(Model1)

Model2 <- lm(Salary ~ Atbat, data = baseball)  
summary(Model2)

Model3 <- lm(Salary ~ Hits, data = baseball)  
summary(Model3)

Model4 <- lm(Salary ~ Homer, data = baseball)  
summary(Model4)

Model5 <- lm(Salary ~ Runs, data = baseball)  
summary(Model5)

Model6 <- lm(Salary ~ RBI, data = baseball)  
summary(Model6)

Model7 <- lm(Salary ~ Errors, data = baseball)  
summary(Model7)

Model8 <- lm(Salary ~ League + Atbat, data = baseball)  
summary(Model8)

Model9 <- lm(Salary ~ League + Atbat + Hits, data = baseball)  
summary(Model9)

Model10 <- lm(Salary ~ League + Atbat + Hits + Homer, data = baseball) 
summary(Model10)

Model11 <- lm(Salary ~ League + Atbat + Hits + Homer + Runs, data = baseball) 
summary(Model11)

Model12 <- lm(Salary ~ League + Atbat + Hits + Homer + Runs + RBI, data = baseball)  
summary(Model12)

Model13 <- lm(Salary ~ League + Atbat + Hits + Homer + Runs + RBI + Errors, data = baseball)  
summary(Model13)

Model1 <- lm(Salary ~ League, data = baseball)
Model8 <- lm(Salary ~ League + Atbat, data = baseball)  
Model9 <- lm(Salary ~ League + Atbat + Hits, data = baseball)  
Model10 <- lm(Salary ~ League + Atbat + Hits + Homer, data = baseball) 
Model11 <- lm(Salary ~ League + Atbat + Hits + Homer + Runs, data = baseball) 
Model12 <- lm(Salary ~ League + Atbat + Hits + Homer + Runs + RBI, data = baseball)  
Model13 <- lm(Salary ~ League + Atbat + Hits + Homer + Runs + RBI + Errors, data = baseball)  
anova(Model1, Model8, Model9, Model10, Model11, Model12, Model13)


AIC(Model1)
AIC(Model2)
AIC(Model3)
AIC(Model4)
AIC(Model5)
AIC(Model6)
AIC(Model7)
AIC(Model8)
AIC(Model9)
AIC(Model10)
AIC(Model11)
AIC(Model12)
AIC(Model13)


allModel <- ols_step_all_possible(Model13)
allModel


modelBest <- ols_step_best_subset(Model13)
modelBest
  

install.packages('MASS')
library(MASS)
data(Cars93)
force(Cars93)
View(Cars93)
head(Cars93)

glimpse(Cars93)

Model1 <- lm(Price ~ Manufacturer, data = Cars93)  
summary(Model1)

Model2 <- lm(Price ~ Manufacturer + Type, data = Cars93)  
summary(Model2)

Model3 <- lm(Price ~ Manufacturer + Type + EngineSize, data = Cars93)  
summary(Model3)

allModel <- ols_step_all_possible(Model3)
allModel

modelBest <- ols_step_best_subset(Model3)
modelBest



