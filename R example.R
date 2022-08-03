install.packages("rpart.plot")
library(rpart.plot)

library(tidyverse)
library(caret)

data(package = .packages(all.available = TRUE))

data("ptitanic")
force(ptitanic)
head(ptitanic)
View(ptitanic)

ptitanic <- ptitanic %>%
  select(survived, sex)

glimpse(ptitanic)
  
table(ptitanic$survived) ## How can I calculate the PROBABILITY from this table? 

contrasts(ptitanic$survived)

model1 <- glm(ptitanic$sex ~ ptitanic$survived, binomial(), data = ptitanic)
sum.ptitanic <- (summary(model1))
sum.ptitanic

sum.ptitanic$coefficients


exp(sum.ptitanic$coefficients[1][1]) ## equation when x1=0 (when male dying)
## This equation gives me the result of the ODDS of male dying (5.370)
## How this equation gives me the ODDS, when the ODDS formula is P/(1-P)???
## WHAT IF I WANT TO SEE WHEN FEMALE DYING)
## Pluging the coefficients into the regression equation 

## What is the intercep? I do line 30 and the lod of line 30 log(5.370)



5.37/(1+5.370) ## This equation gives me the PROBABILITY of diying if a male (0.84301)

## what is the probability of dying if male

## I COULD ADD ANOTHER TABLE HERE FOR CHECK THAT RESULT
## table(ptitanic$sex, ptitanic$survived)
## In the example from where that 37 came from?

## lines 49 to 59 is just for fiding the coefficients 
1/(1+exp(-(1.680-(2.425*1)))) ## equation when x1=1 (When survived)
 ## This equation give me the PROBABILITY of surviving if a male (0.321)


0.321/(1-0.321) ## This equation gives me the ODDS of being male giving that they suvived (0.472)

0.472/5.370 ## This equation gives me the change of the ODDS of... (0.0878)
## new ODDS/ old ODDS

log(0.0878) ## This equation gives me the REGRESSION COEFFICIENT!
## Log ODDS diference!


#########################################################################################

exp(-(sum.ptitanic$coefficients[1][1])) ## givis me part of the probability (0.186)

1/(1+0.186)  ## of part of the proba... ## gives me the probablitily (0.843)

0.843/(1-0.843) ## Odds
## The odds of a man being death is 5.369

#########
exp(-(1.68+(-2.425*1))) ## when survived (2.106) part of the proba...

1/(1+2.106) ##(0.319) this is proba..

0.319/(1-0.319) ## Odds (0.468)
## The odds of a man surviving is 0.468

0.468/5.369 ## The difference (0.0871)
## If we increase variable x1 by 1, in other words what happen to the intercep  

log(0.0871) ## Gives me the coefficient

## Tells us how much the intercet will increase if we increase the variable by 1 unit 
## The log odds will increase by #... when the dying turns into surviving 

## intercep is log odss when x = 0 
## coefficient is the change is log odds (/the ne and old odds) when we go from x=0 to x=1 
## 



  