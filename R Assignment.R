library(rpart.plot)
library(tidyverse)
data(ptitanic)
force(ptitanic)
head(ptitanic)
View(ptitanic)
summary(ptitanic)

ptitanic <- ptitanic %>%
  select (sex, survived)

glimpse(ptitanic)

table(ptitanic$survived)

contrasts(ptitanic$survived)

model1 <- glm(sex ~ survived, binomial, data = ptitanic)
sum.model1 <- (summary (model1))
sum.model1

exp(sum.model1$coefficients[ ,1][1]) ## Result (5.370)
## This equation gives me the Odds of dying if male OR the Odds when x1=0


5.370/(1+5.370) ## Result (0.843)
## This equation gives me the probability of male dying

##NTERPRETATION: 0.843 (84.3%) is the probability of males dying 
## Or 0.843 (84.3%) is the probability of dying when being male

1/(1+exp(-(1.680-(2.425*1)))) ## Result (0.321)
## This equation gives me the Probability of males surviving
## Or being dying incresing by 1 unit

0.321/(1-0.321) ## Result (0.472)
## This equetion gives me the Odds of being male giving that they survived

0.472/5.370 ## Result (0.0878)
## This equation gives me the change in Odds 
## new Odds/ old Odds

log(0.0878) ## Result (-2.432) 
## This equation gives me the coefficient

## INTERPRETATION: 0.0878 is the change in Odds between males dying and surviving 
## 0.0878 is the change in Odds when males surviving 




