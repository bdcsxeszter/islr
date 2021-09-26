library(MASS)
library(ISLR)
Carseats
?Carseats
pairs(~Sales+Price+Urban+US, data=Carseats)
multiple.regression <- lm(Sales ~ Price+Urban+US, data=Carseats)
summary(multiple.regression)

#B

#C
contrasts(US)
contrasts(Urban)

#Sales = 13.04 + -0.05Price + -0.02Urban(Yes 1,No 0) + 1.20US(Yes 1,No 0)

#D compprice, income, advertising, price, shelvelocgood, shelvelocmedium
carseats_all_lm = lm(Sales~.,data=Carseats)
summary(carseats_all_lm)

#E
multiple.regression2 <- lm(Sales ~ CompPrice+Income+Advertising+Price+ShelveLoc, data=Carseats)
summary(multiple.regression2)
summary(multiple.regression)


#F differences in RSE, R2 and F-statistics indicate that multiple.regression2 is a better fit

#G
par(mfrow=c(2,2))
plot(multiple.regression2)
