library(AER)
data("SmokeBan")
SmokeBan
?SmokeBan
plot(smoker ~ education, data = SmokeBan)
plot(smoker ~ age, data = SmokeBan)
pairs(SmokeBan)
plot(smoker ~ gender, data = SmokeBan)


SmokeBan$smokes <- ifelse(SmokeBan$smoker == "yes", 1, 0)
SmokeBan$notsmokes <- ifelse(SmokeBan$smoker == "no", 1, 0)


allin <- lm(smokes ~ age+education+afam+hispanic+gender, data=SmokeBan)
summary(allin)
