library(MASS)
Boston
?Boston
#A
#506 rows of suburbs or towns and 14 columns of predictors


#B
#crime rate per capita
#noxnitrogenoxides concentration
#distance from 5 employment centres (weighed, mean)
#property tax
#median value of owner-occupied homes
pairs(~crim+nox+dis+tax+medv, data = Boston)
#nox&dis - negative linear relation
#crim&dis and crim&medv - negative linear relation
#dis&medv positive linear relation


#C
pairs(Boston)
#age, dis, medv
cor(Boston[-1],Boston$crim)
#poz linear: indus nox rad tax
#neg linear: black medv dis


#D range is widest at crim
highcrim = Boston[which(Boston$crim > mean(Boston$crim) + 2*sd(Boston$crim)),]
highcrim
range(Boston$crim) ; 
mean(Boston$crim) ; 
sd(Boston$crim)

hightax = Boston[which(Boston$tax > mean(Boston$tax) + 2*sd(Boston$tax)),]
range(Boston$tax)

highratio = Boston[which(Boston$ptratio > mean(Boston$ptratio) + 2*sd(Boston$ptratio)),]
range(Boston$ptratio)

#E - 35
sum(Boston$chas==1)

#F
median(Boston$pratio)

#G town 399 and 406
which(Boston$medv == min(Boston$medv))
Boston[399,]
Boston[406,] 
#crim close to max, especially in 406. similar values in both suburbs
max(Boston$crim)

#H
which(Boston$rm > 7) #3
which(Boston$rm > 8) #1
Boston[1,]
sum(Boston$rm > 8)
sum(Boston$rm > 7)


