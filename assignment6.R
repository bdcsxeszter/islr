#7

```{r}
# Train and test sets with their respective Y responses.
set.seed(1)
df = Boston
sample.data = sample.split(df$medv, SplitRatio = 0.70)
train.set = subset(df, select=-c(medv), sample.data==T) #Using select to drop medv(Y) column.
test.set = subset(df, select=-c(medv), sample.data==F)
train.Y = subset(df$medv, sample.data==T)
test.Y = subset(df$medv, sample.data==F)
```

```{r}
# Four Random Forest models with m = p, p/2, p/3 and p/4, and ntree = 700.
# Test MSE for smaller trees can be accessed from the random forest object.
p=13
rf1 = randomForest(train.set, train.Y, test.set, test.Y, mtry = p, ntree = 700)
rf2 = randomForest(train.set, train.Y, test.set, test.Y, mtry = p/2, ntree = 700)
rf3 = randomForest(train.set, train.Y, test.set, test.Y, mtry = p/3, ntree = 700)
rf4 = randomForest(train.set, train.Y, test.set, test.Y, mtry = p/4, ntree = 700)
```

```{r fig.height=6, fig.width=10}
x.axis = seq(1,700,1) 
plot(x.axis,rf1$test$mse,xlab = "Number of Trees",ylab="Test Error", ylim=c(5,20),type="l",lwd=2)
lines(x.axis,rf2$test$mse,col="red",lwd=2)
lines(x.axis,rf3$test$mse,col="blue",lwd=2)
lines(x.axis,rf4$test$mse,col="green",lwd=2)
legend(600,19,legend=c("m=p", "m=p/2", "m=p/3", "m=p/4"), 
col=c("black", "red", "blue", "green"),lty=c(1,1,1), lwd=c(2,2,2))
```

#The test error decreases as the number of trees increases.
#The test error gets lower as m decreases from m=p upto m=p/3, and so we find no significant changes.

#8

```{r}
set.seed(2)
df = Carseats
sample.data = sample.split(df$Sales, SplitRatio = 0.70)
train.set = subset(df, sample.data==T)
test.set = subset(df, sample.data==F)
```

```{r fig.height=8, fig.width=12}
# Regression tree on training set.
tree.carseats = tree(Sales~.,data=train.set)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
# Test MSE.
tree.pred = predict(tree.carseats,test.set)
test.mse = mean((tree.pred-test.set$Sales)^2)
test.mse
```

#Shelve location and Price are the most important predictors in both cases. Test MSE 4,98

#3

```{r}
set.seed(2)
cv.carseats = cv.tree(tree.carseats)
plot(cv.carseats$size,cv.carseats$dev,xlab="Terminal Nodes",ylab="CV Error",type="b")
```

- CV Error is lowest for a tree with 6 terminal nodes. The full tree can now be pruned to obtain the 6 node tree.

```{r}
prune.carseats = prune.tree(tree.carseats,best=6)
tree.pred = predict(prune.carseats,test.set)
test.mse = mean((tree.pred-test.set$Sales)^2)
test.mse
```
#The test mse is reduced slightly
__(d)__  
```{r}
# Bagging
set.seed(2)
bag.carseats = randomForest(Sales~.,data=train.set,mtry=10,importance=T)
importance(bag.carseats)
bag.yhat = predict(bag.carseats,newdata = test.set)
mean((bag.yhat-test.set$Sales)^2)
```
- The most important variables are `ShelveLoc` and `Price`, as expected.
- The test MSE is __2.33__.Bagging improves the test mse substantially.

#e
```{r}
# Random Forests using m/2, sqrt(m), and m/4.
set.seed(2)
rf1.carseats = randomForest(Sales~.,data=train.set,mtry=10/2,importance=T)
rf2.carseats = randomForest(Sales~.,data=train.set,mtry=sqrt(10),importance=T)
rf3.carseats = randomForest(Sales~.,data=train.set,mtry=10/4,importance=T)
importance(rf1.carseats)
importance(rf2.carseats)
importance(rf3.carseats)
varImpPlot(rf2.carseats)
```

- In every model, the most important variables are `ShelveLoc` and `Price`.

```{r}
rf1.mse = mean((predict(rf1.carseats,newdata = test.set)-test.set$Sales)^2)
rf2.mse = mean((predict(rf2.carseats,newdata = test.set)-test.set$Sales)^2)
rf3.mse = mean((predict(rf3.carseats,newdata = test.set)-test.set$Sales)^2)
rf1.mse;rf2.mse;rf3.mse
```

#Test MSE using random forest with m=p/2 is 2,2,and this is slightly lower than using bagging.

#9

```{r}
#dim(OJ)
set.seed(3)
df = OJ
sample.data = sample.split(df$Purchase, SplitRatio = 800/1070) #800 observations for the test set.
train.set = subset(df, sample.data==T)
test.set = subset(df, sample.data==F)
```

```{r fig.height=6, fig.width=12}
tree.OJ = tree(Purchase~.,data=train.set)
summary(tree.OJ)
```

#training error rate: 0,15, terminal nodes. 
#residual mean deviance is high, therefore this model doesn't provide a good fit to the training data.
