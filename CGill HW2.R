## 1.
library(MASS)
data(Boston)
head(as.data.frame(cbind(Boston$medv, Boston$crim, Boston$chas)))
dim(Boston)

## a.
summary(Boston$medv)

## The minimum value is $5000. The average is $22530 but the median is $21200. The max value is $50000.

## b.
plot(Boston$crim, Boston$medv, main = "Scatter of Housing Value & Crime Rate",
     xlab = "Crime Rate", ylab = "Median Housing Value")
m <- lm(Boston$medv ~ Boston$crim)
cor(Boston$crim, Boston$medv)
cor.test(Boston$crim, Boston$medv)

## c.
m1 <- lm(medv ~ crim + chas, data = Boston)
summary(m1)

## Both have an effect on the housing value because the p-value is less than .01%.  
##The lower the crime rate the higher housing values and the closer to the river the higher housing values.

## d.
m2 <- lm(medv ~ crim + chas + tax + dis + rad, data = Boston)
summary(m2)

## The dis variable is not statistically significant because it is over .01%. The rest of the variables are under .01%.
##Taxes and crime lower the housing values.

## e.
library(car)
vif(m1)
vif(m2)

## No, there is not a multicollineartity problem with m1, but maybe with m2 because tax has a high variation.  

## f.
step(m2)

##The vairable dis is eliminated.

## g.
crim <- c(20, 40, 5, 10, 500)
chas <- c(1, 1, 0, 0, 0)
newdata <- as.data.frame(cbind(crim, chas))
prediction <- predict(m1, newdata)
Final <- as.data.frame(cbind(crim, chas, prediction))
names(Final)[1] <- "Crime Rate"
names(Final)[2] <- "Dummy"
names(Final)[3] <- "Prediction"
write.csv(Final, "Prediction.csv")
