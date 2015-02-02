# Data Analysis structure demo

library(kernlab)
data(spam)

# Subsample from kernlab data
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)

## trainIndicator
## 0    1 
## 2314 2287

trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator == 0, ]

# Explore

head(trainSpam)

table(trainSpam$type)
## nonspam    spam 
##    1381     906 

plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)
plot(log10(trainSpam[, 1:4] + 1))

# Clustering
hCluster = hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)

hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:57] + 1))))
plot(hClusterUpdated)

## Statistical Prediction/Modeling

trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x, y) sum(x !=(y >0.5))
cvError = rep(NA, 55)
library(boot)
for (i in 1:55) {
    lmFormula = reformulate(names(trainSpam)[i], response = "numType")
    glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
    cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

# Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]
## [1] "charDollar"

## Get Measure of Uncertainty

# Use the best model from the group
predictionModel = glm(numType ~ charDollar, 
                       family = "binomial", 
                         data = trainSpam)
# Get predictions on the test set
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])

# Classify as spam for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

# Classification Table
table(predictedSpam, testSpam$type)

## predictedSpam nonspam spam
## nonspam          1346  458p
## spam               61  449

# Error Rate
(61+458)/(1346 + 458 + 61 + 449) * 100
## [1] 22.42869

