## 1. Load the ToothGrowth data and perform some basic exploratory data analyses 
## 2. Provide a basic summary of the data.
## 3. Use confidence intervals and/or hypothesis tests to compare tooth growth 
## by supp and dose. (Only use the techniques from class, even if there's other 
## approaches worth considering)
## 4. State your conclusions and the assumptions needed for your conclusions. 

## Load and install required libraries
if (!(require(ggplot2, quietly=T))) {
  install.packages('ggplot2')
}
library(ggplot2)

## Load in Tooth Growth data
data(ToothGrowth)

## Perform some basic exploratory analysis
str(ToothGrowth)

head(ToothGrowth)

tail(ToothGrowth)

## Look at the graph in help
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")

## Convert dosage variable to factor
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

## Make new boxplot
g <- ggplot(ToothGrowth, aes(x = dose, y = len, fill=dose))
g <- g + geom_boxplot(notch=F) + facet_grid(. ~ supp) + 
  stat_summary(fun.y = mean, geom = "point", shape = 5, size = 4) + 
  stat_boxplot(geom ='errorbar')
g


## Provide a basic summary of the data.
summary(ToothGrowth)


## Hypothesis Testing
t1 <- t.test(len ~ supp, data = ToothGrowth, paired = F, var.equal = T)
conf1 <- rbind(t1$conf)
dt1 <- cbind(conf1, t1$p.value)
colnames(dt1) <- c("Confidence" ,"Interval", "P-Value")
dt1

dose <- subset (ToothGrowth, dose %in% c(0.5, 2.0)) 
t2 <- t.test(len ~ dose, data = dose, paired = F, var.equal = F)
t2
conf2 <- rbind(t2$conf)
dt2 <- cbind(conf2, t2$p.value)
dt2