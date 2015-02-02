## Install and load required libraries
if (!(require(ggplot2, quietly=T))) {
  install.packages('ggplot2')
}
library(ggplot2)

## Set seed for random variables
set.seed(1234)

## Simulation
lambda <- 0.2
n <- 40
index <- rep.int(1:1000, 1)
ev <- rep(0, 1000)
for (i in index){
  ev[i] <- mean(rexp(n, lambda))
}

df <- data.frame(mean = ev, n = index)

## Plot the results
g <- ggplot(df, aes(x = ev))
g + geom_histogram(aes(y = ..density..), colour = "black", 
                   fill = "white", binwidth = 0.5) + 
  xlab("Mean") +
  ggtitle("Distribution Curve for 1000 Randomly Sampled Exponential Means \n (n = 40, lambda = 0.2)") + 
  stat_function(fun = dnorm, aes(color = "red"), args = list(mean = mean(ev), 
                                                             sd = sd(ev))) + 
  stat_function(fun = dnorm, aes(color = "blue"),
                args = list(mean = (1/.2), 
                            sd = ((1/lambda) / sqrt(n)))) +
  scale_colour_manual("", values = c("red", "blue"), 
                      labels = c("Actual Curve", "Expected Curve"))

## Compare theoretical values with actual values
center.theory <- 1/(lambda) # 5
center.actual <- mean(ev) # 4.96
center.error <- ((center.theory - center.actual) / center.theory)*100 # 0.90

variance.theory <- ((1/lambda) / sqrt(n))^2 # 0.625
variance.actual <- var(ev) # 0.566
variance.error <- ((variance.theory - variance.actual)/variance.theory)*100 # 9.45

## Make a table for visualization
x <- c(center.actual, center.theory, center.error)
y <- c(variance.actual, variance.theory, variance.error)

tab <- cbind(x,y)
colnames(tab) <- c("Center", "Variance")
rownames(tab) <- c("Actual", "Theoretical", "Error")
tab