###################
#### CHAPTER 7 ####
###################

#########
## 7.2 ##
#########

## Table 7.1. Data on 20 projects (using process A and B) ##
projects <- data.frame(Order = 1:20,
           Process = c(rep("A", 10), rep("B", 10)),
           Reliability = c(89.7, 81.4, 84.5, 84.8, 87.3, 79.7, 85.1, 81.7, 83.7, 84.5, 84.7, 86.1, 83.2, 91.9, 86.3, 79.3, 82.6, 89.1, 83.7, 88.5))

# Calculate the mean reliability of process A and process B
(processMeans <- with(projects, tapply(X = Reliability, INDEX = Process, FUN = mean)))

ybar_a <- processMeans[[1]]
ybar_b <- processMeans[[2]]

# Difference in means
ybar_b - ybar_a

# historical differences greater than 1.30
probabilityDiff <- 9/191

# which is less than 0.05
probabilityDiff < 0.05

#########
## 7.3 ##
#########
languages <- data.frame(Order = 1:11,
                        Language = c("A", "A", "B", "B", "A", "B", "B", "B", "A", "A", "B"),
                        Correctness = c(29.9, 11.4, 26.6, 23.7, 25.3, 28.5, 14.2, 17.9, 16.5, 21.1, 24.3))

# Find means for language A and B
(means <- with(languages, tapply(X = Correctness, INDEX = Language, FUN = mean)))

# Find n for language A and B
(ns <- with(languages, tapply(X = Correctness, INDEX = Language, FUN = length)))

# Find the difference between language A and language B
(meanDiff <- means[[2]] - means[[1]])

ybar_a <- means[[1]]
ybar_b <- means[[2]]

n_a <- ns[[1]]
n_b <- ns[[2]]

y_a <- languages$Correctness[languages$Language == "A"]
y_b <- languages$Correctness[languages$Language == "B"]

# Table 7.6 t0 calculations
ybar_b - ybar_a
(s2_a <- (sum((y_a - ybar_a)^2))/(n_a - 1))
(s2_b <- (sum((y_b - ybar_b)^2))/(n_b - 1))
(s2 <- ((n_a - 1)*s2_a + (n_b - 1)*s2_b)/((n_a-1)+(n_b-1)))
(t0 <- (meanDiff - 0)/sqrt(s2*((1/n_a) + (1/n_b))))

# This function gives you the exact probability of 0.44 in this t-distribution
pt(0.44, df = 9)

# When using the table in the annexes, the probability must be estimated between the values
#  shown in the table. As you can see, the probability that the above function gives you, 0.66,
#  is in fact between 0.260 and 0.697 as said in the book.

# The range of the 95% confidence interval for t-distribution with df = 9
c(qt(0.025, df = 9), qt(0.975, df = 9))
# The t-value we found is between this interval, therefore we cannot reject out null hypothesis

# A plot of the t-distribution, the 95% confidence interval, and the t-value we calculated
plot(x = seq(-3,3,length.out = 1000),
     y = dt(seq(-3,3,length.out = 1000), df = 9),
     main = "t Distribution",
     ylab = "Probability Density",
     xlab = "",
     type = "l",
     lwd = 2)
abline(v = qt(0.025, df = 9), lty = 2)
abline(v = qt(0.975, df = 9), lty = 2)
points(0.44, dt(0.44, df = 9), pch = 19, cex = 1.5, col = "red")
text(0.44, dt(0.44, df = 9), label = "0.44", pos = 4, col = "red")
text(0, 0.18, label = "95%")

# t-test
t.test(subset(languages, Language == "B")$Correctness, subset(languages, Language == "A")$Correctness)
t.test(y_b, y_a, var.equal = TRUE, paired=FALSE)
#########
## 7.4 ##
#########
techniques <- data.frame(project = 1:10,
                         techniqueA = c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3),
                         techniqueB = c(14.0, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6))
techniques$d <- techniques$techniqueB - techniques$techniqueA

# Mean difference in techniques
mean(techniques$d)

# Variance the quick and easy way
var(techniques$d)
# or as done in the book:
s2 <- (sum((techniques$d - mean(techniques$d))^2))/(length(techniques$d)-1)

# Standard deviation (s)
sd(techniques$d)
# or
s <- sqrt(s2)

# Calculate t statistic
(t0 <- ((mean(techniques$d)-0)/s) * sqrt(length(techniques$d)-1))

# Probability density of the calculated t value. This value is greater than 0.975, therefore
#  we can reject the null hypothesis
pt(t0, df = 9)

# The probability that t >= 3.18 is found by the following:
1-pt(t0, df=9)
# OR
pt(-t0, df=9)

# The range of the 95% confidence interval for t-distribution with 9 degrees of freedom
qt(c(0.025, 0.975), df = 9)
# Again, the t value is outside of this range therefore, reject the null. There is evidence that
#  there is a difference between technique A and B.

# Visual representation of t-distribution:
plot(x = seq(-3.5,3.5,length.out = 1000),
     y = dt(seq(-3.5,3.5,length.out = 1000), df = 9),
     main = "t Distribution",
     ylab = "Probability Density",
     xlab = "",
     type = "l",
     lwd = 2)
abline(v = qt(0.025, df = 9), lty = 2)
abline(v = qt(0.975, df = 9), lty = 2)
points(t0, dt(t0, df = 9), pch = 19, cex = 1.5, col = "red")
text(t0, dt(t0, df = 9), label = round(t0, 2), pos = 3, col = "red")
text(0, 0.18, label = "95%")

# A simple t-test on d (the differences)
t.test(techniques$d)

