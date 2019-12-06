###################
#### CHAPTER 6 ####
###################

# Note on the following code:
# When an "assign" line such as line 16 is enclosed in parenthesis, it outputs
# the number calulated on the right side of the assign ("<-") to the console

###########
### 6.2 ###
###########
## percentage errors detected
errors <- c(66.7, 64.3, 67.1, 66.1, 65.5, 69.1, 67.2, 68.1, 65.7, 66.4)
n <- length(errors)

## sample mean of the errors detected
(xbar_errors <- sum(errors)/n)
## OR use the mean() function
mean(errors)

## sample variance (s^2)
(var_errors <- (sum((errors - xbar_errors)^2))/(n-1))
## OR use the var() function to get the same answer:
var(errors)

## sample standard deviation (s)
(sd_errors <- sqrt(var_errors))
## OR use the sd() function
sd(errors)

###########
### 6.4 ###
###########
## Figure 6.1. Distribution of the z statistic
plot(x = seq(-3,3,length.out = 1000),
     y = dnorm(seq(-3,3,length.out = 1000)),
     main = "Normal Distribution",
     ylab = "Probability Density",
     xlab = "",
     type = "l",
     lwd = 2)
abline(v = c(qnorm(0.025), qnorm(0.975)), col = "red")
text(x = 0, y = 0.25, label = "0.95")
text(x = -2.2, y = 0.007, label = "0.025")
text(x = 2.2, y = 0.007, label = "0.025")
text(x = qnorm(0.025), y = 0.2, label = "z = -1.96", col = "red", pos = 2)
text(x = qnorm(0.975), y = 0.2, label = "z = 1.96", col = "red", pos = 4)

#############
### 6.5.1 ###
#############
## Figure 6.2: Student's t distribution for several values of v (degrees of freedom)
plot(x = seq(-3,3,length.out = 1000),
     y = dnorm(seq(-3,3,length.out = 1000)),
     main = "t Distributions",
     ylab = "Probability Density",
     xlab = "",
     type = "l",
     lwd = 2)
lines(x = seq(-3,3,length.out = 1000),
     y = dt(seq(-3,3,length.out = 1000), df = 4),
     lwd = 2,
     col = "magenta")
lines(x = seq(-3,3,length.out = 1000),
      y = dt(seq(-3,3,length.out = 1000), df = 1),
      lwd = 2,
      col = "goldenrod1")
text(x = 0.5, y = 0.37, label = "Normal distribution", pos = 4)
text(x = 0.56, y = 0.3, label = " -------- t distribution, v = 4", pos = 4, col = "magenta")
text(x = 0.75, y = 0.2, label = "------------- t distribution, v = 1", pos = 4, col = "goldenrod1")

#######################
## Example on p. 139 ##
#######################
xbar <- 15.9
mu <- 15
n <- 10
s <- 0.9

# find t statistic for the above values
(t <- ((xbar - mu)/s) * sqrt(n-1))

# find the 95% confidence level cut off for this t-distribution
c(qt(0.025, df = n-1), qt(0.975, df = n-1))

# the value of t is outside the 95% range, therefore we can regect the null hypothesis

#######################
## Example on p. 141 ##
#######################
n1 <- 12
n2 <- 12
xbar1 <- 5.1
xbar2 <- 4.8
s1 <- 0.36
s2 <- 0.4

# calculate the standard deviation first
(sigma <- sqrt(((n1-1)*(s1^2) + (n2-1)*(s2^2))/(n1+n2-2)))

# t statistic
(xbar1-xbar2)/(sigma*sqrt((1/n1)+(1/n2)))

# find the 99% confidence level cut off for this t-distribution
c(qt(0.01, df = n1+n2-2), qt(0.99, df = n1+n2-2))

# the calculated t-statistic falls within the 99% confidence range, therefore we cannot reject our null hypothesis

###########
## 6.5.2 ##
###########
## Figure 6.3. Snedecor's F distribution
plot(x = seq(0,5,length.out = 1000),
     y = df(seq(0,5,length.out = 1000), df1 = 10, df2 = 10),
     main = "F Distribution",
     ylab = "Probability Density",
     xlab = "",
     type = "l",
     lwd = 2)
text(x = 1.5, y = 0.7, "v1 = 10, v2 = 10")
abline (v = qf(0.9, df1 = 10, df2 = 10), lty = 2)
abline (v = qf(0.95, df1 = 10, df2 = 10), lty = 2)
abline (v = qf(0.99, df1 = 10, df2 = 10), lty = 2)
text(x = qf(0.90, df1 = 10, df2 = 10), y = 0.4, label = "F = 0.90", pos = 2)
text(x = qf(0.95, df1 = 10, df2 = 10), y = 0.35, label = "F = 0.95", pos = 2)
text(x = qf(0.99, df1 = 10, df2 = 10), y = 0.3, label = "F = 0.99", pos = 2)

#### THE FOLLOWING IS NOT IN THE BOOK ###
## Compare the above F-distribution to the distribution with other values of v1 and v2:
lines(x = seq(0,5,length.out = 1000),
     y = df(seq(0,5,length.out = 1000), df1 = 8, df2 = 8),
      lwd = 2,
      col = "magenta")
text(x = 1.5, y = 0.65, "v1 = 8, v2 = 8", col = "magenta")

lines(x = seq(0,5,length.out = 1000),
     y = df(seq(0,5,length.out = 1000), df1 = 4, df2 = 4),
      lwd = 2,
      col = "goldenrod1")
text(x = 1.5, y = 0.6, "v1 = 4, v2 = 4", col = "goldenrod1")

#######################
## Example on p. 143 ##
#######################
n1 <- 9
n2 <- 12
sigma1 <- 16
sigma2 <- 25
s1 <- 20
s2 <- 8

## Calculate F-statistic
(f <- (n1*s1)/((n1-1)*sigma1))/((n2*s2)/((n2-1)*sigma2))

## F-Statistic is greater than the 95% confidence level, therefore we can reject the null hypothesis.
# See plot below

## F-distribution plot for example on p.143
fDistPlot(fvalue = f, df1 = 8, df2 = 11, alpha = 0.05)

###########
## 6.5.3 ##
###########
## Figure 6.4. Chi-square distribution for several values of v
plot(x = seq(0, 25,length.out = 1000),
     y = dchisq(seq(0, 25,length.out = 1000), df= 2),
     main = "Chi-squared Distribution",
     ylab = "Probability Density",
     xlab = "",
     type = "l",
     lwd = 2)
lines(x = seq(0, 25,length.out = 1000),
      y = dchisq(seq(0, 25,length.out = 1000), df= 4),
      lwd = 2,
      col = "blue")
lines(x = seq(0, 25,length.out = 1000),
      y = dchisq(seq(0, 25,length.out = 1000), df= 6),
      lwd = 2,
      col = "forestgreen")
lines(x = seq(0, 25,length.out = 1000),
      y = dchisq(seq(0, 25,length.out = 1000), df= 10),
      lwd = 2, col = "orange")
text(x = 2, y = 0.4, label = "v = 2")
text(x = 4, y = 0.2, label = "v = 4", col = "blue")
text(x = 6, y = 0.15, label = "v = 6", col = "forestgreen")
text(x = 12, y = 0.1, label = "v = 10", col = "orange")

##########################
## Example on p.145-146 ##
##########################
o1 <- 75
o2 <- 65
o3 <- 25
o4 <- 35

e1 <- 70
e2 <- 70
e3 <-30
e4 <- 30

k <- 2

## Calculation Chi-sq
((o1-e1)^2)/e1 + ((o2-e2)^2/e2) +((o3-e3)^2/e3) + ((o4-e4)^2/e4)
## degrees of freedom:
k-1

## find value of Chi-Sq distribution at 95% confidence
qchisq(0.95, df = k-1)

## The Chi-sq statistic is within the 95% confidence range, therefore we cannot reject the null hypothesis

## Plot of Chi-sq example problem ##
plot(x = seq(0, 25,length.out = 1000),
     y = dchisq(seq(0, 25,length.out = 1000), df= 1),
     main = "Chi-squared Distribution",
     ylab = "Probability Density",
     xlab = "",
     type = "l",
     lwd = 2)
text(x = 1, y = 2.0, label = "v = 1")
abline(v = qchisq(0.95, df = k-1), lty=2)
text(x = qchisq(0.95, df = k-1), y = 0.4, label = "Chi-sq(0.95) = 3.84", pos = 4)
points(2.28, dchisq(2.28, df = 1), col = "red", pch = 19, cex = 1.5)
text(2.28, dchisq(2.28, df = 1), label = "2.28",pos = 3, col = "red")

