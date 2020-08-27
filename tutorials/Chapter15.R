####################
#### CHAPTER 15 ####
####################

##########
## 15.3 ##
##########

alpha <- 0.01

mu1 <- 11
mu2 <- 12
mu3 <- 15
mu4 <- 18
mu5 <- 19

(mu_bar <- mean(c(mu1,mu2,mu3,mu4,mu5)))

tau1 <- mu1 - mu_bar
tau2 <- mu2 - mu_bar
tau3 <- mu3 - mu_bar
tau4 <- mu4 - mu_bar
tau5 <- mu5 - mu_bar

sum_tau2 <- sum(tau1^2, tau2^2, tau3^2, tau4^2, tau5^2)

sigma <- 3
a <- 5
n <- c(4,5,6)

##########################
## Values in Table 15.2 ##
##########################
(phi2 <- n*sum_tau2/(a*sigma^2))

(phi <- sqrt(phi2))

a*(n-1)

##########
## 15.4 ##
##########

## The most similar R function to the tests carried out in chapter 15
### is power.t.test(). It doesn't give the exact same n estimate as
### the book. This function gives n=4.62, which should be rounded up to
### 5 samples, whereas the book give n=6.
power.t.test(power=0.9,delta=10,sd=3,sig.level = 0.01, type="two.sample")




