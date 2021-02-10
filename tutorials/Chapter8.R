###################
#### CHAPTER 8 ####
###################

###########
### 8.1 ###
###########

## Table 8.1. Number of errors in 24 similar projects
langErrors <- data.frame(projectID = 1:24,
                         language = c("C", "A", "D", "B", "D", "D", "C", "B", "B", "A", "A", "B", "C", "B", "B", "C", "C", "D", "D", "A", "C", "D", "D", "D"),
                         errors = c(71, 60, 62, 65, 63, 60, 66, 66, 67, 59, 63, 63, 68, 64, 71, 68, 67, 61, 64, 62, 68, 63, 56, 59))
## the mean errors for each language
(errorMeans <- with(langErrors, tapply(X = errors, INDEX = language, FUN = mean)))

## mean errors across all languages
(ybar <- mean(langErrors$errors))

## number of projects per language from this study
(r <- with(langErrors, tapply(projectID, language, length)))

#########
## 8.2 ##
#########
## Effect per alternative
(langEffect <- with(langErrors, tapply(errors, language, function(x) mean(x)-ybar)))
## In the above line of code I created an in-line function as the "FUN" arguement in the tapply function
# this simple function I created takes the mean of the input (i.e. x) and subtracts the "grand mean" (i.e. ybar)

###########
### 8.3 ###
###########
langErrors$yhat_ij <- numeric(24)

#with(langErrors, tapply(yhat_ij, language, assign, c(61, 66, 68, 61)))

langErrors$yhat_ij[langErrors$language == "A"] <- rep(61, 4)
langErrors$yhat_ij[langErrors$language == "B"] <- rep(66, 6)
langErrors$yhat_ij[langErrors$language == "C"] <- rep(68, 6)
langErrors$yhat_ij[langErrors$language == "D"]<- rep(61, 8)

langErrors$residuals <- langErrors$errors - langErrors$yhat_ij


## Residuals of each observation
# Note that the order is different as in Table 8.4, but all value are the same
# The order is different because in the data frame that I created I order the values based on project number (projectID)
(residErrors <- with(langErrors, tapply(errors, language, function(x) x-mean(x))))

## Histogram of residuals is similar to the point graph of Figure 8.1.
# Histograms show the frequency of the numbers that you give it. In this case,
# the histogram is showing the frequency of our calculated in-group residuals.
# instead of points, the frequency is represented by bars
hist(unlist(residErrors), breaks = 12, xlim = c(-6, 6),
     main = "Histogram of residuals", xlab = "Residual")

## Figure 8.2. Residuals plotted as a function of estimated response variable values.
plot(residuals ~ yhat_ij, data = langErrors, xlim = c(58, 69), ylim = c(-6,6))
abline(h = 0, lty = 2)

## Figure 8.5. Residuals graphs for each language. Again, displayed as histograms:
# par() alters graphics parameters. In this case I want to plot the histogram of residuals for each language in
# "4 rows and 1 column", i.e. c(4, 1) in the code below
par(mfrow = c(4,1))
hist(langErrors$residuals[langErrors$language == "A"], breaks = 12, xlim = c(-6, 6), ylim = c(0,3),
     main = "Histogram of residuals: Language A", xlab = "Residual")
hist(langErrors$residuals[langErrors$language == "B"], breaks = 12, xlim = c(-6, 6), ylim = c(0,3),
     main = "Histogram of residuals: Language B", xlab = "Residual")
hist(langErrors$residuals[langErrors$language == "C"], breaks = 12, xlim = c(-6, 6), ylim = c(0,3),
     main = "Histogram of residuals: Language C", xlab = "Residual")
hist(langErrors$residuals[langErrors$language == "D"], breaks = 12, xlim = c(-6, 6), ylim = c(0,3),
     main = "Histogram of residuals: Language D", xlab = "Residual")
## once par() is changed, it stays that way for the rest of your R session, therefore I am changing the values for
# mfrow back to the default which is one plot per graphics display (i.e. mfrow = c(1,1))
par(mfrow = c(1,1))

## Figure 8.6. Graph of residuals as a function of time
plot(residuals ~ projectID, data = langErrors, xlab = "Time (or ProjectID)",
     ylim = c(-6,6))
abline(h = 0, lty = 2)

###########
### 8.4 ###
###########
n <- length(langErrors$projectID)
(SSY <- sum(langErrors$errors^2))
(SS0 <- n*(ybar^2))
(SSA <- sum(r *(langEffect)^2))
(SSE <- with(langErrors, sum(residuals^2)))

(SST <- SSA + SSE)

## Percent variation explained by the programing language:
SSA/SST*100

###########
### 8.5 ###
###########
k <- length(unique(langErrors$language))

## Check the degrees of freedom on each side of the equation
n == 1 + (k-1) + (n-k)

(MSA <- SSA/(k-1))
(MSE <- SSE/(n-k))

MSA/MSE

plot(x = seq(0,14,length.out = 1000),
     y = df(seq(0,14,length.out = 1000), df1 = k-1, df2 = n-k),
     main = "F Distribution",
     ylab = "Probability Density",
     xlab = "",
     type = "l",
     lwd = 2)
text(x = 12, y = 0.7, "v1 = 3, v2 = 20")
abline (v = qf(0.9, df1 = k-1, df2 = n-k), lty = 2)
abline (v = qf(0.95, df1 = k-1, df2 = n-k), lty = 2)
abline (v = qf(0.99, df1 = k-1, df2 = n-k), lty = 2)
text(x = qf(0.90, df1 = k-1, df2 = n-k), y = 0.4, label = paste0("F(0.90) = ", round(qf(0.90, df1 = k-1, df2 = n-k), 2)), pos = 2, srt = 90)
text(x = qf(0.95, df1 = k-1, df2 = n-k), y = 0.35, label = paste0("F(0.95) = ", round(qf(0.95, df1 = k-1, df2 = n-k), 2)), pos = 2, srt = 90)
text(x = qf(0.99, df1 = k-1, df2 = n-k), y = 0.3, label = paste0("F(0.99) = ", round(qf(0.99, df1 = k-1, df2 = n-k), 2)), pos = 2, srt = 90)
points(13.5, df(13.5, df1 = 3, df2 = 20), col = "red", pch = 19, cex = 1.5)
text(13.5, df(13.5, df1 = 3, df2 = 20), label = "13.5", col ="red", pos = 3)

###########
### 8.6 ###
###########

mse <- 5.6
n_bar <- 6

(scaling <- sqrt(mse/n_bar))

##############################
## In-text example on p.198 ##
##############################
ybarB <- 66
ybarC <- 68

## difference in means
(mean_diff <- ybarC - ybarB)

s2_R <- 5.6

## degrees of freedom
v <- 20

nC <- 6
nB <- 6

## estimated variance:
(est_var <- s2_R * (1/nC + 1/nB))

## 95% confidence interval:
# using t-value (2.08) taken from the chart:
mean_diff + c(-1,1)*2.08 * sqrt(est_var)

## using the R-calculated t-value using the qt() function:
mean_diff + qt(c(0.025, 0.975), 20) * sqrt(est_var)

## The confidence interval includes 0, therefore we conclude that there is no
# appreciable difference between the two languages used.
