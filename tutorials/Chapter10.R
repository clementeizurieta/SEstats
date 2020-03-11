####################
#### CHAPTER 10 ####
####################

##########
## 10.2 ##
##########

(factorial <- data.frame(technique = rep(c("R", "S", "T", "U"), each = 12),
                        domain = rep(c("I", "II", "III"), each = 4, times = 4),
                        value = c(0.31, 0.45, 0.46, 0.43,   0.36, 0.29, 0.40, 0.23,   0.22, 0.21, 0.18, 0.23,
                                  0.82, 1.10, 0.88, 0.72,   0.92, 0.61, 0.49, 1.24,   0.30, 0.37, 0.38, 0.29,
                                  0.43, 0.45, 0.63, 0.76,   0.44, 0.35, 0.31, 0.40,   0.23, 0.25, 0.24, 0.22,
                                  0.45, 0.71, 0.66, 0.62,   0.56, 1.02, 0.71, 0.38,   0.30, 0.36, 0.31, 0.33)))

#############################################
# 10.2.1 Identifying the Mathematical Model #
#############################################

model1 <- lm(value ~ domain + technique + domain*technique, data = factorial)
summary(model1)

## How to interpret model summary output:
## The estimate for the first row in the model summary output is the estimated
# value of domain I, technique R. Notice it is the same value (except with more
# significant figures) as the value in the first row, first column of table 10.2
# in the book. Each subsequent row in this summary table is in reference to
# the baseline or 'Intercept' -- which is technique R, domain I. The second row,
# domainII, is the effect of using domain II on technique R. In this case, the estimate
# for domain II for technique R is 0.09250 LESS (because negative) than the estimated
# value for domain I, technique R. To get the actual estimate for domain II,
# technique R, add the second model coefficient to the first:
coefficients(model1)[1] + coefficients(model1)[2]

# To get the estimated value for domian III, technique R, add the third model
# coefficient to the baseline:
coefficients(model1)[1] + coefficients(model1)[3]

# The fourth row in the model summary output is 'techniqueS', which is the
# effect of technique S on domain I (remember domain I is part of the
# baseline condition):
coefficients(model1)[1] + coefficients(model1)[4]

# Domain I, technique T:
coefficients(model1)[1] + coefficients(model1)[5]

# Domain I, technique U:
coefficients(model1)[1] + coefficients(model1)[6]

# In order to change both the domain AND the technique from the baseline,
# we have to start adding multiple lines to the baseline. To calculate the
# estimated value for domain II, technique S, we add coefficient 4 to change
# technique R to technique S and then we add the 7th line, which is the alteration
# to change domain I to domain II for technique S:
coefficients(model1)[1] + coefficients(model1)[4] + coefficients(model1)[7]

# Domain III, technique S:
coefficients(model1)[1] + coefficients(model1)[4] + coefficients(model1)[8]

# Domain II, technique T:
coefficients(model1)[1] + coefficients(model1)[5] + coefficients(model1)[9]

# Domain III, technique T:
coefficients(model1)[1] + coefficients(model1)[5] + coefficients(model1)[10]

# Domain II, technique U:
coefficients(model1)[1] + coefficients(model1)[6] + coefficients(model1)[11]

# Domain III, technique U:
coefficients(model1)[1] + coefficients(model1)[6] + coefficients(model1)[12]

#############################################################
# 10.2.2 Calculating the Variation in the Response Variable #
#############################################################
anova(model1)

## In the anova table output, the Sum Sq value for domain (row 1, column 2)
# is SSA in the book. Sum Sq value for technique (row2, column 2) is SSB.
# Sum Sq for domain:tachnique is SSAB, and the last row, Residuals is SSE.
# SST can be found by add all Sum Sq:
(sst <- sum(anova(model1)$"Sum Sq"))

## Percentage varitions in response due to each factor and foactor interactions:
# Domain
anova(model1)$"Sum Sq"[1]/sst * 100

# Technique
anova(model1)$"Sum Sq"[2]/sst * 100

# Domain:Technique
anova(model1)$"Sum Sq"[3]/sst * 100

# Error
anova(model1)$"Sum Sq"[4]/sst * 100


####################################################################################
# 10.2.3 Statistical Significance of the Variation Due to Factors and Interactions #
####################################################################################
anova(model1)

## See statistical significance in the last column ('Pr(>F)') of the anova table

#################################################################
# 10.2.4 Recommendations on the Best Alternative of Each Factor #
#################################################################

## PLOTTING

dImean <- mean(subset(factorial, domain == "I")$value)
dIImean <- mean(subset(factorial, domain == "II")$value)
dIIImean <- mean(subset(factorial, domain == "III")$value)

tRmean <- mean(subset(factorial, technique == "R")$value)
tSmean <- mean(subset(factorial, technique == "S")$value)
tTmean <- mean(subset(factorial, technique == "T")$value)
tUmean <- mean(subset(factorial, technique == "U")$value)


## Figure 10.1 ##
par(mfrow = c(1,2), mar = c(5,2,3,1), oma = c(0,3,0,0))
plot(y = c(dImean, dIImean, dIIImean),
     x = c(1:3),
     type = "o",
     pch = 19,
     xlab = "Domain",
     xaxt = "n",
     ylim = c(0.2, 0.7))
axis(side = 1, at = c(1,2,3), labels = c("I", "II", "III"))
plot(y = c(tRmean, tSmean, tTmean, tUmean),
     x = c(1:4),
     type = "o",
     pch = 19,
     xlab = "Technique",
     xaxt = "n",
     ylim = c(0.2, 0.8))
axis(side = 1, at = c(1,2,3,4), labels = c("R", "S", "T", "U"))
mtext("Response variable", side = 2, outer = T, line = 1)
par(mfrow = c(1,1), mar = c(5,4,4,1), oma = c(0,0,0,0))

## Alternative:
## Use a barplot in R when looking at factors. This simplifies the plotting code:
# (The default plot in R is a scatterplot, which is meant for 2 numeric variables)
barplot(height = c(dImean, dIImean, dIIImean),
        names.arg = c("I", "II", "III"),
        xlab = "Domain")
barplot(height = c(tRmean, tSmean, tTmean, tUmean),
        names.arg = c("R", "S", "T", "U"),
        xlab = "Technique")

#################################
# 10.2.5 Testing Model Validity #
#################################

## Figure 10.2 ##
plot(model1, which = 1)

plot(model1, which = 2)

plot(model1, which = 3)

##########
## 10.3 ##
##########

#############################################
# 10.3.1 Analysis for 2^2 Factorial Designs #
#############################################

factorial2 <- data.frame(paradigm = c())
