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

# Domain II, technique S:
coefficients(model1)[1] + coefficients(model1)[2] +coefficients(model1)[4] + coefficients(model1)[7]

# Domain III, technique S:
coefficients(model1)[1] + coefficients(model1)[3] + coefficients(model1)[4] + coefficients(model1)[8]

# Domain II, technique T:
coefficients(model1)[1] + coefficients(model1)[2] + coefficients(model1)[5] + coefficients(model1)[9]

# Domain III, technique T:
coefficients(model1)[1] + coefficients(model1)[3] + coefficients(model1)[5] + coefficients(model1)[10]

# Domain II, technique U:
coefficients(model1)[1] + coefficients(model1)[2] + coefficients(model1)[6] + coefficients(model1)[11]

# Domain III, technique U:
coefficients(model1)[1] + coefficients(model1)[3] + coefficients(model1)[6] + coefficients(model1)[12]

#############################################################
# 10.2.2 Calculating the Variation in the Response Variable #
#############################################################
anova(model1)

## In the anova table output, the Sum Sq value for domain (row 1, column 2)
# is SSA in the book. Sum Sq value for technique (row2, column 2) is SSB.
# Sum Sq for domain:technique is SSAB, and the last row, Residuals is SSE.
# SST can be found by add all Sum Sq:
(sst <- sum(anova(model1)$"Sum Sq"))

## Percentage variations in response due to each factor and factor interactions:
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

################
# Let's run a non-parametric ANOVA for a factorial design.  Although Kruskal-Walis is a
# non-parametric alternative, it only works for a one-way ANOVA.  The Aligned Rank Transform (ART)
# test helps you run a non-parametric version of the factorial design.
library(ARTool)
domainFactor <- as.factor(factorial$domain)
techniqueFactor <- as.factor(factorial$technique)
m <- art(value ~ domainFactor*techniqueFactor, data = factorial)
anova(m)
##########################################################################
## 10.3 ANALYSIS FOR FACTORIAL DESIGNS WITH TWO ALTERNATIVES PER FACTOR ##
##########################################################################

#############################################
# 10.3.1 Analysis for 2^2 Factorial Designs #
#############################################

factorial2 <- data.frame(paradigm = c(rep("new", times = 3),
                                      rep("oo",times = 3),
                                      rep("new", times = 3),
                                      rep("oo", times= 3)),
                         knowledge = c(rep("with",times = 6),
                                       rep("without", times = 6)),
                         y = c(15, 18, 12,
                               45, 48, 51,
                               25, 28, 19,
                               75, 75, 81))

## 10.3.1.1 Identification of Mathematical Model ##
model2 <- lm(y ~ paradigm + knowledge + paradigm:knowledge, data = factorial2)

## Looking at the summary and anova tables gives us the estimates,
# R-squared of the model, sums of squares, F-values, p-values, etc.
summary(model2)

anova(model2)

### THE BOOK METHOD ###

# The following code builds a new data table for this analysis using
# the information from table 10.7
factorial2.2 <- factorial2
names(factorial2.2) <- c("A", "B", "y")
factorial2.2$alternativeA <- c(rep(-1, times = 3), rep(1, times = 3), rep(-1, times = 3), rep(1, times = 3))
factorial2.2$alternativeB <- c(rep(-1, times = 6), rep(1, times = 6))
factorial2.2

# Substituting the values of the coefficients:

## First find the mean of each treatment replication to be used in the contrast equation:
(y1_bar <- mean(subset(factorial2.2, alternativeA == -1 & alternativeB == -1)$y))
(y2_bar <- mean(subset(factorial2.2, alternativeA == 1 & alternativeB == -1)$y))
(y3_bar <- mean(subset(factorial2.2, alternativeA == -1 & alternativeB == 1)$y))
(y4_bar <- mean(subset(factorial2.2, alternativeA == 1 & alternativeB == 1)$y))

factorial2.2$trtMean <- c(rep(y1_bar, times = 3),
                          rep(y2_bar, times = 3),
                          rep(y3_bar, times = 3),
                          rep(y4_bar, times = 3))


(C0 <- 1/4 * (y1_bar + y2_bar + y3_bar + y4_bar))
(CA <- 1/4 * (-y1_bar + y2_bar - y3_bar + y4_bar))
(CB <- 1/4 * (-y1_bar - y2_bar + y3_bar + y4_bar))
(CAB <- 1/4 * (y1_bar - y2_bar - y3_bar + y4_bar))

## Sign Table
sign <- data.frame(I = rep(1, times = 4),
                   A = c(-1, 1, -1, 1),
                   B = c(-1, -1, 1, 1),
                   AB = c(1, -1, -1, 1),
                   yBar = c(y1_bar, y2_bar, y3_bar, y4_bar))

# Total
sum(sign$I * sign$yBar)
sum(sign$A * sign$yBar)
sum(sign$B * sign$yBar)
sum(sign$AB * sign$yBar)

# Total/Divisor
sum(sign$I * sign$yBar)/(2^2)
sum(sign$A * sign$yBar)/((2^2)/2)
sum(sign$B * sign$yBar)/((2^2)/2)
sum(sign$AB * sign$yBar)/((2^2)/2)

# Regression Coefficients (Divide the treatment by 2):
(sum(sign$I * sign$yBar)/(2^2))
(sum(sign$A * sign$yBar)/((2^2)/2))/2
(sum(sign$B * sign$yBar)/((2^2)/2))/2
(sum(sign$AB * sign$yBar)/((2^2)/2))/2

## 10.3.1.2 Examining residuals to Validate the Model ##
factorial2.2$trtResid <- factorial2.2$y - factorial2.2$trtMean
factorial2.2

## 10.3.1.2.1 Testing for Normal Residual Distribution ##

# The following plot is the STANDARDIZED residuals plotted against the THEORHETICAL quantiles
plot(model2, which = 2)

## 10.3.1.2.2 Testing for Error Independence ##
plot(model2, which = 1)

## 10.3.1.2.3 Testing for Constant Error Variance ##
plot(model2, which = 1)

## 10.3.1.2.4 Testing for Model Additivity ##
max(factorial2.2$y)/min(factorial2.2$y)

### 10.3.1.3 Calculation of the Variation in the Response Variable Due to ###
################# Each Factor, Factor Interactions and Errors ###############

(sst <- sum(anova(model2)$"Sum Sq"))

## Percentage varitions in response due to each factor and factor interactions:
# Paradigm (Factor A)
(ssa <- anova(model2)$"Sum Sq"[1]/sst) * 100

# Knowledge (Facot B)
(ssb <- anova(model2)$"Sum Sq"[2]/sst) * 100

# Paradigm:Knowledge (Factor AB)
(ssab <- anova(model2)$"Sum Sq"[3]/sst) * 100

# Error
(sse <- anova(model2)$"Sum Sq"[4]/sst) * 100

### 10.3.1.4 Calculation of the Statistical Significance of the ###
#### Variation Due to Each Factor and Factor Interactions #########

## last column in anova table shows the p-values:
anova(model2)

## SE(paradigm)
(mse <- sse/((2^2)*(3-1)))
r <- 3
k <- 2

sqrt((1/(3 *(2^(2-2)))) * mse)

sqrt((1/(3*2^(2-2)))*anova(model2)$'Mean Sq'[1])

sqrt((1/(3*2^(2-2)))*anova(model2)$'Mean Sq'[2])

sqrt((1/(3*2^(2-2)))*anova(model2)$'Mean Sq'[3])

sqrt((1/(3*2^(2-2)))*anova(model2)$'Mean Sq'[4])

summary(model2)
confint(model2)

## Recommentations on the Best Alternative of Each Factor
plot(y~paradigm, data = factorial2)
plot(y~knowledge, data = factorial2)

plot(x = c(0,1),
     y = c(mean(subset(factorial2, knowledge == "with" & paradigm == "new")$y),
           mean(subset(factorial2, knowledge == "with" & paradigm == "oo")$y)),
     xlim = c(-0.2,1.2),
     ylim = c(12, 80),
     type = "o",
     xaxt = "n",
     ylab = "Response",
     xlab = "Paradigm")
lines(x = c(0,1),
      y = c(mean(subset(factorial2, knowledge == "without" & paradigm == "new")$y),
            mean(subset(factorial2, knowledge == "without" & paradigm == "oo")$y)),
      type = "o")
axis(1, at =c(0,1), labels = c("new", "oo"))

##############################################
## 10.3.2 Analysis for 2^k Factorial Design ##
##############################################

# *** Analysis using the sign table method as presented in the book
#     will not be demonstrated ***

factorial3 <- data.frame(strategy = rep(rep(c("gamma", "pi"), each = 2), times = 4),
                         size = rep(rep(c("large", "small"), each = 4), times = 2),
                         validationTime = rep(c("long", "short"), each = 8),
                         y = c(59, 61,
                               74, 70,
                               50, 58,
                               69, 67,
                               50, 54,
                               81, 85,
                               46, 44,
                               79, 81)
                         )

model3 <- lm(y ~ strategy*size*validationTime, data = factorial3)
##  By using '*' in the model statement above, R will include all
##  main effects & interactions

summary(model3)

## Note significance for strategy, size, and strategy:validationTime
anova(model3)

## The portion of of variation explained by each factor & its interactions:
(sst <- sum(anova(model3)$"Sum Sq"))

# Strategy
(ssa <- anova(model3)$"Sum Sq"[1]/sst) * 100

# Size
(ssb <- anova(model3)$"Sum Sq"[2]/sst) * 100

# Validation Time
(ssc <- anova(model3)$"Sum Sq"[3]/sst) * 100

# Strategy:Size
(ssab <- anova(model3)$"Sum Sq"[4]/sst) * 100

# Strategy:Validation Time
(ssac <- anova(model3)$"Sum Sq"[5]/sst) * 100

# Size:Validation Time
(ssbc <- anova(model3)$"Sum Sq"[6]/sst) * 100

# Strategy:Size:Validation Time
(ssabc <- anova(model3)$"Sum Sq"[7]/sst) * 100

# Error
(sse <- anova(model3)$"Sum Sq"[8]/sst) * 100

## Plots:
plot(y ~ strategy, data = factorial3)

plot(y ~ size, data = factorial3)

plot(y ~ validationTime, data = factorial3)

#### Figure 10.7 (a)
plot(x = c(rep(1, times = 8), rep (2, times=8)),
     y = c(subset(factorial3, strategy == "gamma")$y,
           subset(factorial3, strategy == "pi")$y),
     xlim = c(0.5, 2.5),
     ylim = c(40,90),
     xaxt = "n",
     xlab = "strategy",
     ylab = "response")
lines(x = c(1,2),
      y = c(mean(subset(factorial3, strategy == "gamma")$y),
            mean(subset(factorial3, strategy == "pi")$y)),
      type = "o",
      col = "violet",
      pch = 19)
axis(side = 1, at = c(1,2), labels = c("gamma", "pi"))

#### Figure 10.7 (b)
plot(x = c(rep(1, times = 8), rep (2, times=8)),
     y = c(subset(factorial3, size == "large")$y,
           subset(factorial3, size == "small")$y),
     xlim = c(0.5, 2.5),
     ylim = c(40,90),
     xaxt = "n",
     xlab = "size",
     ylab = "response")
lines(x = c(1,2),
      y = c(mean(subset(factorial3, size == "large")$y),
            mean(subset(factorial3, size == "small")$y)),
      type = "o",
      col = "red",
      pch = 19)
axis(side = 1, at = c(1,2), labels = c("large", "small"))

#### Figure 10.7 (c)
plot(x = c(rep(1, times = 4), rep (2, times = 4)),
     y = c(subset(factorial3, strategy == "gamma" & validationTime == "long")$y,
           subset(factorial3, strategy == "pi"& validationTime == "long")$y),
     xlim = c(0.5, 3),
     ylim = c(40,90),
     xaxt = "n",
     xlab = "strategy",
     ylab = "response",
     col = "forestgreen")
points(x = c(rep(1, times = 4), rep (2, times = 4)),
       y = c(subset(factorial3, strategy == "gamma" & validationTime == "short")$y,
       subset(factorial3, strategy == "pi"& validationTime == "short")$y),
       col = "dodgerblue")
lines(x = c(1,2),
      y = c(mean(subset(factorial3, strategy == "gamma" & validationTime == "long")$y),
            mean(subset(factorial3, strategy == "pi"& validationTime == "long")$y)),
      type = "o",
      col = "forestgreen",
      pch = 15)
lines(x = c(1,2),
      y = c(mean(subset(factorial3, strategy == "gamma" & validationTime == "short")$y),
            mean(subset(factorial3, strategy == "pi"& validationTime == "short")$y)),
      type = "o",
      col = "dodgerblue",
      pch = 15)
axis(side = 1, at = c(1,2), labels = c("gamma", "pi"))
text(2,
     mean(subset(factorial3, strategy == "pi"& validationTime == "short")$y),
     labels = "short validation time", col = "dodgerblue", pos = 4)
text(2,
     mean(subset(factorial3, strategy == "pi"& validationTime == "long")$y),
     labels = "long validation time", col = "forestgreen", pos = 4)

#############################################################
## 10.4 ANALYSIS FOR FACTORIAL DESIGNS WITHOUT REPLICATION ##
#############################################################

###################################################################
# 10.4.1 Determining the Significance of Factors and Interactions #
###################################################################
factorial4 <- data.frame(developers = rep(c("inhouse", "external"), times = 8),
                         processMaturity = rep(rep(c("high", "low"), each = 2), times = 4),
                         devExperience = rep(rep(c("inexperienced", "experienced"), each = 4), times = 2),
                         complexity = rep(c("difficult", "simple"), each = 8),
                         y = c(45, 71, 48, 65, 68, 60, 80, 65, 43, 100, 45, 104, 75, 86, 70, 96))

## I will show a proceedure similar to the one described in section 10.4.4
# First, look solely at main effects (i.e. no interaction terms in the model)
# determine which main effect doesn't have a large effect on y
model4 <- lm(y ~ developers + processMaturity + devExperience + complexity, data = factorial4)
summary(model4)
## As you can see in the model summary output, processMaturity (factor B) has a very high
#  p-value at 0.69! Thus, we can discount process maturity from a more saturated model which
#  includes interactions:
model4.2 <- lm(y ~ developers*devExperience*complexity, data = factorial4)
summary(model4.2)

plot(y ~ developers, data = factorial4)
plot(y ~ processMaturity, data = factorial4)
plot(y ~ devExperience, data = factorial4)
plot(y ~ complexity, data = factorial4)


###################################
## 10.5 HANDLING UNBALANCED DATA ##
###################################

###########################################
# 10.5.1 Proportional Data: A Simple Case #
###########################################

proportional <-  data.frame(experience = c(rep(">1yr", times = 10),
                                           rep("1yr", times = 5),
                                           rep("inexp", times = 5)),
                            inheretance = c(rep(c("3L", "2L"), each = 4), "1L", "1L",
                                            rep(c("3L", "2L"), each = 2), "1L",
                                            rep(c("3L", "2L"), each = 2), "1L"),
                            time = c(130, 155, 74, 180,
                                     34, 40, 80, 75,
                                     70, 58,
                                     159, 126,
                                     136, 115,
                                     45,
                                     138, 160,
                                     150, 139,
                                     96))

model5 <- lm(time ~ experience*inheretance, data = proportional)

anova(model5)
