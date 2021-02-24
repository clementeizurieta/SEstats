###################
#### CHAPTER 9 ####
###################

###########
### 9.1 ###
###########
# Table 9.1. Data taken for the example of a design with one blocking variable

## In R, data must be in "long" format (Table 9.1 in the text is in "wide" format).
# There are 2 different factors in this data, the language used (the primary factor
# of interest) and the programmer (the blocking factor).
(languages <- data.frame(value = c(9.3, 9.4, 9.6, 10.0, 9.4, 9.3, 9.8, 9.9, 9.2, 9.4, 9.5, 9.7, 9.7, 9.6, 10.0, 10.2),
                        language = rep(c("A", "B", "C", "D"), each = 4),
                        programmer = rep(c("I", "II", "III", "IV"), times = 4)))

###########
### 9.2 ###
###########

################################################
## 9.2.1 Identification of Mathematical Model ##
################################################

## The following is how to set up a linear model in R, using the lm() function.
# Read as "value is a function of the language and the programmer".
# The tilde denotes "is a function of". The model statement refers the column names
# of the 'languages' data frame, therefore the data frame must be specified using
# data = dataframename argument.

# The order of the explanatory variables does not matter.  Note that R takes care of converting 
# the variables into categorical variables.  If you wanted to look at the levels of both variables
# I could do something like this:
(levels(as.factor(languages$language)))
(levels(as.factor(languages$programmer)))

#If I wanted to look at the mean of all the programmers of type III for example:
(mean(languages$value[as.factor(languages$programmer)=='III']))

# Generate the multiple linear model
model <- lm(value ~ language + programmer, data = languages)

## Calling just the model doesn't give a whole lot of information:
model

## Using the function summary() to get more information.
# The summary gives you the fitted regression equation which is:
# mu(y|x) = 9.35 + 0.025(LanB) - 0.125(LanC) + 0.3(LanD) + 0.025(II) +0.325(III) + 0.55(IV)
#
# This says that the regression line for say Programmer II using language B is:
# mu(y|x) = 9.35 +0.025(LabB) + 0.025(II)   All other terms are multiplied by 0.
#
# Note that the intercept is the baseline.
# A categorical variable with k levels has k-1 dummy variables.
# So the estimated value of y (the response variable) given all languages and programmer dummy 
# variables = 0 (which means programming language A and programmer level I) is 9.35
#
summary(model)

# Confidence intervals of our variables.  Provides the range for the slope of the variable.  That is
# we are 95% confident of the true slope of a particular variable.
confint(model, level=0.95)

#Some plots
lang_cat<-as.factor(languages$language)
prog_cat<-as.factor(languages$programmer)
plot(prog_cat, languages$value)
points(prog_cat, languages$value, col="blue", xlab="Programmer Type", ylab="language values", main="Programmer Type vs Language Values")

## Because the functions in R do the math for you, the calculations
# in table 9.2 are unnecessary, but the following code shows you how
# to calculate those values:

languages$effect <- (languages$value - 9.5)*10
languages

## block means (programmer)
(blockmeans <- with(languages, tapply(effect, programmer, mean)))

## alternative means (language used)
(alternativemeans <- with(languages, tapply(effect, language, mean)))

## overall mean (y bar)
(y_bar <- mean(languages$effect))

## block effect (programmer)
(blockeffect <- blockmeans - y_bar)

## alternative effect (language)
(alteffect <- alternativemeans - y_bar)

############################
## 9.2.2 Model Validation ##
############################

###################################################
# 9.2.2.1 Testing for the Absence of Interactions #
###################################################

## The values of table 9.3 can be calculated by setting up a model of the effect
# in the following way:
effectmodel <- lm(effect ~ language + programmer, data = languages)

## Table 9.3: Experimental Residuals
(exp_resid <- data.frame(y_ij = languages$effect, yhat_ij = fitted.values(effectmodel), e_ij = round(residuals(effectmodel), 2)))

## Figure 9.1: Distribution of residuals against estimated (fitted) values for our example
plot(e_ij ~ yhat_ij,
     data = exp_resid,
     xlim = c(-4, 8),
     ylim = c(-1.5, 1.5))
abline(h = 0, lty = 2)
# * note that this plot looks slightly different from the one shown in the book
#   because the book authors staggered repeat error values (a plotting term called
#   'jittering', where a random small number is added), whereas R plots over
#   the repeat values. Notice how there are 16 point in the book plot and 14 in
#   this R plot. But the point remains, there is no trend in the effect residuals.

##########################################
# 9.2.2.2 Testing for Residual Normality #
##########################################

## R can plot the normal probability graph discussed in the book (called normal Q-Q in R):
plot(model, which = 2)

## The 'which' argument above specifies which of 4 different model validation plots
# I would like to see. To see all of them use:
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))
# * note: using par(mfrow = c(2,2)) allows me to put all plots on the same sheet.
#   I then change it back to the default.

##############################################
# 9.2.2.3 Testing for Independence of Errors #
##############################################

## Figure 9.1 represents the residuals against the estimated values, but the more
# typical way of testing of independence in errors for any model is to plot the residuals
# versus fitted for the model of interest instead of the effects model in Figure 9.1.  It is
# the same thing.  Figure one uses the transformed values, and here we use the raw values.

## The best way is to use the default plot given by the model object:
plot(model, which = 1)

## The following code tells you how to extract the residuals and fitted values of
# any model and to make the residuals v. fitted plot from these values instead:

## The residuals of the model are part of the model object.
# They can easily be found by using the residuals() function.
residuals(model)

## The fitted values are found by using the fitted() function:
fitted(model)

## Then plotting residuals as a function of fitted values:
plot(residuals(model) ~ fitted(model))
abline(h = 0, lty = 2)

###################################################
# 9.2.2.4 Testing for Constant Variance of Errors #
###################################################

## Figure 9.3(a)
plot(residuals(effectmodel) ~ prog_cat)

## Figure 9.3(b)
plot(residuals(effectmodel) ~ lang_cat)

#############################################################################
## 9.2.3 Factor-, Block-, and Error-Induced Variation in Response Variable ##
#############################################################################

## The analysis of variance table will show you the mean of the
# sum of squares of the block effects (SSB), the sum of squares of the factor
# alternatives (SSA), the sum of squares of the error (SSE).
# **remember the book multiplied these numbers by 10
anova(model)

#######################################################################################
## 9.2.4 Calculation of the Statistical Significance of the Factor-Induced Variation ##
#######################################################################################

## Also looking at the anova table in R:

## In this table the the sum of squares for the factor alternatives, SSA (the 'languages'
# factor in this model) is in the Sum Sq column, languages row

## SSB, sum of squares for the block effects, the 'programmer' in this model, is the second
# row down in the Sum Sq column.

## SSE, sum of squared errors, or 'residuals', is the 3rd row of the Sum Sq column

## The anova table in R also gives you the F-value (top row, F value column) and the
# p-value (top row, Pr(>F)) of this F-value in the correct F-distribution.

## The F-value and p-value for the block row, i.e. programmer, will be explained later
anova(model)

# What if we had not used a Block design? and the randomization had produced the same table
# as in figure 9.1?  The F-test is polluted with errors.
test<-lm(effect ~ language, data = languages)
anova(test)

####################################################################
## 9.2.5 Recommendations on the Optimal Alternative of the Factor ##
####################################################################

## Extracting tvalues from languages B, C and D
tvalues <- summary(model)[["coefficients"]][, "t value"]

## Here I am releveling the language factor to extract the tvalue for
# language A. The intercept t-value is not the t-value for language A.
languages$language <- relevel(languages$language, ref = "B")
modelrelevel <- lm(value ~ language + programmer, data = languages)
tval_A <- summary(modelrelevel)[["coefficients"]][, "t value"][2]


## The following plot tells you similar information as Figure 9.5
# The point is that language D is different than the other languages because it is
# outside of the 95% confidence interval.
tDistPlot(df = 9, tvalue = round(c(tval_A, tvalues[2:4]), 2), alpha = 0.05, distWidth = 5)

###########
### 9.3 ###
###########

## The following code calculates organizes the data from the latin square into
# the 'long' format. I then create a linear model with this data and show the anova table
# with the same values of sum of squares and F-value as seen in the book.

## Note that there is an error in Table 9.7 in the book. The values for
# programmer 5, program 4, language C should be -3 and not -34.5

languages2 <- data.frame(programmers = as.factor(rep(1:5, each = 5)),
                         program = as.factor(rep(1:5, times = 5)),
                         language = c("A", "B", "C", "D", "E",
                                      "B", "C", "D", "E", "A",
                                      "C", "D", "E", "A", "B",
                                      "D", "E", "A", "B", "C",
                                      "E", "A", "B", "C", "D"),
                         value = c(-1, -8, -7, 1, -3,
                                   -5, -1, 13, 6, 5,
                                   -6, 5, 1, 1, -5,
                                   -1, 2, 2, -2, 4,
                                   -1, 11, -4, -3, 6))

model4 <- lm(value ~ language + program + programmers, data = languages2)
anova(model4)

###########
### 9.5 ###
###########

## For analysis of designs with more than two blocking variables in R, all you need to do
# is add that predictor variable to the table of data and add it into the model
# statement in the lm() function. For example:
# lm(value ~ factorAlternative + blocking1 + blocking2 + blocking3 + blocking4 + ..., data = dataTable)


###########
### 9.6 ###
###########

## In R, denote a missing value with NA. Do not put in 0 for missing data because
# remember that 0 is a meaningful value!

## Table 9.16 Incomplete randomised black design for the programming language experiment
languages3 <- data.frame(programmer = rep(c("I", "II", "III", "IV"), times = 4),
                         alternative = rep(c("A", "B", "C", "D"), each = 4),
                         value = c(-2, -1, -3, 2, -1, -2, -1, 1, 1, NA, 0, 5, 5, 4, 2, 7))

(yprime <- sum(languages3$value, na.rm = TRUE))

(yprimei <- with(languages3, tapply(value, alternative, sum, na.rm = TRUE)))
(yprimej <- with(languages3, tapply(value, programmer, sum, na.rm = TRUE)))

(x <- (4*yprimei[3] + 4*yprimej[2] - yprime)/((4-1)*(4-1)))

## Replace the NA value in the data frame with the estimated number
languages3[10,]$value <- x
languages3[10,]

## Run the model with this data and look at anova table. Remember that the data
# given in table 9.16 is incomplete, so the numbers and model are slightly
# different than the anova Table 9.17
model5 <- lm(value ~ programmer + alternative, data = languages3)
anova(model5)

###########
### 9.7 ###
###########

## Table 9.18 Balanced incomplete block design for the tools experiment
(tools <- data.frame(individual = rep(c("I", "II", "III", "IV"), each = 4),
                    tool = rep(c(1, 2, 3, 4), times = 4),
                    value = c(73, NA, 73, 75,
                              74, 75, 75, NA,
                              NA, 67, 68, 72,
                              71, 72, NA, 75)))
## Cannot use the previous methods of making a model and looking at the anva table
# for incomplete block designs. See below. Must perform the calculations in Table 9.19/
model6 <- lm(value ~ tool + individual, data = tools)
anova(model6)


