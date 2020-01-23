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

####################################################################################
# 10.2.3 Statistical Significance of the Variation Due to Factors and Interactions #
####################################################################################
anova(model1)

#################################################################
# 10.2.4 Recommendations on the Best Alternative of Each Factor #
#################################################################

## PLOTTING

# Values from table 10.2:
summary(model1)





#################################
# 10.2.5 Testing Model Validity #
#################################

## Figure 10.2
plot(model1, which = 1)

plot(model1, which = 2)

plot(model1, which = 3)



