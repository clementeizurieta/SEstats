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


# domain I, technique R:
coefficients(model1)[1]

# domain II, technique R:
coefficients(model1)[1] + coefficients(model1)[2]

# domain III, technique R:
coefficients(model1)[1] + coefficients(model1)[3]

# domain I, technique S:
coefficients(model1)[1] + coefficients(model1)[4]

# domain I, technique T:
coefficients(model1)[1] + coefficients(model1)[5]

# domain I, technique U:
coefficients(model1)[1] + coefficients(model1)[6]

# domain II, technique S:
coefficients(model1)[1] + coefficients(model1)[4] + coefficients(model1)[7]

# domain III, technique S:
coefficients(model1)[1] + coefficients(model1)[4] + coefficients(model1)[8]

# domain II, technique T:
coefficients(model1)[1] + coefficients(model1)[5] + coefficients(model1)[9]

# domain III, technique T:
coefficients(model1)[1] + coefficients(model1)[5] + coefficients(model1)[10]

# domain II, technique U:
coefficients(model1)[1] + coefficients(model1)[6] + coefficients(model1)[11]

# domain III, technique U:
coefficients(model1)[1] + coefficients(model1)[6] + coefficients(model1)[12]

#################################
# 10.2.5 Testing Model Validity #
#################################

## Figure 10.2
plot(model1, which = 1)

plot(model1, which = 2)

plot(model1, which = 3)



