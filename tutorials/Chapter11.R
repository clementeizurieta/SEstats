####################
#### CHAPTER 11 ####
####################

nested <- data.frame(method = rep(c("A", "B"), each = 4),
                     tool = rep(c("withA", "withoutA", "withB", "withoutB"), each = 2),
                     y = c(10, 13,
                           14, 17,
                           13, 11,
                           15, 12))

###################################################
## 11.2 IDENTIFICATION OF THE MATHEMATICAL MODEL ##
###################################################

model1 <- lm(y ~ method + tool, data = nested)

##################################
## 11.3 VALIDATION OF THE MODEL ##
##################################
plot(model1, which = 1)
plot(model1, which = 2)
plot(model1, which = 3)
plot(model1, which = 5)


#########################################################################################
## 11.4 CALCULATION OF THE VARIATION IN THE RESPONSE VARIABLE DUE TO FACTORS AND ERROR ##
#########################################################################################

anova(model1)

(sst <- sum(anova(model1)$"Sum Sq"))

# Method
anova(model1)$"Sum Sq"[1]/sst * 100

# Tool
anova(model1)$"Sum Sq"[2]/sst * 100

# Error
anova(model1)$"Sum Sq"[3]/sst * 100

#############################################################################
## 11.5 STATISTICAL SIGNIFICANCE OF THE VARIATION IN THE RESPONSE VARIABLE ##
#############################################################################

anova(model1)


