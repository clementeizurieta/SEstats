####################
#### CHAPTER 13 ####
####################
####################
# Experimental Design
####################
# install.packages("AlgDesign")
library(AlgDesign)

#Generate a factorial design
gen.factorial(levels=c(3,2,3), nVars=3, varNames=c("A","B","C"));
# Generate a common 2^k design
gen.factorial(levels=2, nVars=3, varNames=c("A","B","C"))
# A 2^k design but restrict the true factors to k-1
gen.factorial(levels=2, nVars=3, factors=c(1,2), varNames=c("A","B","C"))
gen.factorial(levels=c(2,2,4), nVars=3, factors=c(1,2), varNames=c("A","B","C"))
gen.factorial(levels=2, nVars=4, factors=c(1,2,3), varNames=c("A","B","C","Block"))

#test a fractional design
mydesign<-gen.factorial(levels=2, nVars=4, varNames=c("A","B","C","Block"))
set.seed(54321)
(newdesign<-optFederov(~.,mydesign,8))


##########
## 13.1 ##
##########
(blocked <- data.frame(Tool = c("use", "use",
                               "use", "use",
                               "non-use", "non-use",
                               "non-use", "non-use",
                               "non-use", "non-use",
                               "use", "use",
                               "use", "use",
                               "non-use", "non-use"),
                      Maturity = c("mature", "mature",
                                   "immature", "immature",
                                   "mature", "mature",
                                   "immature", "immature",
                                   "mature", "mature",
                                   "immature", "immature",
                                   "mature", "mature",
                                   "immature", "immature"),
                      Requirements = c("clear", "clear",
                                       "ambiguous", "ambiguous",
                                       "ambiguous", "ambiguous",
                                       "clear", "clear",
                                       "clear", "clear",
                                       "clear", "clear",
                                       "ambiguous", "ambiguous",
                                       "ambiguous", "ambiguous"),
                      Block = as.factor(c(1, 1,
                                1, 1,
                                1, 1,
                                1, 1,
                                2, 2,
                                2, 2,
                                2, 2,
                                2, 2)),
                      Y = c(2, 3,
                            18, 22,
                            15, 25,
                            4, 6,
                            6, 14,
                            10, 15,
                            6, 9,
                            8, 12)))

##########
## 13.2 ##
##########

## Model: ##
blockedmodelfull <- lm(Y ~ Tool * Maturity * Requirements, data = blocked)

## Tests: ##
plot(blockedmodelfull, which = 1)
plot(blockedmodelfull, which = 2)
plot(blockedmodelfull, which = 3)

##########
## 10.3 ##
##########
anova(blockedmodelfull)

## SST ###
(sst <- sum(anova(blockedmodelfull)$"Sum Sq"))

# See anova table for values of SSA (i.e. Tool in anova table),
##  SSB (i.e. Maturity in anova table), SSC, etc.

###################
### Figure 13.1 ###
###################
## Interaction AB ##
plot(x = c(1,2),
     y = c(mean(subset(blocked, Maturity == "mature" & Tool == "use")$Y),
          mean(subset(blocked, Maturity == "mature" & Tool == "non-use")$Y)),
     type = "o",
     ylim =c(1, 20),
     xlim = c(0.9, 2.9),
     xaxt = "n",
     ylab = expression(bar(Y)),
     xlab = "Tool use",
     col = "blue",
     main = "Tool & Maturity interaction")
lines(x = c(1,2), y = c(mean(subset(blocked, Maturity == "immature" & Tool == "use")$Y),
                        mean(subset(blocked, Maturity == "immature" & Tool == "non-use")$Y)), type = "o")
axis(1, at = c(1,2), labels = c("with", "without"))
text(2, 15, "mature", pos = 4, col = "blue")
text(2, 7, "immature", pos = 4)

