####################
#### CHAPTER 14 ####
####################

################################
## 14.2.1 Mann-Whitney U-Test ##
################################
Tool1 <- c(18.3, 16.4, 22.7,
           17.8, 19.9, 25.3,
           16.1, 24.2)
Tool2 <- c(12.6, 14.1, 20.5,
           10.7, 15.9, 19.6, 12.9,
           15.2, 11.8, 14.7)
(toolerrors <- data.frame(tool = c(rep("tool1", times = 8), rep("tool2", times = 10)),
                         errors = c(Tool1, Tool2),
                         rank = c(rank(c(Tool1,Tool2)))))

(R1 <- sum(subset(toolerrors, tool == "tool1")$rank))
(R2 <- sum(subset(toolerrors, tool == "tool2")$rank))

N1 <- length(Tool1)
N2 <- length(Tool2)

(U <- N1*N2 + (N1*(N1+1))/2 - R1)

(mu_U <- N1*N2/2)

(sigma2_U <- (N1*N2*(N1+N2+1))/12)

(z <- (U - mu_U)/sigma2_U)

# In R
wilcox.test(Tool1, Tool2)

#####################################
## 14.2.2 Kruskal-Wallis or H-test ##
#####################################
(casetools <- data.frame(devtool = rep(c("A", "B", "C", "D", "E"), each = 5),
                        y = c(7,7,15,11,9,
                              12,17,12,18,18,
                              14,18,18,19,19,
                              19,25,22,19,23,
                              7,10,11,15,11),
                        rank = rank(c(7,7,15,11,9,
                                      12,17,12,18,18,
                                      14,18,18,19,19,
                                      19,25,22,19,23,
                                      7,10,11,15,11))))

## rank totals by development tool:
with(casetools,(tapply(rank, devtool, sum)))

## R has a built-in function for the Kruskal-Wallis test:
kruskal.test(rank~devtool, casetools)

###########
## 14.3  ##
###########
A <- c(0.47, 1.02, 0.33, 0.7, 0.94, 0.85, 0.39, 0.47)
B <- c(0.41, 1.00, 0.46, 0.61, 0.84, 0.87, 0.36, 0.51)
# Note: I removed Project 8 paired results because
## their difference in 0, and it throws an error in the
## following wilcoxon test function.

## R has a function for Wilcoxon signed rank test:
wilcox.test(A,B, paired = T)





