
####################
#### ANOVA in R ####
####################

#####################
### One-way ANOVA ###
#####################

#  attach(InsectSprays) Using Attach is considered 'bad practice'

## The data() function loads R default datasets
# Notice in the environment it says <Promise>: the dataset will truly
# be loaded as soon as you use it directly in your code.
data("InsectSprays")

## the help() function gives information about the specified dataset
help("InsectSprays")

## str() gives information about the dataset
str(InsectSprays)

##############################
### Descriptive Statistics ###
##############################

### MEAN ###
## Returns the values in the count column where the spray column is
# equal to "A"
InsectSprays$count[InsectSprays$spray == "A"]
## Wrap in the mean() function to return mean of treatment "A"
mean(InsectSprays$count[InsectSprays$spray == "A"])

## To simplify the above line of code, use the with() function
# The with() function specifies a data environment in which the expession can be evaluated
# In the mean function, we refer directly to the columns within the dataframe. The
# with() statement specifies which dataframe those columns can be found
with(InsectSprays, mean(count[spray=="A"]))

## This tapply statement can be thought of as:
# "Apply the function 'mean' to the response variable 'count' for each treatment 'spray'"
with(InsectSprays, tapply(count, spray, mean))

### VARIANCE ###
## Variance within spray groups
with(InsectSprays, tapply(count, spray, var))

### SAMPLE SIZE ###
# Length of the data within each group
with(InsectSprays, tapply(count, spray, length))

### BOXPLOT ###
## Look at the data by creating a boxplot. Note the different arguements
# that improve plot visualization:
boxplot(count ~ spray,
        data = InsectSprays,
        xlab = "Type of spray",
        ylab = "Insect count",
        main = "InsectSprays data",
        col = "lightgray")

##########################
### A couple of Asides ###
##########################

## The importnace of releveling becomes apparent when analysing the output
# table from anova(). The p-values in the table rows are sequential.
## Say you want to compare the effectiveness of insect sprays to the no
# spray control treatment, which is spray "F":
InsectSprays$spray <- relevel(InsectSprays$spray, ref = "F")
# The above line of code replaces the spray column in the InsectSprays
# dataframe with the releveled data.


## When there is ordered structure in the treatment levels, where
# treatments are in creasing such as: F<B<C<D<E<A, use the ordered() function
Photoperiod <- ordered(InsectSprays$spray, levels = c("F", "B", "C", "D", "E", "A"))

# Check the re-ordering
tapply(InsectSprays$count, Photoperiod, mean)

# And check the re-leveling
with(InsectSprays, tapply(count, spray, mean))

# Check if a variable is a factor using the function is.factor()
is.factor(InsectSprays$spray)
is.factor(Photoperiod)

########################
### Run 1-way ANVOVA ###
########################

###################
## oneway.test() ##
###################

oneway.test(count ~ spray, data = InsectSprays)

## Change the var.equal option to TRUE
oneway.test(count ~ spray, data = InsectSprays, var.equal = TRUE)

###############################
## Run and ANOvA using aov() ##
###############################
## The aov() function analyses a one-way anova
aov.out <- aov(count ~ spray, data = InsectSprays)
summary(aov.out)

####################
## Post Hoc Tests ##
####################
TukeyHSD(aov.out)

###############
## Contrasts ##
###############
summary.lm(aov.out)

## Alternatively, to create a linear model:
aov.lm <- lm(count ~ spray, data = InsectSprays)
summary(aov.lm)

######################
## Test Assumptions ##
######################
bartlett.test(count ~ spray, data = InsectSprays)

##########################
## Model Checking plots ##
##########################
## Run the following line and follow the prompts in the command line:
plot(aov.out) # same as plot(aov.lm)

#########################################
## Non-parametric alternative to ANOVA ##
#########################################
kruskal.test(count ~ spray, data = InsectSprays)

#########################################
## ANOVA as Linear Regression Analysis ##
#########################################
data("PlantGrowth")
summary(PlantGrowth)

## GROUP MEANS ##
with(PlantGrowth, tapply(weight, group, mean))

## GROUP VARIANCE ##
with(PlantGrowth, tapply(weight, group, var))

## BARTLETT TEST ##
with(PlantGrowth, bartlett.test(weight ~ group))

## LINEAR REGRESSION ##
lm.out <- with(PlantGrowth, lm(weight ~ group))
summary(lm.out)

# aov table:
summary.aov(lm.out)

# Post Hoc test
TukeyHSD(aov(lm.out))













