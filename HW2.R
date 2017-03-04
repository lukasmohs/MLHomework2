#Overview

# Homework 2 is about applying what you have learned in class into analysis in R. 
# You will draw from both your learning in lecture and discussion with the skills 
# you are developing in the workshop sessions.

# Get dataset from: http://datashare.is.ed.ac.uk/bitstream/handle/10283/128/IST_corrected.csv

library(dplyr)
library(survival)
data <- read.csv("IST_corrected.csv")

# synthetic depression data
depressionData = data.frame( # do not change "depressionData"
  pregnant = c(1,0,1,1),
  depressed = c("yes","yes","no","no") %>% as.factor(),
  hospitalized = c(1, 0, 0, 0) %>% as.logical()
) %>% tbl_df()

# tree: a model that outputs the odds of hospitalization from inputs of data (datums)
tree = data.frame( # do not change "tree"
  splitVariable = c("depressed", "pregnant", NA, NA, NA),
  split = c("yes", 1, NA, NA, NA),
  trueChild = c(2, 4, NA, NA, NA),
  falseChild = c(3, 5, NA, NA, NA),
  odds = c(NA, NA, 0.1, 2, 3)
)

predictOddsOnDataSet = function(tree, data, active = 1) {
  apply(data, 1, (function(x) {predictedOdds(tree=tree, x, active=1)}) )
}

predictedOdds = function(tree, datum, active = 1) {
  if(is.na(tree[active,"splitVariable"])) { # leaf of tree, so output value 
    return(tree$odds[active])
  } else { # internal node of tree, so continue down tr ee to true/false child
    if( (datum[[tree[active,"splitVariable"] %>% as.character]] %>% as.character) == tree[active,"split"])
      return(predictedOdds(tree, datum, active = tree[active,"trueChild"])) else
        return(predictedOdds(tree, datum, active = tree[active,"falseChild"])) }
}

# First, verify to yourself that, for the fourth patient in depressionData , 
# the tree should have output an odds of 0.1.
predictedOdds(tree, depressionData[3,], active=1) # = [1] 0.1

# What did you change?
# The recursive call of the predictedOdds() function had a syntax error in the last argument:
# To either get the false or true child of the current node, one has indicate the value in the 
# corresponding row by a string. I changed "tree[active,falseChild]" to tree[active,"falseChild"].

# Add a column of the predicted probabilities of hospitalization to depressionData. Display it. 
depressionData <- depressionData %>% mutate(probabilityHospitalized = predictOddsOnDataSet(tree,depressionData))

TP <- sum(depressionData$hospitalized=="TRUE" & depressionData$predictedHospitalized=="TRUE")
FP <- sum(depressionData$hospitalized=="FALSE" & depressionData$predictedHospitalized=="TRUE")
FN <- sum(depressionData$hospitalized=="TRUE" & depressionData$predictedHospitalized=="FALSE")
TN <- sum(depressionData$hospitalized=="FALSE" & depressionData$predictedHospitalized=="FALSE")

#Using a threshold probability of 0.5, what is:
depressionData <- mutate(depressionData, predictedHospitalized = ifelse(probabilityHospitalized > 0.5,"TRUE","FALSE"))

#the accuracy of the model? 
accuracy <- sum(depressionData$hospitalized==depressionData$predictedHospitalized) / nrow(depressionData) #[1] 0.75
#or
accuracy <- (TP+TN) / (TP+FN+FP+TN) 
accuracy 

#the sensitivity of the model? 
sensitivity <- TP/(TP+FN) #[1] 1
sensitivity

#the specificity of the model? 
specificity <- 1-(FP/(TN+FP))
specificity

#the precision of the model? 
precision <- TP/(TP+FP) #[1] 0.5
precision

#the recall of the model?
recall <- TP/(TP+FN) #[1] 1
recall

# Suppose you want to know the prevalence of diabetes in Pittsburgh. 
# If you randomly survey 10 Pittsburghers and 5 of them state they have diabetes:
# maximum likelihood estimate
individualsWithDiabetes <- 5
individualsWithoutDiabetes <- 5
MLE <- individualsWithDiabetes/
  (individualsWithDiabetes+individualsWithoutDiabetes)
MLE #[1] 0.5

# what is the maximum a posteriori estimate for the prevalence of diabetes?
alpha <- 11
beta <- 21
MAP <- (individualsWithDiabetes + alpha -1) /
  (individualsWithDiabetes+individualsWithoutDiabetes+alpha+beta-2)
MAP #[1] 0.375

#Part 2: Analysis (9 points) Preliminaries
# Y: What was the definition of the primary outcome in this study? 
# Two primary outcomes were 'death within 14 days' and 'death, dependency, and incomplete recovery' at 6 months."
# Meanwhule the protocol just specified them as: 
# (a) 'death from any cause within 14 days'
# (b) 'death or dependency (ie, needing help from another person with daily activities) at 6 months'

# What is (are) the variable name(s) for the outcome?
# For both of the first study outcomes, the variables are called: 
# 'DDEAD' and described as 'Dead on discharge form' and
# 'FDEAD', which is described as 'Dead at six month follow-up (Y/N)'

# U: what is (are) the variable name(s) for the intervention, and what is (are) their possible values?
# DASP14 Aspirin given for 14 days or till death or discharge (Y/N)
# DLH14 Low dose heparin given for 14 days or till death/discharge (Y/N)
# DMH14 Medium dose heparin given for 14 days or till death/discharge (Y/N)
# DHH14 Medium dose heparin given for 14 days etc in pilot (combine with above)
# DOAC Other anticoagulants (Y/N)
# DGORM Glycerol or manitol (Y/N)
# DSTER Steroids (Y/N)
# DCAA Calcium antagonists (Y/N)
# DHAEMD Haemodilution (Y/N)
# DCAREND Carotid surgery (Y/N)
# DTHROMB Thrombolysis (Y/N)
# DMAJNCH Major non-cerebral haemorrhage (Y/N)

# DSCH Non trial subcutaneous heparin (Y/N)
# DIVH Non trial intravenous heparin (Y/N)
# DAP Non trial antiplatelet drug (Y/N)


# V, W: describe the covariates included and the population being studied.


# A patient was eligible for the study, if a physician identified an acute stroke,
# where the severity was not taken into account, but the onset must have been less than 48 hours previously.
# Further, there should be no evidence about intracranial haemorrhage
# and no clear indications for heparin or aspirin and neither contraindications to heparin or aspirin.
# In total, 19 435 patients were observed, which were between 16 and 99 years old with an average of 71.72 years.
# 46% of the population was female and 54% male. The delay of the stroke and the randomization
# was between one and 48 hours with a mean of 20.12 hours. 71% of the patients were awake during the onset,
# the other 29% were sleeping. The conscious level of the patients was mostly alert (77%), 22% were classified as drowsy 
# and 1% was without consciousness.

# Construct a so-called Table 1 for groups of {aspirin, no aspirin} use, 
# including information on age, gender, systolic blood pressure, and conscious state.

# install.packages("tableone")
library(tableone)


listVariables <- c("AGE", "SEX", "RSBP", "RCONSC")

categorialVariables <- c("SEX","RCONSC")

data$DASP14[data$DASP14 == "y"] <- "Y"
data$DASP14[data$DASP14 == "n"] <- "N"
data$DASP14 <- droplevels(data$DASP14)

table1 <- CreateTableOne(vars = listVariables, data = data, 
              factorVars = categorialVariables,  strata = "DASP14")
table1

#Machine learning analysis
#Note: for this analysis, use a simple 50-50 train-test split.
#Let our outcome of interest be 'dead or dependent at 6 months', 
# i.e. so that we have a binary classification problem. 
#What percent of patients are dead or dependent at 6 months in your train set and test set? 

dataTrain = data[1:(nrow(data)/2),]
dataTest = data[-(1:(nrow(data)/2)),]



