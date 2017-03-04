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
sum(depressionData$hospitalized==depressionData$predictedHospitalized) / nrow(depressionData) #[1] 0.75
#the sensitivity of the model? 

#the specificity of the model? 
specificity <- 1-(FP/(TN+FP))
specificity

#the precision of the model? 
precision <- TP/(TP+FP) #[1] 0.5
precision

#the recall of the model?
recall <- TP/(TP+FN) #[1] 1
recall