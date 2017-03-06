#INFO: This file contains the plain source code, answers can be found in the .rmd file

# Homework 2 is about applying what you have learned in class into analysis in R. 
# You will draw from both your learning in lecture and discussion with the skills 
# you are developing in the workshop sessions.


library(vegan)
library(survival)
library(bnlearn)
library(dplyr)
library(ggplot2)
library(lattice)
library(mice)
library(ROCR)
library(tableone)
library(caret)
library(rpart)

# Get dataset from: http://datashare.is.ed.ac.uk/bitstream/handle/10283/128/IST_corrected.csv

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
# corresponding row by a string. I changed "tree[active,'falseChild']" to "tree[active,'falseChild']"
# and tree[active,'trueChild']" to "tree[active,'trueChild']".

# Add a column of the predicted probabilities of hospitalization to depressionData. Display it. 
depressionData <- depressionData %>% mutate(probabilityHospitalized = predictOddsOnDataSet(tree,depressionData))

#Using a threshold probability of 0.5, what is:
depressionData <- mutate(depressionData, predictedHospitalized = ifelse(probabilityHospitalized > 0.5,"TRUE","FALSE"))
TP <- sum(depressionData$hospitalized=="TRUE" & depressionData$predictedHospitalized=="TRUE")
FP <- sum(depressionData$hospitalized=="FALSE" & depressionData$predictedHospitalized=="TRUE")
FN <- sum(depressionData$hospitalized=="TRUE" & depressionData$predictedHospitalized=="FALSE")
TN <- sum(depressionData$hospitalized=="FALSE" & depressionData$predictedHospitalized=="FALSE")

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
MLE

# what is the maximum a posteriori estimate for the prevalence of diabetes?
alpha <- 11
beta <- 21
MAP <- (individualsWithDiabetes + alpha -1) /
  (individualsWithDiabetes+individualsWithoutDiabetes+alpha+beta-2)
MAP #[1] 0.375

#Part 2: Analysis (9 points) Preliminaries
# Construct a so-called Table 1 for groups of {aspirin, no aspirin} use, 
# including information on age, gender, systolic blood pressure, and conscious state.

listVariables <- c("AGE", "SEX", "RSBP", "RCONSC")
categorialVariables <- c("SEX","RCONSC")
table1 <- CreateTableOne(vars = listVariables, data = data, 
              factorVars = categorialVariables,  strata = "RXASP")
table1

#Machine learning analysis
#Note: for this analysis, use a simple 50-50 train-test split.
dataTrain = data[seq(1,nrow(data),2),]
dataTest = data[seq(2,nrow(data),2),]

#Let our outcome of interest be 'dead or dependent at 6 months', 
# i.e. so that we have a binary classification problem. 

dataTrain <- mutate(dataTrain,DOD=ifelse(OCCODE==1|OCCODE==2,1,0))
dataTest <- mutate(dataTest,DOD=ifelse(OCCODE==1|OCCODE==2,1,0))

# leads to similar results
#dataTrain <- mutate(dataTrain,DOD=ifelse(FDEAD=="Y"|FDENNIS=="Y",1,0))
#dataTest <- mutate(dataTest,DOD=ifelse(FDEAD=="Y"|FDENNIS=="Y",1,0))

#What percent of patients are dead or dependent at 6 months in your train set and test set? 
percentDeadInTrain <- sum(dataTrain$FDEAD=="Y" | dataTrain$FDENNIS=="Y")/nrow(dataTrain)
# percentDeadInTrain <- sum(dataTrain$OCCODE==1 | dataTrain$OCCODE==2)/nrow(dataTrain) # [1] 0.6351755
percentDeadInTrain # [1] 0.6336318
percentDeadInTest <- sum(dataTest$FDEAD=="Y" | dataTrain$FDENNIS=="Y")/nrow(dataTest)
#percentDeadInTest <- sum(dataTest$OCCODE==1 | dataTest$OCCODE==2)/nrow(dataTest) # [1] 0.6125746
percentDeadInTest # [1] 0.550319

# Choose which variables to include in your model. For example, remove variables for outcomes at 14 days 
# (because if you are dead at 14 days you are certainly dead at 6 months).
# Moreover, you should remove all features measured after baseline if you want to make 
# a prediction based on baseline data
# Similarly, specific indicators of the outcome should also be removed, 
# since those are measurements past the baseline that are not our outcome of interest. 
# For these reasons, you will need to remove clusters of variables. Justify your approach.

# First, just include the variables from sction: "Randomisation data" (according to: http://datashare.is.ed.ac.uk/bitstream/handle/10283/128/IST_variables.pdf).
# This includes all the information that were collected before Randomisation at the beginning of the study. 

dataTrain <- dataTrain[c(which( colnames(dataTrain)=="HOSPNUM") : which( colnames(dataTrain)=="RXHEP"), which( colnames(dataTrain)=="CNTRYNUM"), which( colnames(dataTrain)=="DOD"))]
dataTest <- dataTest[c(which( colnames(dataTest)=="HOSPNUM") : which( colnames(dataTest)=="RXHEP"), which( colnames(dataTest)=="CNTRYNUM"), which( colnames(dataTest)=="DOD"))]

# Use the following machine learning algorithms: logistic regression, naive Bayes, Tree Augmented Naive Bayes, and decision tree 
# (specify any parameters you set that are not the default). The packages that you may find useful here are: 'glm', 'bnlearn', and 'rpart', 
# but you may use others if desired. In a table, report the accuracy with 95% confidence intervals for each algorithm.

dataTrain$DOD <- as.factor(dataTrain$DOD)

dataTest$DOD <- as.factor(dataTest$DOD)



############## Naive Bayes   ##############

library(ROCR) 
library(e1071)
nb = naiveBayes(DOD~.,dataTrain)

bayesAccuracy <- sum(predict(nb, dataTest)==dataTest$DOD) / nrow(dataTest)
bayesPredictedProbabilities <- predict(nb, dataTest,"raw")
bayesPred <- prediction( bayesPredictedProbabilities[,2], dataTest$DOD)
bayesPerfROC <- performance(bayesPred,"tpr","fpr")
bayesPerfPR <- performance(bayesPred, "prec", "rec")

############## Tree Augmented NB ##############

tanTrain <- dataTrain[ ,sapply(dataTrain, is.factor)]
tanTest <- dataTest[ ,sapply(dataTest, is.factor)]

tan = tree.bayes(tanTrain, "DOD")
fittedTan = bn.fit(tan, tanTrain)
tanAccuracy <- (sum(predict(object=fittedTan, data=tanTest)==tanTest$DOD)) / nrow(tanTest)
tanPredictedProbabilities <- attr(predict(object=fittedTan, data=tanTest, prob=TRUE),"prob")
tanPredictedProbabilities <- data.frame(t(tanPredictedProbabilities))
tanPred <- prediction( tanPredictedProbabilities[2], tanTest$DOD)
tanPerfROC <- performance(tanPred,"tpr","fpr")
tanPerfPR <- performance(tanPred, "prec", "rec")

############## Logistic Regression ##############

lr <- glm(formula = DOD  ~ .,
          family=binomial(link="logit"),data = dataTrain, control = list(maxit = 100))
lrAccuracy <- (sum(predict(lr, dataTest, type="response")>0.5 & dataTest$DOD==1) 
                + sum(predict(lr, dataTest, type="response")<0.5 & dataTest$DOD==0) ) / nrow(dataTest)

lrPredictedProbabilities <- predict(lr, dataTest, type="response")
linPred <- prediction( lrPredictedProbabilities, dataTest$DOD)
linPerfROC <- performance(linPred,"tpr","fpr")
linPerfPR <- performance(linPred, "prec", "rec")

############## Decision Tree ##############

tree <- rpart(DOD~ .,
             data=dataTrain,
             method="class")
dtAccuracy <- (sum(predict(tree, dataTest, type="class")==dataTest$DOD)) / nrow(dataTest)
dtPredictedProbabilities <- predict(tree, dataTest, type = "prob")
dtPred <- prediction( dtPredictedProbabilities[,2], dataTest$DOD)
dtPerfROC <- performance(dtPred,"tpr","fpr")
dtPerfPR <- performance(dtPred, "prec", "rec")

############## Accuracy comparison  ##############
n <- nrow(dataTest)
dtAccuracyInCI <-  paste(toString(dtAccuracy-1.96*sqrt(dtAccuracy*(1-dtAccuracy)/n)),
                         toString(dtAccuracy+1.96*sqrt(dtAccuracy*(1-dtAccuracy)/n)),sep="; ")
lrAccuracyInCI <- paste(toString(lrAccuracy-1.96*sqrt(lrAccuracy*(1-lrAccuracy)/n)),
                        toString(lrAccuracy+1.96*sqrt(lrAccuracy*(1-lrAccuracy)/n)), sep="; ")
tanAccuracyInCI <- paste(toString(tanAccuracy-1.96*sqrt(tanAccuracy*(1-tanAccuracy)/n)),
                         toString(tanAccuracy+1.96*sqrt(tanAccuracy*(1-tanAccuracy)/n)), sep="; ")
bayesAccuracyInCI <- paste(toString(bayesAccuracy-1.96*sqrt(bayesAccuracy*(1-bayesAccuracy)/n)),
                           toString(bayesAccuracy+1.96*sqrt(bayesAccuracy*(1-bayesAccuracy)/n)), sep="; ")
accuracyComparisonMatrix <- matrix(c(dtAccuracyInCI,lrAccuracyInCI,tanAccuracyInCI,bayesAccuracyInCI),ncol=4,byrow=TRUE)
colnames(accuracyComparisonMatrix) <- c("Decision Tree","Logistic Regression", "Tree Augmented Naive Bayes", "Naive Bayes")
rownames(accuracyComparisonMatrix) <- c("Accuracy")
accuracyComparisonTable <- as.table(accuracyComparisonMatrix)
accuracyComparisonTable

## Plotting ##############

plot(linPerfROC, col="blue")
plot(tanPerfROC, add = TRUE, col="red")
plot(bayesPerfROC, add = TRUE, col="green")
plot(dtPerfROC, add = TRUE, col="violet")
title(main="ROC Comparison of different Classifiers")
legend(0.6,0.4,c("Logistic Regression","Tree Augmented Naive Bayes","Bayes Network","Decision Tree"),
       col=c("blue","red", "green","violet"), lwd=5, y.intersp = 0.4)

plot(linPerfPR, col="blue")
legend(0.6,0.9,c("Logistic Regression"),col=c("blue"), lwd=5)
plot(tanPerfPR, col="red")
legend(0.6,0.9,c("Tree Augmented Naive Bayes"),col=c("red"), lwd=5)
plot(bayesPerfPR, col="green")
legend(0.6,0.9,c("Bayes Network"),col=c("green"), lwd=5)
plot(dtPerfPR, col="violet")
legend(0.6,0.8,c("Decision Tree"),col=c("violet"), lwd=5)

#how well are we able to predict death or dependence at 6 months? [response required]
#what is the average treatment effect of aspirin on death or dependence at 6 months? Is aspirin significantly better than the alternative? [response required]
data <- mutate(data,DOD=ifelse(OCCODE==1|OCCODE==2,1,0))
aspirinATE <- ((sum(data$DOD==1&data$RXASP=="N")/sum(data$RXASP=="N"))-(sum(data$DOD==1&data$RXASP=="Y")/sum(data$RXASP=="Y"))) * 100
paste("Aspirin ATE: ", toString(aspirinATE),"%")
heparinATE <-((sum(data$DOD==1&data$RXHEP=="N")/sum(data$RXHEP=="N"))-(sum(data$DOD==1&data$RXHEP!="N")/sum(data$RXHEP!="N"))) * 100
paste("Heparin ATE: ",toString(heparinATE), "%")
paste("Probability of randomly drawing this or an even more extrem sample: ",pbinom(sum(data$DOD==1&data$RXASP=="Y"), size=sum(data$DOD), prob=(sum(data$DOD==1&data$RXHEP!="N")/sum(data$DOD))),"%")
#of the algorithms tested, which algorithms perform the best? Justify your statement. [response required]
