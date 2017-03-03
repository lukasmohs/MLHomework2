#Overview

# Homework 2 is about applying what you have learned in class into analysis in R. 
# You will draw from both your learning in lecture and discussion with the skills 
# you are developing in the workshop sessions.

# Get dataset from: http://datashare.is.ed.ac.uk/bitstream/handle/10283/128/IST_corrected.csv

library(dplyr)

# synthetic depression data
depressionData = data.frame( # do not change "depressionData"
  pregnant = c(1,0,1,1),
  depressed = c("yes","yes","no","no") %>% as.factor(),
  hospitalized = c(1, 0, 0, 0) %>% as.logical()
) %>% tbl_df()