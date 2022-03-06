###########################################################
# this script takes our results and analyzes them
set.seed(42)

############################################
# load dependencies
library(tidyverse)

############################################################
# load our TESTING data from github (groups 5)
testing_data <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group5.csv") 

############################################################
# load our RESULTS
results <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_one-hot-(300).csv") 

############################################################
# sanity check we tested what we thought we did
check1 <- testing_data %>% filter(msg_id %in% results$msg_id) # 2000
check2 <- results %>% filter(msg_id %in% testing_data$msg_id) # 2000

############################################################
# visualize all of our RESULTS side-by-side
results <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_one-hot-(300).csv") 