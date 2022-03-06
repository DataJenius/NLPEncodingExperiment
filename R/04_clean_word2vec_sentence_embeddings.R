################################################################
# this script allows us to view our word2vec sentend embeddings
# and split them into 5 files so they fit on github
set.seed(42)

############################################
# load dependencies
library(tidyverse)

############################################################
# load our embeddings data from colab
embeddings <- read.csv("all_word2vec_embeddings.csv") 
colnames(embeddings) <- c(paste0("dim ",seq(1,300,1)),"msg_id")

############################################################
# display a sample of what this data looks like
zeros <- rep(0,301)
dots <- rep("...",301)
sample <- data.frame(a=t(embeddings[1,]),
                     b=t(embeddings[2,]),
                     dots=dots,
                     c=t(embeddings[3,]),
                     d=t(embeddings[4,]),
                     e=zeros,
                     f=zeros,                     
                     g=zeros)
rownames(sample) <- paste0("dim ",seq(1,301,1))
colnames(sample) <- c("i","love","...","luke","skywalker","[PAD]","[PAD]","[PAD]")


############################################################
# load ALL of our data from github (groups 1, 2, 3, 4, 5)
comments <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group1.csv") %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group2.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group3.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group4.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group5.csv")) %>%
  select(msg_id, token_count, my_group, my_role, label)

############################################################
# join the word2vec sentence encoding
comments_word2vec <- comments %>%
  left_join(embeddings, by="msg_id")


############################################################
# save as 5 different CSV files so they fit on github
for(i in seq(1,5,1)) {
  print(i)
  tmp.data <- comments_word2vec %>%
    filter(my_group==i) 
  filename <- paste0('word2vec_encoded300_group',i,'.csv')
  write.csv(tmp.data, filename, row.names = FALSE)
}

