###########################################################
# this script takes our BERT embeddings from python
# and turns them into pretty CSVs that fit on github
set.seed(42)

############################################
# load dependencies
library(tidyverse)

# load the BERT embeddings python gave us
# not included in the repo because they are too big for github
bert_embed_raw <- read.csv("R/raw_BERT_embeddings_train.csv") %>%
  rbind(read.csv("R/raw_BERT_embeddings_test.csv")) %>%
  rbind(read.csv("R/raw_BERT_embeddings_val.csv"))

# give the columns pretty labels
colnames(bert_embed_raw) <- c(paste0("dim",seq(1,768,1)),"msg_id")

# load all comments (groups 1, 2, 3, 4, 5)
all_comments <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group1.csv") %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group2.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group3.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group4.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group5.csv"))

# final form
bert_encoded_comments <- all_comments %>%
  select(msg_id, token_count, my_group, my_role, label) %>%
  left_join(bert_embed_raw, by="msg_id")


# save as 10 different CSV files so they fit on github
for(i in seq(1,5,1)) {
  print(i)
  
  # save first 1000 in A
  tmp.data <- bert_encoded_comments %>% filter(my_group==i) 
  tmp.dataA <- tmp.data[1:1000,]
  filenameA <- paste0('bert_encoded768_group',i,'A.csv')  
  write.csv(tmp.dataA, filenameA, row.names = FALSE)
  
  # save second 1000 in B  
  tmp.dataB <- tmp.data[1001:nrow(tmp.data),]
  filenameB <- paste0('bert_encoded768_group',i,'B.csv')    
  write.csv(tmp.dataB, filenameB, row.names = FALSE)
}