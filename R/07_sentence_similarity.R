###########################################################
# using this to play around with sentence similarity 
# and turns them into pretty CSVs that fit on github
set.seed(42)

############################################
# load dependencies
library(tidyverse)

#####################################################################################################
# cosine similarity functions
cosine_sim <- function(a, b) crossprod(a,b)/sqrt(crossprod(a)*crossprod(b))
find_similar_comments <- function(target_vector, matrix, comments) {
  cos_sims <- apply(matrix, 1,
                    FUN = function(y) 
                      cosine_sim(target_vector, y))
  out <- as.data.frame(cos_sims, row.names = rownames(cos_sims), stringsAsFactors=FALSE) %>%
    tibble::rownames_to_column("msg_id") %>%
    select(msg_id, similarity := cos_sims) %>%
    arrange(-similarity) %>%
    mutate(rank = row_number(),
           msg_id=as.numeric(msg_id)) %>%
    left_join(comments, by="msg_id")
}

#####################################################################################################
# load all comments
comments <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group1.csv") %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group2.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group3.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group4.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group5.csv"))



#####################################################################################################
# load BERTft sentence embeddings
bert <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/bert_ft_768/bert_ft_encoded768_group1A.csv") %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/bert_ft_768/bert_ft_encoded768_group1B.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/bert_ft_768/bert_ft_encoded768_group2A.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/bert_ft_768/bert_ft_encoded768_group2B.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/bert_ft_768/bert_ft_encoded768_group3A.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/bert_ft_768/bert_ft_encoded768_group3B.csv")) %>%  
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/bert_ft_768/bert_ft_encoded768_group4A.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/bert_ft_768/bert_ft_encoded768_group4B.csv")) %>%  
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/bert_ft_768/bert_ft_encoded768_group5A.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/bert_ft_768/bert_ft_encoded768_group5B.csv"))
print(length(unique(bert$msg_id)))
rownames(bert) <- bert$msg_id
bert <- bert %>% select(-msg_id, -token_count, -my_group, -my_role, -label)
bert <- data.matrix(bert)

#####################################################################################################
# load custom768 sentence embeddings
custom768 <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/custom768/custom_encoded768_group1A.csv") %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/custom768/custom_encoded768_group1B.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/custom768/custom_encoded768_group2A.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/custom768/custom_encoded768_group2B.csv")) %>%  
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/custom768/custom_encoded768_group3A.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/custom768/custom_encoded768_group3B.csv")) %>%    
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/custom768/custom_encoded768_group4A.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/custom768/custom_encoded768_group4B.csv")) %>%    
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/custom768/custom_encoded768_group5A.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/custom768/custom_encoded768_group5B.csv")) 
print(length(unique(custom768$msg_id)))
rownames(custom768) <- custom768$msg_id
custom768 <- custom768 %>% select(-msg_id, -token_count, -my_group, -my_role, -label)
custom768 <- data.matrix(custom768)


#####################################################################################################
# what else is close to 18052 I want to know??

# example of how bert 
check1A <- find_similar_comments(bert["18052",], bert, comments) %>% filter(grepl("asexual",raw_text))
check1B <- find_similar_comments(custom768["18052",], custom768, comments) %>% filter(grepl("asexual",raw_text))

# the "custom" similarity (768) is more human readable

# good LOTR example...
checkC <- find_similar_comments(custom768["28126",], custom768, comments)
checkCa <- checkC %>% filter(grepl("gandalf",raw_text))


#####################################################################################################
# playing with special BERT encodings
#setwd("/Users/joshpause/Desktop/Experiments/NLPEncodingExperiment/R")
samples <- read.csv("testing_BERT_embeddings7.csv") %>%
  mutate(msg_id=row_number())
colnames(samples) <- c(paste0("dim",seq(1,768,1)),"msg","msg_id")
rownames(samples) <- samples$msg_id
samples_comments <- samples %>% select(msg_id, msg)
samples_matrix <- samples %>% select(-msg_id, -msg) %>% data.matrix()

# how do these respond?
checkF <- find_similar_comments(samples_matrix["5",], samples_matrix, samples_comments)
