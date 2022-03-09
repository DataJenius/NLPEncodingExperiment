###########################################################
# this script takes our selected comments from Reddit
# and turns them into Custom Embedding encoded data
set.seed(42)

############################################
# load dependencies
library(tidyverse)
library(tidytext)
library(tictoc)
library(widyr) # for pairwise_count function
library(irlba) # for dimensionality reduction
library(sjmisc) # to rotate our data frames


############################################################
# load our TRAINING data from github (groups 1, 2, 3)
comments <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group1.csv") %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group2.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group3.csv"))


############################################################
# define our stopwords and make unigrams from clean_text
words_to_ignore <- stop_words$word
unigrams <- comments %>%
  unnest_tokens(token, clean_text) %>%
  filter(!(token %in% words_to_ignore))


############################################################
# define vocabulary that appears in at least 2 documents
# we have 10,161 total tokens
vocab <- unigrams %>%
  group_by(token) %>%
  summarise(B_total_occurrences=length(unique(msg_id))) %>%
  arrange(desc(B_total_occurrences)) %>%
  filter(B_total_occurrences >= 2) %>%
  rename(target_token=token)


############################################################
# define all pairs of our 10,161 vocab words 
vocab.pair <- unigrams %>%
  # only consider tokens in vocab
  filter(token %in% vocab$target_token) %>%
  # find A the occurrences of both tokens
  pairwise_count(token, msg_id, diag=FALSE) %>% # include diagonal (always 100%)
  rename(target_token=item1,
         context_token=item2,
         A_pair_occurrences=n)


############################################################
# define prob of each pair as A/B
vocab.prob <- vocab.pair %>%
  left_join(vocab, by="target_token")  %>%
  mutate(prob=A_pair_occurrences/B_total_occurrences) %>%
  select(target_token, context_token, prob) %>%
  mutate(prob=ifelse(is.na(prob),0,prob)) %>%
  mutate_if(is.numeric,coalesce,0)

# visualize matrix as df to illustrate what is going on here
viz <- vocab.prob %>%
  spread(context_token, prob, fill=0)
sample_vector <- rotate_df(viz %>% filter(target_token=="skywalker"))
colnames(sample_vector) <- c("skywalker")


############################################################
# cast to sparse matrix
tic()
vocab.matrix <- vocab.prob %>%
  cast_sparse(target_token, context_token, prob)
toc()


###################################################################
# dimensionality reduction - 300 dimensions takes ~2 min
tic()
pmi.svd <- irlba(vocab.matrix, 300, maxit = 1e3, fastpath=FALSE)
custom.word.vectors300 <- pmi.svd$u
rownames(custom.word.vectors300) <- rownames(vocab.matrix)
toc()

# save our word vector
saveRDS(custom.word.vectors300, "custom.word.vectors300.Rds")


###################################################################
# dimensionality reduction - 768 dimensions takes ~21 min
tic()
pmi.svd <- irlba(vocab.matrix, 768, maxit = 1e3, fastpath=FALSE)
custom.word.vectors768 <- pmi.svd$u
rownames(custom.word.vectors768) <- rownames(vocab.matrix)
toc()

# save our word vector
saveRDS(custom.word.vectors768, "custom.word.vectors768.Rds")



#####################################################################################################
# cosine similarity functions
cosine_sim <- function(a, b) crossprod(a,b)/sqrt(crossprod(a)*crossprod(b))
find_similar_tokens <- function(target_vector, matrix) {
  cos_sims <- apply(matrix, 1,
                    FUN = function(y) 
                      cosine_sim(target_vector, y))
  out <- as.data.frame(cos_sims, row.names = rownames(cos_sims), stringsAsFactors=FALSE) %>%
    tibble::rownames_to_column("context_token") %>%
    select(context_token, similarity := cos_sims) %>%
    arrange(-similarity) %>%
    mutate(rank = row_number())
}

check1 <- find_similar_tokens(custom.word.vectors300["luke",],custom.word.vectors300) 
check2 <- find_similar_tokens(custom.word.vectors300["skywalker",],custom.word.vectors300) 
check3 <- find_similar_tokens(custom.word.vectors300["gandalf",],custom.word.vectors300) 
#check2 <- find_similar_tokens(custom.word.vectors768["luke",],custom.word.vectors768) %>% mutate(row_id = row_number())

# visualize for the article
viz <- data.frame(skywalker=custom.word.vectors300["skywalker",])

viz_example <- data.frame(love=custom.word.vectors300["love",],
                     luke=custom.word.vectors300["luke",],
                     skywalker=custom.word.vectors300["skywalker",],
                     hate=custom.word.vectors300["hate",],
                     gandalf=custom.word.vectors300["gandalf",],
                     grey=custom.word.vectors300["grey",])
rownames(viz_example) <- paste0("dim",seq(1,300,1))

#####################################################################################################
# load up all of our comments so we can encode everything
all_comments <- comments %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group4.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group5.csv"))

# get all unigrams for all comments
# only consider tokens that appear in our vocab (10,161 words)
all_unigrams <- all_comments %>%
  unnest_tokens(token, clean_text) %>%
  filter(token %in% vocab$target_token)



#------------------------------------------------------------------------------
# function to cycle through all comments and encode them 
encode_comment_with_custom_word_embeddings <- function(comments, unigrams, word_emdedding) {
  tic()
  
  # hold output in a new data frame
  encoded_comments <- data.frame()
  
  # how many dimensions does our embedding have?
  dimensions = ncol(word_emdedding) 
  
  # cycle through comments
  for(i in 1:nrow(comments)) {
    print(i)
    
    # details for this comment
    this_comment <- comments[i,]
    
    # get all tokens associated to this comment
    my_comment_tokens <- unigrams %>% filter(msg_id==this_comment$msg_id)    
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # special rule if we do not have tokens for this comment
    # encode with a vector of all 0's
    if(nrow(my_comment_tokens)==0) {
      
      # add vector of all 0's since we have no other information
      my_sentence_embedding <- rotate_df(data.frame(idx=paste0("dim",seq(1,dimensions,1)), value=0) %>% select(-idx)) 
      rownames(my_sentence_embedding) <- c(this_comment$msg_id)
      colnames(my_sentence_embedding) <- paste0("dim",seq(1,dimensions,1))
      
      # make wide and add other needed fields
      my_encoded_comment <- data.frame(msg_id=this_comment$msg_id, 
                                       token_count=this_comment$token_count,
                                       my_group=this_comment$my_group,
                                       my_role=this_comment$my_role,
                                       label=this_comment$label)
      rownames(my_encoded_comment) <- c(this_comment$msg_id)
      my_encoded_comment <- my_encoded_comment %>% cbind(my_sentence_embedding)
      encoded_comments <- encoded_comments %>% rbind(my_encoded_comment)
    }    
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # if we DO have tokens for this comment...
    if(nrow(my_comment_tokens)>0) {    
      
      # add the vectors for each token as a new column
      my_comment_vectors <- data.frame(idx=paste0("dim",seq(1,dimensions,1)))
      for(j in 1:nrow(my_comment_tokens)) {
        my_token <- my_comment_tokens[j,"token"]
        my_vector <- word_emdedding[my_token,]
        col_name <- paste0(my_token,j)
        my_comment_vectors[[col_name]] <- my_vector
      }
      
      # add the rownames to debug more easily
      rownames(my_comment_vectors) <- paste0("dim",seq(1,dimensions,1))
      
      # create a sentence embedding by taking the mean of all 768 dimensions
      my_sentence_embedding <- rotate_df(data.frame(val=rowMeans(my_comment_vectors %>% select(-idx), na.rm=TRUE)))
      rownames(my_sentence_embedding) <- c(this_comment$msg_id)
      
      # add other needed fields to our encoded comment
      my_encoded_comment <- data.frame(msg_id=this_comment$msg_id, 
                                       token_count=this_comment$token_count,
                                       my_group=this_comment$my_group,
                                       my_role=this_comment$my_role,
                                       label=this_comment$label)
      rownames(my_encoded_comment) <- c(this_comment$msg_id)
      my_encoded_comment <- my_encoded_comment %>% cbind(my_sentence_embedding)
      encoded_comments <- encoded_comments %>% rbind(my_encoded_comment)
    }      
    #break  
  }    
  toc()  
  return(encoded_comments)
}

# encode our comments 
#encoded300 <- encode_comment_with_custom_word_embeddings(all_comments, all_unigrams, custom.word.vectors300) # 16 minutes
encoded768 <- encode_comment_with_custom_word_embeddings(all_comments, all_unigrams, custom.word.vectors768) # 63 minutes


# save as 5 different CSV files so they fit on github
for(i in seq(1,5,1)) {
  print(i)
  tmp.data <- encoded300 %>%
    filter(my_group==i) 
  filename <- paste0('custom_encoded300_group',i,'.csv')
  write.csv(tmp.data, filename, row.names = FALSE)
}


# save as 10 different CSV files so they fit on github
for(i in seq(1,5,1)) {
  print(i)
  
  # save first 1000 in A
  tmp.data <- encoded768 %>% filter(my_group==i) 
  tmp.dataA <- tmp.data[1:1000,]
  filenameA <- paste0('custom_encoded768_group',i,'A.csv')  
  write.csv(tmp.dataA, filenameA, row.names = FALSE)
  
  # save second 1000 in B  
  tmp.dataB <- tmp.data[1001:nrow(tmp.data),]
  filenameB <- paste0('custom_encoded768_group',i,'B.csv')    
  write.csv(tmp.dataB, filenameB, row.names = FALSE)
}
