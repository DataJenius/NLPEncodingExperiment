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

# visualize matrix as df
viz <- vocab.prob %>%
  spread(context_token, prob, fill=0)


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
    select(context_token, target_token := cos_sims) %>%
    arrange(-target_token)
}

check1 <- find_similar_tokens(custom.word.vectors300["luke",],custom.word.vectors300) %>% mutate(row_id = row_number())
check2 <- find_similar_tokens(custom.word.vectors768["luke",],custom.word.vectors768) %>% mutate(row_id = row_number())



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
  dimensions = ncol(custom.word.vectors300) 
  
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
encoded300 <- encode_comment_with_custom_word_embeddings(all_comments, all_unigrams, custom.word.vectors300) # 16 minutes
#encoded768 <- encode_comment_with_custom_word_embeddings(all_comments, all_unigrams, custom.word.vectors768)


# save as 5 different CSV files so they fit on github
for(i in seq(1,5,1)) {
  print(i)
  tmp.data <- encoded300 %>%
    filter(my_group==i) 
  filename <- paste0('custom_encoded300_group',i,'.csv')
  write.csv(tmp.data, filename, row.names = FALSE)
}





tic()
encoded_comments <- data.frame()
for(i in 1:nrow(final.comments)) {
  print(i)
  
  # get details of this comment
  my_msg_id <- final.comments[i,"msg_id"]
  my_val_group <- final.comments[i,"val_group"]
  my_label <- final.comments[i,"label"]
  
  # get all tokens associate to this comment
  my_comment_tokens <- comment_tokens %>% filter(msg_id==my_msg_id)
  
  # special rule if we do not have tokens for this comment
  if(nrow(my_comment_tokens)==0) {
    
    # add vector of all 0's since we have no other information
    my_sentence_embedding <- rotate_df(data.frame(idx=paste0("dim",seq(1,768,1)), value=0) %>% select(-idx)) 
    rownames(my_sentence_embedding) <- c(my_msg_id)
    colnames(my_sentence_embedding) <- paste0("dim",seq(1,768,1))
    
    # make wide and add other needed fields
    my_encoded_comment <- data.frame(msg_id=my_msg_id, val_group=my_val_group, label=my_label)
    rownames(my_encoded_comment) <- c(my_msg_id)
    my_encoded_comment <- my_encoded_comment %>% cbind(my_sentence_embedding)
    encoded_comments <- encoded_comments %>% rbind(my_encoded_comment)
  }
  
  # we have tokens for this comment
  if(nrow(my_comment_tokens)>0) {
    
    # add the vectors for each token as a new column
    my_comment_vectors <- data.frame(idx=paste0("dim",seq(1,768,1)))
    for(j in 1:nrow(my_comment_tokens)) {
      my_token <- my_comment_tokens[j,"token"]
      my_vector <- word.vectors[my_token,]
      col_name <- paste0(my_token,j)
      my_comment_vectors[[col_name]] <- my_vector
    }
    
    # add the rownames to debug more easily
    rownames(my_comment_vectors) <- paste0("dim",seq(1,768,1))
    
    # create a sentence embedding by taking the mean of all 768 dimensions
    my_sentence_embedding <- rotate_df(data.frame(val=rowMeans(my_comment_vectors %>% select(-idx), na.rm=TRUE)))
    rownames(my_sentence_embedding) <- c(my_msg_id)
    
    # add other needed fields to our encoded comment
    my_encoded_comment <- data.frame(msg_id=my_msg_id, val_group=my_val_group, label=my_label)
    rownames(my_encoded_comment) <- c(my_msg_id)
    my_encoded_comment <- my_encoded_comment %>% cbind(my_sentence_embedding)
    encoded_comments <- encoded_comments %>% rbind(my_encoded_comment)
  }   
}
toc()







baskets.df <- as.data.frame(t(vocab.matrix))




################################################################
# find most important tokens according to document frequency
tokens_by_df <- unigrams %>%
  group_by(token) %>%
  summarise(total_df=length(unique(msg_id))) %>%
  arrange(desc(total_df))


############################################################
# which tokens are popular in which group?
tokens_sw <- unigrams %>%
  filter(label==0) %>%
  group_by(token) %>%
  summarise(total_df=length(unique(msg_id))) %>%
  arrange(desc(total_df)) %>%
  rename(freq_sw=total_df)

tokens_lotr <- unigrams %>%
  filter(label==1) %>%
  group_by(token) %>%
  summarise(total_df=length(unique(msg_id))) %>%
  arrange(desc(total_df)) %>%
  rename(freq_lort=total_df)

combined <- tokens_lotr %>%
  left_join(tokens_sw, by="token") %>%
  mutate(freq_sw=ifelse(is.na(freq_sw),0,freq_sw)) %>%
  mutate(freq_lort=ifelse(is.na(freq_lort),0,freq_lort)) %>%
  mutate(total=freq_sw+freq_lort) %>%
  mutate(bias=freq_lort-freq_sw)


############################################################
# visualize a few cherry-picked examples
cherry_pick <- c("story","characters","trilogy",
                 "books","tolkien","frodo",
                 "luke","episode","mandalorian")
ggdata <- combined %>%
  filter(token %in% cherry_pick)

plot <- ggplot(ggdata, aes(x=freq_sw, y=freq_lort, color=bias, label=token)) +
  geom_point() +
  geom_text(size=4,nudge_y=30) +
  xlim(-50, 600) +
  ylim(-50, 600) +  
  geom_abline(slope=1, intercept = 0, color="#333333", lty=3) +
  scale_color_gradient2(
    low="#db4646",
    mid = "#333333",
    high="#4646db"    
  ) +
  ggtitle("Document Frequency") +
  labs(x="/r/StarWars", y="/r/lotr") +
  theme_bw() +
  theme(legend.position = "none")
plot


############################################################
# top 300 and 768 tokens 
top300_tokens <- tokens_by_df[1:300,"token"]
top768_tokens <- tokens_by_df[1:768,"token"]

############################################################
# load all of our comments, including val and test
all_comments <- comments %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group4.csv")) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group5.csv"))

############################################################
# tokenize all comments into unigrams
all_unigrams <- all_comments %>%
  unnest_tokens(token, clean_text) %>%
  filter(!(token %in% words_to_ignore))

############################################################
# get one-hot encoded data using top 300 tokens 
one_hot_encoded300 <- all_unigrams %>%
  filter(token %in% top300_tokens$token) %>%
  group_by(msg_id, token) %>%
  summarise(value=1) %>%
  select(msg_id, token, value) %>%
  spread(token, value, fill=0)

# combine encoding with other comment features
all_comments_one_hot_encoded300 <- all_comments %>%
  select(msg_id, token_count, my_group, my_role, label) %>%
  left_join(one_hot_encoded300, by="msg_id") %>%
  mutate_if(is.numeric,coalesce,0)

# save as 5 different CSV files so they fit on github
for(i in seq(1,5,1)) {
  print(i)
  tmp.data <- all_comments_one_hot_encoded300 %>%
    filter(my_group==i) 
  filename <- paste0('one_hot_encoded300_group',i,'.csv')
  write.csv(tmp.data, filename, row.names = FALSE)
}



############################################################
# get one-hot encoded data using top 768 tokens 
one_hot_encoded768 <- all_unigrams %>%
  filter(token %in% top768_tokens$token) %>%
  group_by(msg_id, token) %>%
  summarise(value=1) %>%
  select(msg_id, token, value) %>%
  spread(token, value, fill=0)

# combine encoding with other comment features
all_comments_one_hot_encoded768 <- all_comments %>%
  select(msg_id, token_count, my_group, my_role, label) %>%
  left_join(one_hot_encoded768, by="msg_id") %>%
  mutate_if(is.numeric,coalesce,0)

# save as 5 different CSV files so they fit on github
for(i in seq(1,5,1)) {
  print(i)
  tmp.data <- all_comments_one_hot_encoded768 %>%
    filter(my_group==i) 
  filename <- paste0('one_hot_encoded768_group',i,'.csv')
  write.csv(tmp.data, filename, row.names = FALSE)
}
