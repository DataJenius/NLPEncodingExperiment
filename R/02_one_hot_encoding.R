###########################################################
# this script takes our selected comments from Reddit
# and turns them into one-hot encoded data
set.seed(42)

############################################
# load dependencies
library(tidyverse)
library(tidytext)

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


