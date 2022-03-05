###########################################################
# this script takes our raw comments from Reddit
# and turns them into 5 organized, labelled CSV files
# for use in the next steps
set.seed(42)

############################################
# load dependencies
library(tidyverse)
library(tidytext)
library(tictoc)


#######################################################
# load our raw comments

# load our raw StarWars comments- 22,346 total
# in this experiment StarWars is the negative class
raw_comments_sw <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/raw/raw_reddit_comments_StarWars.csv") %>%
  mutate(source="/r/StarWars", label=0)

# load our raw lotr comments- 31,274 total
# in this experiment lotr is the positive class
raw_comments_lotr <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/raw/raw_reddit_comments_lotr.csv") %>%
  mutate(source="/r/lotr", label=1)


##########################################################
# this function takes a raw comment df and cleans it up
clean_it_up <- function(df) {
  tic()
  
  # hold clean data in new df
  clean.data <- df %>%
    
    # consider comments with positive karma only  
    filter(karma > 0) %>%
    
    # text preprocessing- clean it up
    mutate(clean_text = gsub('http\\S+\\s*', "", raw_comment), # remove URLs
           clean_text = gsub("[[:punct:]]", "", clean_text), # remove all punctuation
           clean_text = gsub("[[:digit:]]", "", clean_text), # remove all numbers
           clean_text = gsub("&amp", "", clean_text), # special rule
           clean_text = gsub("amp", "", clean_text), # special rule
           clean_text = str_trim(clean_text), # trim needless whitespace
           clean_text = tolower(clean_text), # everything is lowercase
           raw_text=raw_comment) %>%
    
    # add row number as our index
    mutate(msg_id=row_number()) %>%
    select(msg_id, label, source, raw_text, clean_text)
  toc()
  return(clean.data)
}


#######################################################
# clean up our comments
clean_comments <- clean_it_up(rbind(raw_comments_sw, raw_comments_lotr)) # clean all at once so everyone gets a unique msg_id
clean_comments_sw <- clean_comments %>% filter(label==0)
clean_comments_lotr <- clean_comments %>% filter(label==1)


#######################################################
# tokenize the clean comments into unigrams
# ignore any stop words

# define our stopwords
words_to_ignore <- stop_words$word # use default stop words

# break sw into unigrams, ignore any stopwords
clean_comments_sw.unigrams <- clean_comments_sw %>%
  unnest_tokens(token, clean_text) %>%
  filter(!(token %in% words_to_ignore))

# break lotr into unigrams, ignore any stopwords
clean_comments_lotr.unigrams <- clean_comments_lotr %>%
  unnest_tokens(token, clean_text) %>%
  filter(!(token %in% words_to_ignore))


###################################################################
# count how many non-stop word tokens each comment has 

# count tokens per sw comment
clean_comments_sw.token_count <- clean_comments_sw.unigrams %>%
  group_by(msg_id) %>%
  summarise(token_count=n())
clean_comments_sw <- clean_comments_sw %>% left_join(clean_comments_sw.token_count, by="msg_id")

# count tokens per lotr comment
clean_comments_lotr.token_count <- clean_comments_lotr.unigrams %>%
  group_by(msg_id) %>%
  summarise(token_count=n())
clean_comments_lotr <- clean_comments_lotr %>% left_join(clean_comments_lotr.token_count, by="msg_id")


#######################################################################
# only consider long comments, those with 13+ non-stop word tokens
long_comments_sw <- clean_comments_sw %>% filter(token_count >= 13)
long_comments_lotr <- clean_comments_lotr %>% filter(token_count >= 13)


#######################################################################
# randomly select 5,000 comments from both groups w/o replacement
# this randomizes the order of comments in both groups
selected_comments_sw <- long_comments_sw %>% sample_n(5000, replace=FALSE)
selected_comments_lotr <- long_comments_lotr %>% sample_n(5000, replace=FALSE)


#######################################################################
# define 5 groups of equal size (1,000 each per source) as vector
my_groups <- c(rep(1,1000),rep(2,1000),rep(3,1000),rep(4,1000),rep(5,1000))
groups_comments_sw <- selected_comments_sw %>% mutate(my_group=my_groups)
groups_comments_lotr <- selected_comments_lotr %>% mutate(my_group=my_groups)

#######################################################################
# combine back into a single dataframe
# we will use groups 1,2,3 to test
# we will use group 4 to validate
# we will use group 5 to test
all_comments_final <- rbind(groups_comments_sw, groups_comments_lotr) %>%
  mutate(my_role="train") %>%
  mutate(my_role=ifelse(my_group==4,"validate",my_role)) %>%  
  mutate(my_role=ifelse(my_group==5,"test",my_role)) %>%
  select(msg_id, token_count, my_group, my_role, label, source, raw_text, clean_text)
           
# shuffle the comments again for good measure
all_comments_final <- all_comments_final[sample(1:nrow(all_comments_final)), ]


#######################################################################
# save as 5 different CSV files so they fit on github
for(i in seq(1,5,1)) {
  print(i)
  tmp.data <- all_comments_final %>%
    filter(my_group==i) 
  filename <- paste0('selected_reddit_comments_group',i,'.csv')
  write.csv(tmp.data, filename, row.names = FALSE)
}