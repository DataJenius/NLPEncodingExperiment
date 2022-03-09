###########################################################
# this script takes our results and analyzes them
set.seed(42)

############################################
# load dependencies
library(tidyverse)


############################################################
############################################################
# visualize our results
############################################################
# get summarized results of each method
get_results <- function(name, method, inputs, params, epochs, link_to_csv) {
  tmp <- read.csv(link_to_csv)
  res <- data.frame(name=name,
                    method=method, 
                    inputs=inputs,
                    params=params,
                    epochs=epochs,
                    accuracy=mean(tmp$correct),
                    prec = sum(tmp$tp)/(sum(tmp$tp)+sum(tmp$fp)),
                    recall = sum(tmp$tp)/(sum(tmp$tp)+sum(tmp$fn)))
  return(res)
}



############################################################
# get all our results for summary plots...
results <- get_results("one-hot (300) 100", "one-hot", 300, 301, 100, "https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_one-hot-(300).csv")
results <- rbind(results, get_results("one-hot (768) 100", "one-hot", 768, 769, 100, "https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_one-hot-(768).csv"))
results <- rbind(results, get_results("word2vec (300) 100", "word2vec", 300, 301, 100, "https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_word2vec-(300).csv"))
results <- rbind(results, get_results("word2vec (300) 1,000", "word2vec", 300, 301, 1000, "https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_word2vec-(300)_1000_epochs.csv"))
results <- rbind(results, get_results("custom (300) 100", "custom", 300, 301, 100, "https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_custom-(300).csv"))
results <- rbind(results, get_results("custom (300) 1,000", "custom", 300, 301, 1000, "https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_custom-(300)_1000_epochs.csv"))
results <- rbind(results, get_results("custom (768) 100", "custom", 768, 769, 100, "https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_custom-(768).csv"))
results <- rbind(results, get_results("custom (768) 1,000", "custom",  768, 769, 1000, "https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_custom-(768)_1000_epochs.csv"))
results <- rbind(results, get_results("BERT (768) 1", "BERT",  768, 109483778, 1000, "https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_BERT-(768)_1_epoch.csv"))
#results$inputs <- factor(results$inputs, levels=c("300","768"))
#results$epochs <- factor(results$epochs, levels=c("100","1000"))

############################################################
# time to plot
library(ggplot2)
library(scales)
library(gridExtra)
my_limits <- c(0.80, 1)
# my_colors <- c("one-hot (300) 100"="#ffb845",
#                "one-hot (768) 100"="#ff9e00",
#                "word2vec (300) 100"="#eb344c",
#                "word2vec (300) 1,000"="#ad2638",
#                "custom (300) 100"="#7a95ff",
#                "custom (300) 1,000"="#5264ab",
#                "custom (768) 100"="#1c49ff",
#                "custom (768) 1,000"="#0a29a6",
#                "BERT (768) 1"="#b31cd9")

my_colors <- c("one-hot"="#BF7D2C",
               "word2vec"="#D6A07E",
               "custom"="#7C94BA",
               "BERT"="#8371A2")

show_items <- c(
                "one-hot (300) 100",
                "one-hot (768) 100",
                "word2vec (300) 100",
                "word2vec (300) 1,000",
                "custom (300) 100",
                "custom (300) 1,000",
                "custom (768) 100",
                "custom (768) 1,000"
                #"BERT (768) 1"
                )
ggdata <- results %>% filter(name %in% show_items)

# accuracy
#my_limits <- c(0.85, max(ggdata$accuracy)+0.00)
plot1 <- ggplot(ggdata, aes(y=reorder(name,accuracy), x=accuracy, fill=method, label=percent(accuracy))) +
  geom_bar(stat="identity", color="black") +
  geom_text(nudge_x=-0.04) +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Accuracy") +
  labs(x=" ", y=" ") +
  scale_x_continuous(labels=percent, limits=my_limits, oob = rescale_none) +
  scale_fill_manual(name=" ",
                    values = my_colors)
#plot1

# prec
#my_limits <- c(0.85, max(ggdata$prec)+0.00)
plot2 <- ggplot(ggdata %>% mutate(prec=round(prec,4)), aes(y=reorder(name,prec), x=prec, fill=method, label=percent(prec))) +
  geom_bar(stat="identity", color="black") +
  geom_text(nudge_x=-0.04) +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Precision") +
  labs(x=" ", y=" ") +
  scale_x_continuous(labels=percent, limits=my_limits, oob = rescale_none) +
  scale_fill_manual(name=" ",
                    values = my_colors)
#plot2

# recall
#my_limits <- c(0.85, max(ggdata$recall)+0.00)
plot3 <- ggplot(ggdata %>% mutate(recall=round(recall,4)), aes(y=reorder(name,recall), x=recall, fill=method, label=percent(recall))) +
  geom_bar(stat="identity", color="black") +
  geom_text(nudge_x=-0.04) +
  theme_bw() +
  theme(legend.position="none") +
  ggtitle("Recall") +
  labs(x=" ", y=" ") +
  scale_x_continuous(labels=percent, limits=my_limits, oob = rescale_none) +
  scale_fill_manual(name=" ",
                    values = my_colors)
#plot3

# plot all 3 together
grid.arrange(plot1, plot2, plot3, nrow = 1)





############################################################
# compare which model got which comment correct
results.compare <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_one-hot-(300).csv") %>% mutate(onehot300_correct=correct, p1=p_sigmoid) %>% select(msg_id, onehot300_correct, p1) %>% 
  left_join(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_one-hot-(768).csv") %>% mutate(onehot768_correct=correct, p2=p_sigmoid) %>% select(msg_id, onehot768_correct, p2), by="msg_id") %>%
  left_join(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_word2vec-(300).csv") %>% mutate(word2vec300_correct=correct, p3=p_sigmoid) %>% select(msg_id, word2vec300_correct, p3), by="msg_id") %>%
  left_join(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_word2vec-(300)_1000_epochs.csv") %>% mutate(word2vec300b_correct=correct, p4=p_sigmoid) %>% select(msg_id, word2vec300b_correct, p4), by="msg_id") %>%
  left_join(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_custom-(300).csv") %>% mutate(custom300_correct=correct, p5=p_sigmoid) %>% select(msg_id, custom300_correct, p5), by="msg_id") %>%
  left_join(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_custom-(300)_1000_epochs.csv") %>% mutate(custom300b_correct=correct, p6=p_sigmoid) %>% select(msg_id, custom300b_correct, p6), by="msg_id") %>%
  left_join(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_custom-(768).csv") %>% mutate(custom768_correct=correct, p7=p_sigmoid) %>% select(msg_id, custom768_correct, p7), by="msg_id") %>%
  left_join(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_custom-(768)_1000_epochs.csv") %>% mutate(custom768b_correct=correct, p8=p_sigmoid) %>% select(msg_id, custom768b_correct, p8), by="msg_id") %>%
  left_join(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_BERT-(768)_1_epoch.csv") %>% mutate(bert_correct=correct, p9=p_sigmoid) %>% select(msg_id, bert_correct, p9), by="msg_id") %>%
  mutate(total_correct = onehot300_correct+onehot768_correct+word2vec300_correct+word2vec300b_correct+custom300_correct+custom300b_correct+custom768_correct+custom768b_correct+bert_correct) %>%
  mutate(avg_p = (p1+p2+p3+p4+p5+p6+p7+p8+p9)/9)

  
# look at the comments
comments <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/comments/selected/selected_reddit_comments_group5.csv")

# examples everyone got right
everyone_right <- results.compare %>% filter(total_correct==9) %>% left_join(comments, by="msg_id")
everyone_wrong <- results.compare %>% filter(total_correct==0) %>% left_join(comments, by="msg_id")

sanity_check <- results.compare %>% filter(msg_id==17480) %>% left_join(comments, by="msg_id")

# example that confused it
confused <- results.compare %>% filter(avg_p > 0.3) %>% filter(avg_p < 0.7) %>% left_join(comments, by="msg_id")
check <- confused %>% filter(msg_id==22306)

############################################################
# visualize how each model scores a given comment
results.sigmoids <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_one-hot-(300).csv") %>% mutate(method='one-hot (300) 100') %>% select(msg_id, method, p_sigmoid, correct) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_one-hot-(768).csv") %>% mutate(method='one-hot (768) 100') %>% select(msg_id, method, p_sigmoid, correct)) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_word2vec-(300).csv") %>% mutate(method='word2vec (300) 100') %>% select(msg_id, method, p_sigmoid, correct)) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_word2vec-(300)_1000_epochs.csv") %>% mutate(method='word2vec (300) 1,000') %>% select(msg_id, method, p_sigmoid, correct)) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_custom-(300).csv") %>% mutate(method='custom (300) 100') %>% select(msg_id, method, p_sigmoid, correct)) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_custom-(300)_1000_epochs.csv") %>% mutate(method='custom (300) 1,000') %>% select(msg_id, method, p_sigmoid, correct)) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_custom-(768).csv") %>% mutate(method='custom (768) 100') %>% select(msg_id, method, p_sigmoid, correct)) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_custom-(768)_1000_epochs.csv") %>% mutate(method='custom (768) 1,000') %>% select(msg_id, method, p_sigmoid, correct)) %>%
  rbind(read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/output/results_BERT-(768)_1_epoch.csv") %>% mutate(method='BERT (768) 1') %>% select(msg_id, method, p_sigmoid, correct)) %>%
  mutate(my_color=ifelse(correct==1,"green","red"))

results.sigmoids$method <- factor(results.sigmoids$method, c(
  "one-hot (300) 100",
  "one-hot (768) 100",
  "word2vec (300) 100",
  "word2vec (300) 1,000",
  "custom (300) 100",
  "custom (300) 1,000",
  "custom (768) 100",
  "custom (768) 1,000",
  "BERT (768) 1"
))
  
# some examples to look at
my_example <- results.sigmoids %>% filter(msg_id==175) # everyone was right - StarWars...
my_example <- results.sigmoids %>% filter(msg_id==27417) # everyone was right - LOTR
my_example <- results.sigmoids %>% filter(msg_id==734) # confusion - SW

my_example <- results.sigmoids %>% filter(msg_id==22306) # Trekkie

my_example <- results.sigmoids %>% filter(msg_id==46701) # Everyone wrong - SW in LOTR

my_example <- results.sigmoids %>% filter(msg_id==17480) # SW from start of article




# plot how our model responds to this comment
ggplot(my_example, aes(x=method, y=p_sigmoid, fill=my_color, color=my_color, shape=my_color)) +
  geom_bar(stat="identity", color="black") +
  geom_point(y=0.5, size=3) +
  scale_y_continuous(labels=percent, limits=c(0,1)) +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty=2, color="#999999") +
  ggtitle(paste0("msg_id: ",max(my_example$msg_id))) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x=" ", y="p(lotr)") +
  scale_fill_manual(name=" ",
                    values = c("red"="#fc8b9a",
                               "green"="#73c98b")) +
  scale_color_manual(name=" ",
                     values = c("red"="#eb344c",
                                "green"="#318f4b")) 



############################################################
# redo some of the table to be clearer in the article
sample_data <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/one_hot300/one_hot_encoded300_group1.csv")
sample_data$dots <- rep("...",2000)
sample_data <- sample_data %>% select(msg_id, token_count, my_group, my_role, label,
                                      absolutely, action, actors, dots, wrote, yeah, youre)
colnames(sample_data) <- c("msg_id", "token_count", "my_group", "my_role", "label",
                           "absolutely", "action", "actors", "...", "wrote","yeah","youre")


sample_data <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/one_hot768/one_hot_encoded768_group1.csv")
sample_data$dots <- rep("...",2000)
sample_data <- sample_data %>% select(msg_id, token_count, my_group, my_role, label,
                                      absolute, absolutely, act, dots, youll, youre, youve)
colnames(sample_data) <- c("msg_id", "token_count", "my_group", "my_role", "label",
                           "absolute", "absolutely", "act", "...", "youll","youre","youve")



sample_data <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/word2vec300/word2vec_encoded300_group1.csv")
sample_data$dots <- rep("...",2000)
sample_data <- sample_data %>% select(msg_id, token_count, my_group, my_role, label,
                                      dim.1, dim.2, dim.3, dots, dim.300)
colnames(sample_data) <- c("msg_id", "token_count", "my_group", "my_role", "label",
                           "dim1", "dim2", "dim3", "...", "dim300")


sample_data <- read.csv("https://raw.githubusercontent.com/DataJenius/NLPEncodingExperiment/main/data/input/custom300/custom_encoded300_group1.csv")
sample_data$dots <- rep("...",2000)
sample_data$dots1 <- rep("0",2000)
sample_data$dots2 <- rep("0",2000)
sample_data$dots3 <- rep("0",2000)
sample_data$dots4 <- rep("0",2000)
sample_data$dots6 <- rep("0",2000)
sample_data$dots5 <- rep("...",2000)
sample_data <- sample_data %>% select(dim6, dim7, dim8, dim9, dim10, dots, dim11, dots1, dots2, dots3, dots5,dots6)
colnames(sample_data) <- c("i", "love", "ganda","##l", "##f", "...", "skywalker","[UNK]","[PAD]","[PAD]","...","[PAD]")
jojo = sample_data[1:12,]
jojo[11,] <- rep("...",12)
rownames(jojo) <- c(paste0("dim",seq(1,10,1)), "...","dim300")


