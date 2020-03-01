# Library
library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(reshape2)
library(wordcloud)
library(ggplot2)
library(caret)
library(quanteda)
library(RColorBrewer)

#loading the responses collected from text to speach
path_responses <- "/Users/shresthsethi/Desktop/MSBA Hult/MOD B/Text Analytics /Responses.docx"
responses <- read_document(file=path_responses)
no_quesn <- 5 #how many observations we have/ number of questions
client <- 28 #how many variables do you have/ no of clients who responded
my_df <- as.data.frame(matrix(nrow=no_quesn, ncol=client))

#for loop to transfer the data into a table format
for(z in 1:client){
  for(i in 1:no_quesn){
    my_df[i,z]<- responses[i-no_quesn+z*no_quesn]
  }#closing z loop
}#closing i loop

#for client 1
responses=5
my_txt <- my_df$V1
mydf <- data_frame(line=1:responses, text=my_txt)
print(mydf) # to check the loaded dataframe

#Tockenizing and removing stop words
data(stop_words)
frequencies_tokens_nostop <- mydf %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client2 tokenizing and removing stop words
my_txt_2 <- my_df$V2
mydf_2 <- data_frame(line=1:responses, text=my_txt_2)
print(mydf_2)

data(stop_words)
frequencies_tokens_nostop_2 <- mydf_2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client 3 tokenizing and removing stop words
my_txt_3 <- my_df$V3
mydf_3 <- data_frame(line=1:responses, text=my_txt_3)
print(mydf_3)

data(stop_words)
frequencies_tokens_nostop_3 <- mydf_3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client4 tokenizing and removing stop words
my_txt_4 <- my_df$V4
mydf_4 <- data_frame(line=1:responses, text=my_txt_4)
print(mydf_4)

data(stop_words)
frequencies_tokens_nostop_4 <- mydf_4 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client5 tokenizing and removing stop words
my_txt_5 <- my_df$V5
mydf_5 <- data_frame(line=1:responses, text=my_txt_5)
print(mydf_5)

data(stop_words)
frequencies_tokens_nostop_5 <- mydf_5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client6 tokenizing and removing stop words
my_txt_6 <- my_df$V6
mydf_6 <- data_frame(line=1:responses, text=my_txt_6)
print(mydf_6)

data(stop_words)
frequencies_tokens_nostop_6 <- mydf_6 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client7 tokenizing and removing stop words
my_txt_7 <- my_df$V7
mydf_7 <- data_frame(line=1:responses, text=my_txt_7)
print(mydf_7)

data(stop_words)
frequencies_tokens_nostop_7 <- mydf_7 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client8 tokenizing and removing stop words
my_txt_8 <- my_df$V8
mydf_8 <- data_frame(line=1:responses, text=my_txt_8)
print(mydf_8)

data(stop_words)
frequencies_tokens_nostop_8 <- mydf_8 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client9 tokenizing and removing stop words
my_txt_9 <- my_df$V9
mydf_9 <- data_frame(line=1:responses, text=my_txt_9)
print(mydf_9)

data(stop_words)
frequencies_tokens_nostop_9 <- mydf_9 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client10 tokenizing and removing stop words
my_txt_10 <- my_df$V10
mydf_10 <- data_frame(line=1:responses, text=my_txt_10)
print(mydf_10)

data(stop_words)
frequencies_tokens_nostop_10 <- mydf_10 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client11 tokenizing and removing stop words
my_txt_11 <- my_df$V11
mydf_11 <- data_frame(line=1:responses, text=my_txt_11)
print(mydf_11)

data(stop_words)
frequencies_tokens_nostop_11 <- mydf_11 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client12 tokenizing and removing stop words
my_txt_12 <- my_df$V12
mydf_12 <- data_frame(line=1:responses, text=my_txt_12)
print(mydf_12)

data(stop_words)
frequencies_tokens_nostop_12 <- mydf_12 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client13 tokenizing and removing stop words
my_txt_13 <- my_df$V13
mydf_13 <- data_frame(line=1:responses, text=my_txt_13)
print(mydf_13)

data(stop_words)
frequencies_tokens_nostop_13 <- mydf_13 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client14 tokenizing and removing stop words
my_txt_14 <- my_df$V14
mydf_14 <- data_frame(line=1:responses, text=my_txt_14)
print(mydf_14)

data(stop_words)
frequencies_tokens_nostop_14 <- mydf_14 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client15 tokenizing and removing stop words
my_txt_15 <- my_df$V15
mydf_15 <- data_frame(line=1:responses, text=my_txt_15)
print(mydf_15)

data(stop_words)
frequencies_tokens_nostop_15 <- mydf_15 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client 16 tokenizing and removing stop words
my_txt_16 <- my_df$V16
mydf_16 <- data_frame(line=1:responses, text=my_txt_16)
print(mydf_16)

data(stop_words)
frequencies_tokens_nostop_16 <- mydf_16 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client17 tokenizing and removing stop words
my_txt_17 <- my_df$V17
mydf_17 <- data_frame(line=1:responses, text=my_txt_17)
print(mydf_17)

data(stop_words)
frequencies_tokens_nostop_17 <- mydf_17 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client18 tokenizing and removing stop words
my_txt_18 <- my_df$V18
mydf_18 <- data_frame(line=1:responses, text=my_txt_18)
print(mydf_18)

data(stop_words)
frequencies_tokens_nostop_18 <- mydf_18 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client19 tokenizing and removing stop words
my_txt_19 <- my_df$V19
mydf_19 <- data_frame(line=1:responses, text=my_txt_19)
print(mydf_19)

data(stop_words)
frequencies_tokens_nostop_19 <- mydf_19 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client20 tokenizing and removing stop words
my_txt_20 <- my_df$V20
mydf_20 <- data_frame(line=1:responses, text=my_txt_20)
print(mydf_20)

data(stop_words)
frequencies_tokens_nostop_20 <- mydf_20 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client21 tokenizing and removing stop words
my_txt_21 <- my_df$V21
mydf_21 <- data_frame(line=1:responses, text=my_txt_21)
print(mydf_21)

data(stop_words)
frequencies_tokens_nostop_21 <- mydf_21 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client22 tokenizing and removing stop words
my_txt_22 <- my_df$V22
mydf_22 <- data_frame(line=1:responses, text=my_txt_22)
print(mydf_22)

data(stop_words)
frequencies_tokens_nostop_22 <- mydf_22 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client23 tokenizing and removing stop words
my_txt_23 <- my_df$V23
mydf_23 <- data_frame(line=1:responses, text=my_txt_23)
print(mydf_23)

data(stop_words)
frequencies_tokens_nostop_23 <- mydf_23 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client24 tokenizing and removing stop words
my_txt_24 <- my_df$V24
mydf_24 <- data_frame(line=1:responses, text=my_txt_24)
print(mydf_24)

data(stop_words)
frequencies_tokens_nostop_24 <- mydf_24 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client25 tokenizing and removing stop words
my_txt_25 <- my_df$V25
mydf_25 <- data_frame(line=1:responses, text=my_txt_25)
print(mydf_25)

data(stop_words)
frequencies_tokens_nostop_25 <- mydf_25 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client 26 tokenizing and removing stop words
my_txt_26 <- my_df$V26
mydf_26 <- data_frame(line=1:responses, text=my_txt_26)
print(mydf_26)

data(stop_words)
frequencies_tokens_nostop_26 <- mydf_26 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client 27 tokenizing and removing stop words
my_txt_27 <- my_df$V27
mydf_27 <- data_frame(line=1:responses, text=my_txt_27)
print(mydf_27)

data(stop_words)
frequencies_tokens_nostop_27 <- mydf_27 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#for client 28 tokenizing and removing stop words
my_txt_28 <- my_df$V28
mydf_28 <- data_frame(line=1:responses, text=my_txt_28)
print(mydf_28)

data(stop_words)
frequencies_tokens_nostop_28 <- mydf_28 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  dplyr::count(word, sort=TRUE)

#inserting client number to columns
client <- rep("client 1", nrow(frequencies_tokens_nostop))
frequencies_tokens_nostop$client <- client
client <- rep("client 2", nrow(frequencies_tokens_nostop_2))
frequencies_tokens_nostop_2$client <- client
client <- rep("client 3", nrow(frequencies_tokens_nostop_3))
frequencies_tokens_nostop_3$client <- client
client <- rep("client 4", nrow(frequencies_tokens_nostop_4))
frequencies_tokens_nostop_4$client <- client
client <- rep("client 5", nrow(frequencies_tokens_nostop_5))
frequencies_tokens_nostop_5$client <- client
client <- rep("client 6", nrow(frequencies_tokens_nostop_6))
frequencies_tokens_nostop_6$client <- client
client <- rep("client 7", nrow(frequencies_tokens_nostop_7))
frequencies_tokens_nostop_7$client <- client
client <- rep("client 8", nrow(frequencies_tokens_nostop_8))
frequencies_tokens_nostop_8$client <- client
client <- rep("client 9", nrow(frequencies_tokens_nostop_9))
frequencies_tokens_nostop_9$client <- client
client <- rep("client 10", nrow(frequencies_tokens_nostop_10))
frequencies_tokens_nostop_10$client <- client
client <- rep("client 11", nrow(frequencies_tokens_nostop_11))
frequencies_tokens_nostop_11$client <- client
client <- rep("client 12", nrow(frequencies_tokens_nostop_12))
frequencies_tokens_nostop_12$client <- client
client <- rep("client 13", nrow(frequencies_tokens_nostop_13))
frequencies_tokens_nostop_13$client <- client
client <- rep("client 14", nrow(frequencies_tokens_nostop_14))
frequencies_tokens_nostop_14$client <- client
client <- rep("client 15", nrow(frequencies_tokens_nostop_15))
frequencies_tokens_nostop_15$client <- client
client <- rep("client 16", nrow(frequencies_tokens_nostop_16))
frequencies_tokens_nostop_16$client <- client
client <- rep("client 17", nrow(frequencies_tokens_nostop_17))
frequencies_tokens_nostop_17$client <- client
client <- rep("client 18", nrow(frequencies_tokens_nostop_18))
frequencies_tokens_nostop_18$client <- client
client <- rep("client 19", nrow(frequencies_tokens_nostop_19))
frequencies_tokens_nostop_19$client <- client
client <- rep("client 20", nrow(frequencies_tokens_nostop_20))
frequencies_tokens_nostop_20$client <- client
client <- rep("client 21", nrow(frequencies_tokens_nostop_21))
frequencies_tokens_nostop_21$client <- client
client <- rep("client 22", nrow(frequencies_tokens_nostop_22))
frequencies_tokens_nostop_22$client <- client
client <- rep("client 23", nrow(frequencies_tokens_nostop_23))
frequencies_tokens_nostop_23$client <- client
client <- rep("client 24", nrow(frequencies_tokens_nostop_24))
frequencies_tokens_nostop_24$client <- client
client <- rep("client 25", nrow(frequencies_tokens_nostop_25))
frequencies_tokens_nostop_25$client <- client
client <- rep("client 26", nrow(frequencies_tokens_nostop_26))
frequencies_tokens_nostop_26$client <- client
client <- rep("client 27", nrow(frequencies_tokens_nostop_27))
frequencies_tokens_nostop_27$client <- client
client <- rep("client 28", nrow(frequencies_tokens_nostop_28))
frequencies_tokens_nostop_28$client <- client

#binding all to cast dfm
binding_fun <- rbind(frequencies_tokens_nostop, 
                       frequencies_tokens_nostop_2, 
                       frequencies_tokens_nostop_3, 
                       frequencies_tokens_nostop_4,
                       frequencies_tokens_nostop_5,
                       frequencies_tokens_nostop_6,
                       frequencies_tokens_nostop_7,
                       frequencies_tokens_nostop_8,
                       frequencies_tokens_nostop_9,
                       frequencies_tokens_nostop_10,
                       frequencies_tokens_nostop_11,
                       frequencies_tokens_nostop_12,
                       frequencies_tokens_nostop_13,
                       frequencies_tokens_nostop_14,
                       frequencies_tokens_nostop_15,
                       frequencies_tokens_nostop_16,
                       frequencies_tokens_nostop_17,
                       frequencies_tokens_nostop_18,
                       frequencies_tokens_nostop_19,
                       frequencies_tokens_nostop_20,
                       frequencies_tokens_nostop_21,
                       frequencies_tokens_nostop_22,
                       frequencies_tokens_nostop_23,
                       frequencies_tokens_nostop_24,
                       frequencies_tokens_nostop_25,
                       frequencies_tokens_nostop_26,
                       frequencies_tokens_nostop_27,
                       frequencies_tokens_nostop_28)
#creating corpus
speach_corpus <- corpus(binding_fun$word) 
msg.dfm <- dfm(speach_corpus, tolower = TRUE)
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 1, min_docfreq = 0)
msg.dfm <- dfm_weight(msg.dfm)

head(msg.dfm) #displaying Document feature matrix: result 99.7% sparse

#spliting data into train_test_split for machine learning model
msg.dfm.train<-msg.dfm[1:22,]
msg.dfm.test<-msg.dfm[22:28,]

#using navie bayes classifier model
NB_classifier <- textmodel_nb(msg.dfm.train, c(0,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,1,1,0,1))
NB_classifier

#OUTPUT:
#Call:
#textmodel_nb.dfm(x = msg.dfm.train, y = c(0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1))
#Distribution: multinomial; prior: uniform; smoothing value: 1; 22 training documents; 374 fitted features.

summary(NB_classifier)
#OUTPUT
# Call:
#   textmodel_nb.dfm(x = msg.dfm.train, y = c(0, 1, 1, 1, 1, 1, 1, 
#                                             0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1))
# 
# Class Priors:
#   (showing first 2 elements)
# 0   1 
# 0.5 0.5 
# 
# Estimated Feature Scores:
#   i'm      4    bar    buy    car cashback change  gonna groceries grocery lifeguard  moles
# 0 0.6747 0.3415 0.3415 0.3415 0.3415   0.3415 0.3415 0.6747    0.3415  0.3415    0.3415 0.3415
# 1 0.3253 0.6585 0.6585 0.6585 0.6585   0.6585 0.6585 0.3253    0.6585  0.6585    0.6585 0.6585
#   shopping  stuff      2   cool percentage   rent rental twitch there's investment accounts
# 0   0.6747 0.3415 0.3415 0.3415     0.3415 0.3415 0.3415 0.3415  0.6747     0.3415   0.5091
# 1   0.3253 0.6585 0.6585 0.6585     0.6585 0.6585 0.6585 0.6585  0.3253     0.6585   0.4909
# additional   bank basically education entertainment expense feature
# 0     0.5091 0.5091    0.5091    0.5091        0.5091  0.5091  0.5091
# 1     0.4909 0.4909    0.4909    0.4909        0.4909  0.4909  0.4909

#Developing prediction model using NB_classifier and testing it on test data
pred <- predict(NB_classifier, msg.dfm.test)
pred
#OUTPUT
# text22 text23 text24 text25 text26 text27 text28 
# 1      1      0      0      0      0      0 
# Levels: 0 1

#install.packages("caret")
#creating confusion matrix to check model performance
truth <- factor(c(0,1,1,1,1,1,0))
cm <- confusionMatrix(pred, truth)
cm$table
#OUTPUT
# Reference
# Prediction 0 1
#          0 1 4
#          1 1 1

#diplay the confusuin matrix for the table
fourfoldplot(cm$table)

