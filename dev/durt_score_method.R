#Nico Manzonelli
#Methodology behind Durt Score. 
 
#I assigned the value of the durt word between 1 and 2, based on how severe said word word is.

#1 is that this word is not often used in a profain or unprofessional context but can be.
#1.5 is that this word can be used profain or unprofesssional or profesional context equally.
#2.0 is that this word is always profain or unprofessional.

#Altered the dirtdict.csv file to reflect this ranking. 

#durt <- read.csv('durtdictionary2.csv', stringsAsFactors = FALSE)

#write.csv(durt, 'durtdict.csv', row.names = FALSE)
#Edited the csv file directly. 

durt <- read.csv('durtdict.csv')
kdurt <- read.csv('kylerstweets.csv', stringsAsFactors = FALSE)

library(qdapTools) # I ahve to use the the qdapTool to look up the values I assigned to the durt words.

durt_val <- kdurt %>%
  distinct() %>%
  unnest_tokens(word, text) %>%
  filter(word %in% durt$word) %>%
  mutate(durtvalue = word %l% durt) %>%
  right_join(distinct(kdurt)) %>%
  group_by(status_id) %>%
  mutate(durt_val_tot = sum(durtvalue)) %>%
  distinct(status_id, durt_val_tot) %>%
  right_join(distinct(kdurt))


#sort by durt val most durty
durt_val_sorted <- durt_val %>%
  arrange(-durt_val_tot)

#sort by durt val least durty
durt_val_sorted <- durt_val %>%
  arrange(durt_val_tot)

#oldest first
durt_val_sorted <- durt_val %>%
  arrange(created_at)

#compared durt value to avg sent value of 10,000 tweets.

library(rtweet)
library(tidyr)

#fbomb <-  search_tweets("fuck", n = 10000, retryonratelimit = TRUE)

fbomb_sent <- fbomb %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments('afinn')) %>%
  group_by(status_id) %>%
  summarize(sent_score = sum(value)) %>%
  right_join(fbomb) %>%
  select(user_id,text,sent_score) %>%
  drop_na()

fbomb_sent_avg <- fbomb_sent %>%
  mutate(sent_avg = sum(sent_score)/nrow(fbomb_sent))

affin <- get_sentiments('afinn')

dict <- durtdict

#use kylers tweets to check co-orlation between durt score and durt-val

library(data.table)
tweet_cor <- data.table(tweet_final)
tweet_cor[is.na(durt_val) == TRUE] <- 0

tweet_cor <- tweet_cor[is.na(sent_score)== FALSE]

cor(tweet_cor$durt_val,tweet_cor$sent_score)
#In this sample durt value is -0.70955 negativley corelated with sentiment. Therefore as durt score increases,
# tweet sentiment becomes more negative. As expected.
