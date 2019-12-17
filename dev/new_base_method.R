# Nico Manzonelli
# Dec 2019

#This is everything the app will do if the user is not in the database
#first hit my API and get all the tweets on the person.

#run the build_df_function
#do a quick sentiment analysis on all tweets pulled

tweet_sentiment <- tweets %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments('afinn')) %>%
  group_by(status_id) %>%
  summarize(sent_score = sum(value)) %>%
  right_join(tweets)

#do text analysis look at most used words
library(qdapTools)
#get all the durt
#do durt_val analysis of durt add score to df
tweet_durt <- tweet_sentiment %>%
  select(text, status_id) %>%
  unnest_tokens(word, text) %>%
  filter(word %in% durtdict$word) %>%
  mutate(durtvalue = word %l% durtdict) %>%
  right_join(distinct(tweet_sentiment)) %>%
  group_by(status_id) %>%
  mutate(durt_val = sum(durtvalue)) %>%
  distinct(status_id, durt_val) %>%
  right_join(distinct(tweet_sentiment))

#get the embed links forand add that to the df

tweet_final <- tweet_durt %>%
  filter(durt_val > 0) %>%
  mutate(embedlink = embedlist(status_url)) %>%
  select(status_id, embedlink) %>%
  right_join(tweet_durt)

#add pull_date formatdates

tweet_final <- tweet_final %>%
  mutate(pull_date = Sys.Date())

#If the user is in the database
 #update the pull date
tweet_final <- tweet_final %>%
  mutate(pull_date = Sys.Date())

#so if this statement is false get all the tweets for the user since that date. then get all
#the tweets since that most recent tweet then run all the stuff for it 
#and append it to the  top of dataframe. make that

if (as.Date(tweet_final$created_at[1], format = "%m-%d-%Y") + 7 > as.Date(tweet_final$pull_date[1]) == FALSE){
  
  new_tweets <- get_timeline('user', n = 3200 , since_id = (tweet_final$status_id[1]))
  
  if (len(new_tweets) > 1){
    new_tweets <- build_durt_df(new_tweets)
    all_tweets <- rbind(new_tweets, tweet_final)
    dat$durt <- all_tweets
    tweetsdb$remove({query})
    tweetsdb$insert(all_tweets)
  }
}

#Each tab will.
#1st Dig Durt will just show all the durty tweets

#2nd Tweets will let you drill down into the tweets
#So you can sort by durt by retweets or quote tweets, sort the durt score, sort by date,
#see top 10 sentiment, see bottom 10 sentimet. 

#3rd Analysis
#Percent of tweets are durty visualization.
#sentiment visualization, avg sentiment.
#durt sentiment visualization, avg sentiment.
#avg durt score, number of durt tweets, visualizations.
#include some sample tweets maybe for each. #rtweet(tweetshot())
#this could be like the printable build report.

#4th download.
#I give the user acess to download all tweets I pull but only a few feilds like:
#status_id, user_id, screen_name, created_at, text, is_retwet,is_quote.





