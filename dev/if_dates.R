# Nico Manzonelli
# Nov 2019

#How to format and compare dates. Used to only upp up to a certain date of status_ids. Pulled from a
#tweet final thing in my enviroment. basically just some dates.

#We added pull_dates to the df_final

tweet_final$created_at[1] #so this is nov 14 2019

tweet_final$pull_date[1] #this is is nov 15 2019

tweet_final$pull_date[1] > tweet_final$created_at[1] #returns error because its not standard format

as.Date(tweet_final$created_at[1], format = "%m-%d-%Y") #standard format

as.Date(tweet_final$pull_date[1])

#method for comparing dates. so this takes the first row (most recenet tweet pulled and compares it to the currrent date)
as.Date(tweet_final$created_at[1], format = "%m-%d-%Y") > as.Date(tweet_final$pull_date[1])
#is nov 14 2019 greater (newer than) nov 15 2019 is false.. this seems cool.

#we need to x days (lets just say a week) to add time to the tweet date to compare to the pull_date.
as.Date(tweet_final$created_at[1], format = "%m-%d-%Y") + 7

#So then this statement returns true if the most recent tweet is less than a week old, and false if 
# it is older than a week old.

as.Date(tweet_final$created_at[1], format = "%m-%d-%Y") + 7 > as.Date(tweet_final$pull_date[1])

#so if this statement is false get all the tweets for the user since that date. then get all
#the tweets since that most recent tweet then run all the stuff for it 
#and append it to the  top of dataframe. make that

if ((as.Date(tweet_final$created_at[1], format = "%m-%d-%Y") + 7 > as.Date(tweet_final$pull_date[1])) == FALSE) {
 print("do the stuff")
}
#new_tweets <- get_timeline('user', n = 3200 , since_id = (tweet_final$status_id[1]))
  
#  if (len(new_tweets) > 1){
#    new_tweets <- build_durt_df(new_tweets)
#    all_tweets <- rbind(new_tweets, tweet_final)
#   dat$durt <- all_tweets
#   tweetsdb$remove({query})
#   tweetsdb$insert(all_tweets)
# }


