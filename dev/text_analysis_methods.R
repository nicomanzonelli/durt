# Nico Manzonelli
# Text Analysis Methods

library(shiny)
library(ggplot2)
library(rtweet)
library(dplyr)
library(tidytext)
library(textdata)

kdurt <- read.csv('kylerstweets.csv', stringsAsFactors = FALSE)
ktweets <- kyler
ktweets <- kyler %>%
  mutate(text = tolower(text)) %>%
  select(status_id, user_id, created_at, screen_name, text, is_quote, is_retweet, status_url)

#tokenize k tweets

ktoken <- ktweets %>%
  unnest_tokens(word, text)

kfreq <- ktoken %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
  
#ok so you see some weird text stuff in there. probably attachte to html so lets get rid of these.

exclude <- rbind(stop_words, c('t.co', NA))
exclude <- rbind(exclude,c('http', NA)) 
exclude <- rbind(exclude, c('https', NA))
exclude <- rbind(exclude, c('gt', NA))
exclude <- rbind(exclude, c('lt', NA))

kfreq <- ktoken %>%
  anti_join(exclude) %>%
  count(word, sort = TRUE) %>%
  top_n(25)

#Ok lets do some sentiment analysis.

tweet_sentiment <- ktweets %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments('afinn')) %>%
  group_by(status_id) %>%
  summarize(sent_score = sum(value)) %>%
  right_join(ktweets)

durt_sentiment <- kdurt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments('afinn')) %>%
  group_by(status_id) %>%
  summarize(sent_score = sum(value)) %>%
  right_join(kdurt)


###bi-grams
kytweets <- read.csv('kylerstweets.csv', stringsAsFactors = FALSE)

kyler_bigram <- tweet_final %>% 
  select(status_id, text) %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  inner_join(get_sentiments('afinn'), by = c(word1 = 'word')) %>%
  inner_join(get_sentiments('afinn'), by = c(word2 = 'word')) %>%
  mutate(bi_value = value.x + value.y) %>%
  unite(bigram, word1, word2, sep = " ") %>% 
  count(bigram, bi_value) %>%
  select(status_id, bi_value) %>%
  right_join(tweet_final)

####lets visualize!

durt_sentiment$id <- 1:nrow(durt_sentiment)
ggplot(data = durt_sentiment, aes(id, sent_score)) +
  geom_bar(stat = "identity", show.legend = FALSE, na.rm = FALSE)

tweet_sentiment$id <- 1:nrow(tweet_sentiment)
ggplot(data = tweet_sentiment, aes(id, sent_score)) +
  geom_bar(stat = "identity", show.legend = FALSE, na.rm = FALSE)

dfpie <- data.frame(group = c('tweets','durt percentage'),
                    count = c(round((100-(nrow(kdurt)/nrow(ktweets)*100)),2), round((nrow(kdurt)/nrow(ktweets)*100),4)))

plot <- ggplot(dfpie, aes(x = "", y = count, fill = group, theme_minimal())) + geom_bar(width = 1, stat = 'identity') +
  ylab(" ") + xlab(" ") + ggtitle("Percentage of DURT tweets") + theme(plot.title = element_text(hjust = .5), 
  axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.background = element_blank()) + 
  geom_text(aes(label = ifelse(group=="durt percentage", count, "")))
pie <- plot + coord_polar("y", start = 0)
pie

ggplot(kfreq, aes(x = reorder(word, n), y = n)) + 
  geom_bar(stat = 'identity') + coord_flip() + 
  theme_minimal(base_size = 8.5) + xlab('') + ylab('Word Count') + 
  theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Most Used Words') 







       