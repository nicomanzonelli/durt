library(shiny)
library(ggplot2)
library(rtweet)
library(dplyr)
library(tidytext)
library(textdata)
library(DT)
library(shinycssloaders)
library(mongolite)
library(httr)
library(qdapTools)


APIkey <- "hAgbJjvF1pzlp9Oal5PnjKo1Z"
APIcode <- "TvPg4eSocDHO10HbcI0QcTrI4QKXf2DB0FwLPAtQgzGIYcVQpD"
token <- "1120751845213974528-ZY3Js2dkozOmgQgpEs6Rzjx4ktrVw9"
tokencode <- "BmT5WA0PEWrcHv8AFcAsnQJuRYxNKVyTBzhOapR4GSPiz"

create_token( app = "DURT", APIkey, APIcode, token, tokencode)

durtdict <- read.csv('durtdict.csv')

getdurt <- function(tweets) {
  
  durt <- tweets %>%
    select(status_id, text)%>%
    mutate(text = tolower(text)) %>%
    mutate(text = gsub("[[:punct:]]", " ", text)) %>%
    unnest_tokens(word, text) %>%
    filter(word %in% durtdict$word) %>%
    select(status_id) %>%
    left_join(tweets, durt, by = NULL)
  
  data.frame(durt)
}

getembed <- function(link){
  embedlink<- paste0('https://publish.twitter.com/oembed?url=',link)
  response <- httr::RETRY("GET", embedlink)
  httrcontent <- httr::content(response, as = "parsed")
  js <- httrcontent$html
  return(js)
}

embedlist <- function(linklist) {
  emlist <- c()
  for (link in linklist){
    embedlink <- getembed(link)
    emlist <- append(emlist, embedlink)
  }
  return(emlist)
}

build_durt_df <- function(df) {
  #add sentiment value to df
  tweet_sentiment <- df %>%
    mutate(text = tolower(text)) %>%
    mutate(text = gsub("[[:punct:]]", " ", text)) %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments('afinn')) %>%
    group_by(status_id) %>%
    summarize(sent_score = sum(value)) %>%
    right_join(df)
  #assign durt value to every durt tweet (all non-durt are 0)
  tweet_durt <- tweet_sentiment %>%
    select(text, status_id) %>%
    mutate(text = tolower(text)) %>%
    mutate(text = gsub("[[:punct:]]", " ", text)) %>%
    unnest_tokens(word, text) %>%
    filter(word %in% durtdict$word) %>%
    mutate(durtvalue = word %l% durtdict) %>%
    right_join(distinct(tweet_sentiment)) %>%
    group_by(status_id) %>%
    mutate(durt_val = sum(durtvalue, na.rm = TRUE)) %>%
    distinct(status_id, durt_val) %>%
    right_join(distinct(tweet_sentiment))
  #add embedlink to every durt tweet (all non-durt are NA)
  tweet_final <- tweet_durt %>%
    filter(durt_val > 0) %>%
    mutate(embedlink = embedlist(status_url)) %>%
    select(status_id, embedlink) %>%
    right_join(tweet_durt)
  #add pull date column
  tweet_final <- tweet_final %>%
    mutate(pull_date = Sys.Date())
  
  return(data.frame(tweet_final))
}

make_pie <- function(df){
  
  dfdurt <- df %>%
    filter(durt_val > 0)
  
  dfpie <- data.frame(group = c('tweets','durt percentage'),
                      count = c(round((100-(nrow(dfdurt)/nrow(df)*100)),2), round((nrow(dfdurt)/nrow(df)*100),4)))
  
  plot <- ggplot(dfpie, aes(x = "", y = count, fill = group, theme_minimal())) + geom_bar(width = 1, stat = 'identity') +
    ylab(" ") + xlab(" ") + ggtitle("Percentage of DURT tweets") + theme(plot.title = element_text(hjust = .5), 
                                                                         axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), 
                                                                         panel.grid.minor = element_blank(), panel.background = element_blank()) + 
    geom_text(aes(label = ifelse(group=="durt percentage", count, "")))
  pie <- plot + coord_polar("y", start = 0)
  pie
  
}

make_wcloud <- function(df){
  library(wordcloud)
  library(tm) 
  
  dfdurt <- df %>%
    filter(durt_val > 0) %>%
    unnest_tokens(word, text) %>%
    filter(word %in% durtdict$word) %>%
    count(word, sort = TRUE)
  
  wordcloud(words = dfdurt$word, freq = dfdurt$n, min.freq = 1, scale = c(8, 0.5),
            max.words = Inf, random.order = FALSE, colors = '#F8766D')
}

make_freq <- function(df){
  
  exclude <- rbind(stop_words, c('t.co', NA))
  exclude <- rbind(exclude,c('http', NA)) 
  exclude <- rbind(exclude, c('https', NA))
  exclude <- rbind(exclude, c('gt', NA))
  exclude <- rbind(exclude, c('lt', NA))
  
  durtdf <- df %>%
    unnest_tokens(word, text) %>%
    anti_join(exclude) %>%
    count(word, sort = TRUE) %>%
    top_n(25)
  
  durtdf$durt <- ifelse(durtdf$word %in% durtdict$word, "red", "blue")
  
  ggplot(durtdf, aes(x = reorder(word,n), y = n, fill = durtdf$durt)) + 
    geom_bar(stat = 'identity') + coord_flip() + 
    scale_fill_manual(values = c('#00BFC4','#F8766D')) +
    theme_minimal(base_size = 8.5) + xlab('') + ylab('Word Count') + 
    theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Most Used Words') +
    theme(legend.position = "none")
}

make_freqd <- function(df){
  
  dfdurt <- df %>%
    filter(durt_val > 0) %>%
    unnest_tokens(word, text) %>%
    filter(word %in% durtdict$word) %>%
    count(word, sort = TRUE)
  
  ggplot(dfdurt, aes(x = reorder(word, n), y = n)) + 
    geom_bar(stat = 'identity', aes(fill = 'red')) + coord_flip() + 
    theme_minimal(base_size = 8.5) + xlab('') + ylab('Word Count') + 
    theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Most Used DURT Words') +
    theme(legend.position = "none")
}

make_sent_time <- function(df){
  
  dfplot <- df %>%
    select(created_at, sent_score, durt_val, text)
  
  dfplot$color <- ifelse(dfplot$durt_val > 0, "red", "blue") 
  
  dfplot_temp <- df %>%
    select(created_at, sent_score, durt_val, text) %>%
    filter(durt_val >0)
  
  dfplot_temp2 <- dfplot %>%
    filter(color == "blue")
  
  dfplot_temp2$created_at2 = c(dfplot_temp$created_at, rep(NA, times = as.integer(nrow(dfplot_temp2)-nrow(dfplot_temp))))
  dfplot_temp2$sent_score2 = c(dfplot_temp$sent_score, rep(NA, times = as.integer(nrow(dfplot_temp2)-nrow(dfplot_temp))))
  dfplot_temp2$durt_val2 = c(dfplot_temp$durt_val, rep(NA, times = as.integer(nrow(dfplot_temp2)-nrow(dfplot_temp))))
  dfplot_temp2$text2 = c(dfplot_temp$text, rep(NA, times = as.integer(nrow(dfplot_temp2)-nrow(dfplot_temp))))
  
  plot_ly(dfplot_temp2, type = 'bar', x = ~ as.Date(created_at), y = ~sent_score, name = 'Clean', text = ~text,
          marker = list(color = '#00BFC4')) %>%
    add_trace(x = ~ as.Date(created_at2), y = ~sent_score2, name = 'DURT', text = ~text2,
              marker = list(color = '#F8766D'))%>%
    layout(yaxis = list(title = 'Attitude'), barmode = 'stack', xaxis = list(title = 'Date'),
           title = "All Time Tweet Attitude") %>%
    rangeslider()
  
}