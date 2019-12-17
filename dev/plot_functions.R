#Nico Manzonelli


#define all plot functions to work with the reactive value(df) generated from durt
library(data.table)
library(ggplot2)
library(plotly)


df <- tweetsdb$find('{}')

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
  
  wordcloud(words = dfdurt$word, freq = dfdurt$n, min.freq = 1, scale = c(2, 0.5),
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
  
  dfplot$color <- ifelse(is.na(dfplot$durt_val)==FALSE, "red", "blue") 
  
  dfplot_temp <- df %>%
    select(created_at, sent_score, durt_val, text) %>%
    filter(durt_val >0)
  
  dfplot_temp2 <- dfplot %>%
    filter(color == "blue")
  
  dfplot_temp2$created_at2 = c(dfplot_temp$created_at, rep(NA, nrow(dfplot_temp2)-nrow(dfplot_temp)))
  dfplot_temp2$sent_score2 = c(dfplot_temp$sent_score, rep(NA, nrow(dfplot_temp2)-nrow(dfplot_temp)))
  dfplot_temp2$durt_val2 = c(dfplot_temp$durt_val, rep(NA, nrow(dfplot_temp2)-nrow(dfplot_temp)))
  dfplot_temp2$text2 = c(dfplot_temp$text, rep(NA, nrow(dfplot_temp2)-nrow(dfplot_temp)))
  
  plot_ly(dfplot_temp2, type = 'bar', x = ~ as.Date(created_at), y = ~sent_score, name = 'Clean', text = ~text,
               marker = list(color = '#00BFC4')) %>%
    add_trace(x = ~ as.Date(created_at2), y = ~sent_score2, name = 'DURT', text = ~text2,
              marker = list(color = '#F8766D'))%>%
    layout(yaxis = list(title = 'Attitude'), barmode = 'stack', xaxis = list(title = 'Date'),
           title = "All Time Tweet Attitude") %>%
    rangeslider()
  
}





