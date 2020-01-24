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


APIkey <- ""
APIcode <- ""
token <- ""
tokencode <- ""

create_token( app = "", APIkey, APIcode, token, tokencode)

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
    unnest_tokens(word, text) %>%
    mutate(text = tolower(text)) %>%
    mutate(text = gsub("[[:punct:]]", " ", text)) %>%
    inner_join(get_sentiments('afinn')) %>%
    group_by(status_id) %>%
    summarize(sent_score = sum(value)) %>%
    right_join(df)
  #assign durt value to every durt tweet (all non-durt are NA)
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
    filter(durt_val > 0)
  
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

#initialize dev database

tweetsdb <- mongo('tweetsdb5.0')

runApp(list(
  
ui = navbarPage('D.U.R.T.',
  
  tabPanel('Dig',              
    tags$head(
      tags$script("!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document,'script','twitter-wjs');")
    ),
    h1('Dig D.U.R.T.', align = 'center'),
    h5(em('Derogatory Use of Regular Text'), align = 'center'),
    fluidRow(column(12, align = 'center', textInput('user', label = '', value = NULL, placeholder = 'enter user @'))),
    fluidRow(column(12, align = 'center', actionButton('go_button', label = 'Dig DURT'))),
    br(), br(),
    h5(textOutput('text'), align = 'center'),
    uiOutput('html_tweets') %>% withSpinner()
    
    
      ),
  tabPanel('Drill',
           fluidPage(
             h1('Drill Into DURT', align = 'center'),
             h5('Select specific tweets based on their overall attitude, DURTiness, or date.', align = 'center'),
             fluidRow(
               column(6,
                      h3('Tweet Table'),
                      DT::dataTableOutput('table1')),
               column(6, 
                      htmlOutput('htmltable')))
             
           )),
  
  tabPanel('Analyze',
           fluidPage(
             h1('Twitter Breakdown', align = 'center'),
             fluidRow(
               column(6,
                      plotOutput('pieplot'),
                      plotOutput('wordfreq')
               ),
               column(6,
                      plotOutput('cloud'),
                      plotOutput('wordfreqd')
                
               ),
             ),
             fluidRow(plotlyOutput('plotly'), width = 1),
           )),
  
  tabPanel('Download',
           h1("Download the Data", align = 'center'),
           h5("Select the the number of tweets you wish to sample and download the raw data.", align = 'center'),
           fluidRow(column(12, align = 'center', downloadButton("downloadData","Download"))),
           fluidRow((column(12, align = 'center', sliderInput("slide", "", min = 0, max = 1000, value = 500)))),
           h5(""),
           DT::DTOutput('tbl') %>% withSpinner())

  ),
  
 server = function(input, output, session) {
   
   options(shiny.sanitize.errors = TRUE)
   dat <- reactiveValues()
   
   observeEvent(input$go_button, {
     
     dat$durt <- NULL
     name <- input$user
     query <- paste('{"screen_name":',paste0('"',name,'"'),'}')
     
     if(tweetsdb$count(query) == 0){
       tweets <- get_timeline(name , n = 10000)
       output$text <- renderText('user not found in database')

       if(nrow(tweets) > 1) {
         output$text <- renderText('user found on twitter, getting durt')
         durtdf <- build_durt_df(tweets)
         #tweetsdb$insert(durtdf) #adds full df to database
         dat$durt <- data.frame(durtdf)

       } else {
          output$text <- renderText('user not on twitter or is private')
       }
       
     } else {
       output$text <- renderText('user found in database, getting durt')
       tweets <- data.frame(tweetsdb$find(query))
       output$text <- renderText('user found in database, updating durt')
       
       if ((as.Date(tweets$created_at[1], format = "%m-%d-%Y") + 7 > as.Date(tweets$pull_date[1])) == FALSE) {
         
         new_tweets <- get_timeline(name, n = 10000 , since_id = (tweets$status_id[1]))
         
         if (nrow(new_tweets) > 1){
           new_tweets <- build_durt_df(new_tweets)
           all_tweets <- rbind(new_tweets, tweets)
           dat$durt <- all_tweets
           tweetsdb$remove({query})
           tweetsdb$insert(all_tweets)
         } else{
           output$text <- renderText('user found in database. all tweets up to date.')
           dat$durt <- tweets
         } 
       } else{
         output$text <- renderText('user found in database. all tweets up to date.')
         dat$durt <- tweets
       }
       
     }
   })

   output$tbl <- DT::renderDT({
      DT::datatable(sample_n(dat$durt, input$slide),
                    options = list(dom = 't',   scrollX = T, scrollCollapse = T))
                                                                    
     })
   
   output$html_tweets <- renderUI({
     if (length(dat$durt) > 0) {
       durte <- dat$durt %>%
         select(durt_val, embedlink) %>%
         filter(durt_val > 0)
       HTML(paste0('<center>',durte$embedlink,'</center>'))
     } else{
     HTML(paste0('<center>',dat$durt$embedlink,'</center>'))
     }
   })
   
   output$downloadData <- downloadHandler(
     filename = paste0(input$user, "tweets.csv"),
     content = function(file) {
       write.csv(dat$durt, file, row.names = FALSE)
     }
   )
   
  output$table1 <- DT::renderDataTable(
  
    table1 <- dat$durt %>%
    select(created_at, sent_score, durt_val, embedlink) %>%
    filter(durt_val > 0) %>%
    rename(sentiment_score = sent_score) %>%
    rename(durt_score = durt_val),
    server = TRUE, selection = 'single',
    options = list(pageLength = 10,
                   dom = 'l',
                   columnDefs = list(list(visible=FALSE, targets = 4)))
  )
    
  
  output$htmltable <- renderUI({
    table1 <- dat$durt %>%
      select(created_at, sent_score, durt_val, embedlink) %>%
      filter(durt_val > 0) %>%
      rename(Attiude = sent_score) %>%
      rename(DURT_Score = durt_val)
    
    HTML(paste0('<center>',table1[input$table1_rows_selected,4],'<center>'))
  })
  
  output$pieplot <- renderPlot(
    make_pie(dat$durt)
  )
  
  output$wordfreq <- renderPlot(
    make_freq(dat$durt)
  )
  
  output$wordfreqd <- renderPlot(
    make_freqd(dat$durt)
  )
  
  output$cloud <- renderPlot(
    make_wcloud(dat$durt)
  )
  
  output$plotly <- renderPlotly(
    make_sent_time(dat$durt)
  )
 }


))


 
