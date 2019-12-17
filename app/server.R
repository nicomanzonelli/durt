
#initialize dev database

tweetsdb <- mongo('tweetsdb5.0')

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
      rename(attitude = sent_score) %>%
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
