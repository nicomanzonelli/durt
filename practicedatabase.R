


inputSuser <- 'Bino'

users <- read_csv("userpractice.csv")

if(inputSuser %in% users$User == TRUE) {
  
  dftweets <- read_csv("dftweets.csv")
  data.frame(dftweets[dftweets$User == 'Nico', ])
  
} else{
  
  add input$user to dfuser
  write.csv(dfuser)
  run the durt thing as regular
  tweets <- new df
  read.csv(dftweets)
  add new df to dftweets
  write.csv(dftweets)
  
}


if(inputSuser %in% users$User == TRUE){
  print('works')
  
} else {
  print('works2')
  userspractice <- rbind(users, inputSuser)
  
  print('rundirt')
}


  
