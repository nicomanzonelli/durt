#.libPaths( c( .libPaths(), "/srv/.R/library") ) 

library(ggplot2)
library(rtweet)
library(dplyr)
library(tidytext)
library(DT)
library(shinycssloaders)

  APIkey <- "bGX8l5k6rYKMVwiKulsIjfNUK"
  APIcode <- "V8sSfll7wrgJxcHNnyCW7xcsNNxN8xjE6VJ21jLNkVqthacofK"
  token <- "1120751845213974528-pEBExgwOIk9q4IfQrdbRdFnGGLwRcU"
  tokencode <- "7vQvSGvEBWav52EdPcQJC00R97hn11eTa4ko82GcYzvbz"
    
  create_token( app = "DURT", APIkey, APIcode, token, tokencode)
    
  durtwords <- c('bitch','bitches','Bitch','Bitches', 'bitched','fuck', 'Fuck', 'FUCK', 'fucking', 'fucked', 'fucker', 'Fucked', 'Fucker', 'Fuckers', 'Cunt', 'cunt', 'CUNT', 'fag',
                   'FAG','Fag', 'faggot', 'Faggot', 'faggit', 'Faggit', 'Faggot', 'NIGGER','Nigger', 'nigger', 'Nigga', 'NIGGA', 'Nigga', 'Ass', 'ass', 'ASS', 'Bastard', 'BASTARD',
                   'bastard', 'Slut', 'SLUT', 'slut', 'WHORE', 'Whore', 'whore', 'skank', 'Skank', 'WHORE', 'porn', 'Porn', 'PORN', 'Vagina', 'VAGINA', 'vagina', 'dick', 'Dick', 'DICK',
                   'Pussy', 'PUSSY', 'pussy', 'penis', 'Penis', 'PENIS','Gay', 'GAY', 'gay','NIGGERS', 'niggas', 'niggers', 'NIGGAS', 'Gay', 'gays', 'GAYS', 'Gays', 'fags', 'Fags', 'Faggots',
                   'shit', 'SHIT', 'Shit', 'Shits', 'shits', 'Mexican', 'mexican', 'mexicans', 'Mexicans', 'Jew', 'Jews', 'jew', 'jews', 'spic', 'COCK', 'cock', 'cocks', 'Cock',
                   'retard', 'Retard', 'Retarded','retarded', 'chode', 'Chode', 'Cum', 'CUM', 'cum', 'Weed', 'weed', 'crack', 'Crack', 'Coke', 'coke', 'blow', 'Blow', 'ayo', 'yayo',
                   'alc', 'beer','Beer', 'Twat', 'twat', 'TWAT', 'prick', 'Prick', 'FUCKER', 'clit', 'Clit', 'CLIT', 'Jizz', 'jizz', 'sex', 'Sex', 'SEX', 'queef', 'Queef',
                   'blowjob', 'Blowjob', 'handjob', 'Handjob', 'choad', 'Choad', 'Tits', 'tits', 'TITS', 'Titties', 'titties', 'tiddies', 'Boobs', 'boobs', 'balls', 'Balls',
                   'ballsack', 'Ballsack', 'Piss', 'piss', 'Taint', 'taint', 'Pubes', 'pubes', 'nutsack', 'Nutsack', 'Spick', 'Spick', 'wetback', 'Terrorist', 'terrorist',
                   'LSD', 'Acid', 'acid', 'Nazi', 'nazi', 'NAZIS', 'NAZI', 'nazis', 'Dyke', 'dyke', 'Dike', 'dike', 'Coon', 'coon', 'COON', 'DIKE', 'DYKE', 'queer', 'Queer',
                   'trans', 'tranny','Tranny', 'NIG', 'nig', 'Nig', 'Lesbian', 'lesbian', 'Lesbo', 'lesbian', 'Redneck', 'redneck', 'Raghead', 'raghead', 'fat', 'FAT', 'Fat',
                   'towelhead', 'Towelhead', 'blunt', 'Blunt', 'Joint', 'joint')
  

  
getdurt <- function(name) {
    
  tweets <- get_timeline(name , n = 100000)
    
  durt <- tweets %>%
      select(status_id, text) %>%
      unnest_tokens(word, text) %>%
      filter(word %in% durtwords) %>%
      select(status_id) %>%
      left_join(tweets, durt, by = NULL)
    
  data.frame(durt)
}


shinyServer(function(input, output, session) {
  
  dat <- reactiveValues()
  
  output$tbl <- DT::renderDT({
    req(input$go_button)
    isolate({
      name <- input$user
      tweets <- get_timeline(name , n = 100000)
      if(nrow(tweets) > 1) {
        output$text <- renderText('user found, getting D.U.R.T.')
        durt <- getdurt(name) %>%
          select(created_at, name, location, text, is_retweet, favorite_count, retweet_count)
        
        dat$durt <- data.frame(durt)
      } else {
        output$text <- renderText('Sorry, no D.U.R.T. found on user.')
      }
    })
    DT::datatable(dat$durt)
  })
})




    
