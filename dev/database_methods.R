#Practicing databsing using mongolie and Method for app implementation.

#install.packages("mongolite")

library(mongolite)

m <- mongo('mtcars', url = "mongodb://localhost" )

m
 #YES FINALLY GOT THIS WORKIN!!!

#ok so the rest of this document I am going to work through the mongolite handbook. https://jeroen.github.io/mongolite/query-data.html

#using the diamonds data from ggplot

dmd <- mongo('diamonds') # create empty mongo database called diamonds

dmd$insert(ggplot2::diamonds) #insert diamond data into blank database

dmd$count() == nrow(ggplot2::diamonds) #verifys that data was inserted. count() counted rows compared to rows in original data set.
nrow(ggplot2::diamonds)
#MonogoDB uses JSON Syntax. The empty query '{}' means to select all data in db. 

dmd$count('{}') #get all records

alldata <- dmd$find('{}') #reads all the data back into R, queries the data.

head(alldata)

premium_diamonds <- dmd$find('{"cut" : "Premium", "price" : {"$lt" : 1000} }') #this code queries all rows where cut == PRremoum and price is greater than 1000


test <- dmd$find(
  query = '{"cut" : "Premium", "price" : { "$lt" : 1000 } }', #so this is the same querey as before
  fields = '{"cut" : true, "clarity" : true}', #this is the feild arguement which only selects the columns you tell it too.
  limit = 5 #limits the number of rows pulled
)

#by default monogo db always includes the id feild to prevent it you must disable it.

test <- dmd$find(
  query = '{ "cut" : "Premium", "price" : {"$lt" : 1000 } }',
  fields = '{"cut" : true, "clarity" : true, "_id" : false}',
  limit = 5
)

#if you need the 7 most expensive diamonds

dmd$find('{"cut" : "Premium"}', #query
         sort = '{"price":-1}',#sorting by price -1 gives us highest
         limit = 7
         ) 

#Note that usually you should only sort by fields that have an index set on them. 
#Sorting by unindexed fields can be very slow and the server might reach the memory limits on the server.

#Ok now using what we know lets do it build a fricking durt test!


library(rtweet)
library(tidyverse)
#install.packages("tidytext")
library(tidytext)

APIkey <- ""
APIcode <- ""
token <- ""
tokencode <- ""

create_token( app = "", APIkey, APIcode, token, tokencode)

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


durtdictionary <- tolower(durtwords)
durtdictionary <- unique(durtdictionary)
write.csv(durtdictionary, 'durtdictionary.csv')
yuh <- read.csv('durtdictionary.csv')

getdurt <- function(name) {
  
  tweets <- get_timeline(name , n = 100000)
  
  durt <- tweets %>%
    select(status_id, text)%>%
    mutate(text = tolower(text)) %>%
    mutate(text = gsub("[[:punct:]]", " ", text)) %>%
    unnest_tokens(word, text) %>%
    filter(word %in% yuh$x) %>%
    select(status_id) %>%
    left_join(tweets, durt, by = NULL) %>%
    select(status_id, user_id, created_at, screen_name, text, is_quote, is_retweet, status_url)
  
  data.frame(durt)
}

#play
durt <- getdurt('k1')
  
lfg1 <- mongo('lfg1')
lfg1$insert(durt)
lfg1$count('{}')
ty2 <- lfg1$find('{}')

durt2 <- getdurt('dreamchaserTy10')

lfg$insert(durt2)
lfg$count('{}')
ty3 <- lfg$find('{}')

name <- "K1"
query <- paste('{"screen_name":',paste0('"',name,'"'),'}')

lfg1$count(
  query = '{"screen_name":"K1"}'
)

lfg$count(query)


#so in the app server it would look something like. using the thing I made above. 

#initiate mongo server. like name the connection. db <- mongo('name of db')

shinyServer(function(input, output, session) {
  
  dat <- reactiveValues()
  
  output$tbl <- DT::renderDT({
    req(input$go_button)
    isolate({
      
      name <- input$user
      query <- paste('{"screen_name":',name,'}')
      
      if(db$count(query) == 0){
        
          tweets <- get_timeline(name , n = 1000000)
          
        if(nrow(tweets) > 1) {
          
          output$text <- renderText('user found, getting D.U.R.T.')
          durt <- getdurt(name)
          dat$durt <- data.frame(durt)
          #db$insert(durt) this is where we decide if we want the user writing into my db.
          
        } else {
            output$text <- renderText('Sorry, no D.U.R.T. found on user.')
        }
      } else{
          output$text <- renderText('user found in database')
          dat$durt <- data.frame(db$find(query))
      }
    })
    DT::datatable(dat$durt)
  })
})

#need to put an if statement in there to do the date thing but I think thats chill i think then