library(rtweet)
library(tidyverse)
#install.packages("tidytext")
library(tidytext)

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


durtdictionary <- tolower(durtwords)
durtdictionary <- unique(durtdictionary)

getdurt <- function(name) {
  
tweets <- get_timeline(name , n = 100000)

durt <- tweets %>%
  select(status_id, text)%>%
  mutate(text = tolower(text)) %>%
  mutate(text = gsub("[[:punct:]]", " ", text)) %>%
  unnest_tokens(word, text) %>%
  filter(word %in% durtdictionary) %>%
  select(status_id) %>%
  left_join(tweets, durt, by = NULL)

data.frame(durt)
}

   
durt <- getdurt('dreamchaserTy10')



highschooldurt <- getdurt(top50highschool)
draftdurt <- getdurt(top50draft)

top50highschool <- c('SmithNoland2', 'kayvonT8', 'JrStingley', 'jadon_haselwood', 'Antonioalfano99', '6sixGod_', 'ENeal73', 'ZP62019', 'buhbuhbru', 'darnell_5232', 'SpencerRattler',
                      'zacharrison_', 'Emery4____',  'daxhill5', 'K_Green_01', 'boimarv9', 'loganbrown53', 'brand0n_smith12', 'KobeDean2', 'GarrettWilson_V', '_TheoWeaseJr',
                      'D1Figure_', 'andrewbooth21', 'geo_Thagoat', 'opfreak15', 'claywebbG3', 'CharlesCross67', 'wanyamorris64', 'ealy_1k', 'h_miller76', 'Thechrishinton',
                      'MarcelBrooks_5', 'bo_nix10', 'J_Whitt3', 'JayD__5', 'domblaylock_1', 'dreamchaserTy10', 'Ford_Kyle6', '_FrankLadson', 'PierceQuick', '_mykael2',
                      'KinggChris7', 'jordantofly100', 'HenryTootoo1', 'LewisCine', 'zachcharbon', 'isopsher', 'Easymoney_Kai', 'jakesmith27', 'DoItAllDent103')

top50draft <- c('TheKylerMurray', 'nbsmallerbear', 'QuinnenWilliams' ,'Cle_Missile' ,'DevinWhite__40' ,'Daniel_Jones10' ,'JoshAllen41_' ,'TheeHOCK8' ,'Edoliver_11' ,'_Dbush11' ,'JonahGWilliams','RashanAGary' 
                ,'cwilkins42' ,'Big_Fish75','dh_simba7' ,'Fire_Burns99','llawrence2139','Gbradbury_11','GrindSimmons94','nrfant','darnellsavage_' ,'AndreDillard_' ,'levelstothis_2' ,'iAM_JoshJacobs' 
                ,'Primetime_jet' ,'_sweat9','JohnathanAbram1' ,'JerryTillery','ljcollier91' ,'DreBaker1_','KalebMcgary' ,'NkealHarry15','byronmurphy','Rock_152','jawaan_taylor74','Uno_Captain'
                ,'Thegreglittle','Cody_Ford74','MrSeanyB1','MullenIsland1','DaltonBigD71','DrewLock23','tavai31','Big_E_14','JoejuanW','Greedy','Erik_McCoy_73','benbanogu','swervinirvin_')



hsdurtclean <- highschooldurt %>%
  select(created_at, screen_name, name, location, status_url, text, is_retweet, favorite_count, retweet_count, retweet_name, retweet_location, profile_image_url)

draftdurtclean <- draftdurt %>%
  select(created_at, screen_name, name, location, status_url, text, is_retweet, favorite_count, retweet_count, retweet_name, retweet_location, profile_image_url)
  
hsdurtiest <- hsdurtclean %>%  
  group_by(screen_name) %>%
  count(name) %>%
  arrange(-n) %>%
  mutate(level = 'Top Highschool')
hsdurtiest <- hsdurtiest[1:10,]

draftdurtiest <- draftdurt %>%
  group_by(screen_name) %>%
  count(name) %>%
  arrange(-n) %>%
  mutate(level = 'Top Drafted')
draftdurtiest <- draftdurtiest[1:10,]

alldurtiest <- rbind(hsdurtiest, draftdurtiest)
  
ggplot(alldurtiest, aes(x = reorder(screen_name, n), y = n)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~level) +
  coord_flip() +
  theme_minimal() + xlab('Twitter User @') + ylab('DURTy Tweet Count')

hsmostwords <- hsdurtclean %>%
  unnest_tokens(word, text) %>%
  filter(word %in% durtwords) %>%
  select(word)

draftmostwords <- draftdurtclean %>%
  unnest_tokens(word, text) %>%
  filter(word %in% durtwords) %>%
  select(word)

allmostdurt <- rbind(hsmostwords, draftmostwords)

#install.packages('wordcloud')
#install.packages('tm')
library(wordcloud)
library(tm)

wordcloud(words = allmostdurt$word, min.freq = 1, max.words = Inf, random.order = FALSE, colors = 'Red')

allmostdurt2 <- allmostdurt %>%
  count(word)

most_durt <- ggplot(allmostdurt2, aes(x = reorder(word, n), y = n)) + geom_bar(stat = 'identity') + coord_flip() + theme_minimal()
ggplotly(most_durt)  
allcleaned <- rbind(draftdurtclean, hsdurtclean)

dates <- allcleaned %>%
  select(created_at) %>%
  separate(created_at, into = 'year', sep = '-') %>%
  count(year) 
  
accurate <- sample_n(allcleaned, 100)

hsdurtydate <- hsdurtclean %>%
  separate(created_at, into = 'Date', sep = ' ') %>%
  separate(Date, into = c('year', 'month'), sep = '-' ) %>%
  unite('year_month', c('year', 'month'), sep =  '-') %>%
  group_by(year_month) %>%
  count(year_month) %>%
  mutate(level = 'Highschool')

draftdurtydate <- draftdurtclean %>%
  separate(created_at, into = 'Date', sep = ' ') %>%
  separate(Date, into = c('year', 'month'), sep = '-' ) %>%
  unite('year_month', c('year', 'month'), sep =  '-') %>%
  group_by(year_month) %>%
  count(year_month) %>%
  mutate(level = 'Drafted')

durt_dates <- rbind(draftdurtydate, hsdurtydate)

Timeline <- ggplot(durt_dates, aes(alpha = 1/5)) + geom_point(aes(x = year_month, y = level, size = n, color = level)) +
  theme(plot.title = element_text(hjust = .5), axis.text = element_blank(), axis.ticks = element_blank())

library(plotly)

write_as_csv(draftdurt, 'draftdurt.csv')
write_as_csv(highschooldurt, 'highschooldurt.csv')

ggplotly(Timeline)

#citations
#https://rstudio.github.io/DT/shiny.html
#https://shiny.rstudio.com/gallery/datatables-options.html
#http://www.sthda.com/english/wiki/word-cloud-generator-in-r-one-killer-function-to-do-everything-you-need
#https://www.rdocumentation.org/packages/wordcloud/versions/2.6/topics/wordcloud

