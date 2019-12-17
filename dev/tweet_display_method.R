#Nico Manzonelli
#Text Analysis Methods

library(ggplot2)
library(rtweet)
library(dplyr)
library(tidytext)
library(DT)
library(shinycssloaders)


# SO THIS IS THE HTML REQUIRED TO EMBED TWEET ONE FROM KYLERS TWEETS.

#<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr"><a href="https://twitter.com/6goingfor6?ref_src=twsrc%5Etfw">@6goingfor6</a> @itsseanmsyg Are you the kid that thought he was clean with the taco meat on his head with the raggedy ass Kobe&#39;s?</p>&mdash; Kyler Murray (@K1) <a href="https://twitter.com/K1/status/347746126767734785?ref_src=twsrc%5Etfw">June 20, 2013</a></blockquote>
#  <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

#MAKE A FUNCTION THAT TAKES IN THAT LINK IN DATA SET AND SPITS OUT THE STUFF ABOVE.
#my initial idea was to scrape the embeded link right off twitter. But twitter uses some pretty jadded html in that part.
#Programatically convert using oEmbebd API.


library(httr)
library(curl)

#https://twitter.com/K1/status/347746126767734785  tweet link

link <- 'https://twitter.com/K1/status/365163373186387969'
  
getembed <- function(link){
  embedlink<- paste0('https://publish.twitter.com/oembed?url=',link)
  response <- httr::RETRY("GET", embedlink)
  httrcontent <- httr::content(response, as = "parsed")
  js <- httrcontent$html
  return(js)
}

getembed('https://twitter.com/K1/status/347746126767734785')

#this does it for 1 link but I want to do it for multiple links

linklist <- c('https://twitter.com/K1/status/347746126767734785','https://twitter.com/K1/status/272197060751679488')

embedlist <- function(linklist) {
  emlist <- c()
  for (link in linklist){
    embedlink <- getembed(link)
    emlist <- append(emlist, embedlink)
  }
  return(emlist)
}

getembed('https://twitter.com/K1/status/365163373186387969')

goal <- embedlist(df$status_url)

