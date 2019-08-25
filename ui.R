.libPaths( c( .libPaths(), "/srv/.R/library") )   

library(shinycssloaders)

shinyUI(navbarPage('D.U.R.T.',
                   
  tabPanel('Dig',
           h1('Dig D.U.R.T.', align = 'center'),
           h5(em('Derogatory Use of Regular Text'), align = 'center'),
           textInput('user', label = '', value = NULL, placeholder = 'enter user @', width = '100%'),
           actionButton('go_button', label = 'Dig DURT'),
           br(), br(),
           DT::DTOutput('tbl') %>% withSpinner(),
           h5(textOutput('text'), align = 'center')
           ),
  
  tabPanel('About', 
           h1('About D.U.R.T.', align = 'center'),
           h5(em('Derogatory Use of Regular Text'), align = 'center'),
           h5("The internet is forever. Everything you have ever instagramed, facebooked, or tweeted can be held against you. In todays' soceity, old tweets come to haunt young professionals. The Derogatory Use of Regular Text can ruin lifes. D.U.R.T. is a powerful search engine that allows its users to scrape through tweets posted on different Twitter profiles. This Shiny app utilizes R-Studio coding, in coalition with a Twitter API, to analyze a large quantity of data from Twitter."),
           h5(''),
           h5(em('This app may need to be refreshed before reuse and it cannot handle many users at once'), align = 'center')
           ),
  
  tabPanel('Research',
           h1('D.U.R.T. for Research', align = 'center'),
           h5('We conducted simple research included in the report below.', align = 'center'),
           shiny::includeHTML('durthtmlforapp.html')
           
           )
  
)) 
