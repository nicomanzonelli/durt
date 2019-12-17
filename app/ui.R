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
                           h5('Select specific tweets in the tweet table based on their overall attitude, durtiness, or date.', align = 'center'),
                           fluidRow(
                             column(6,
                                    h3('Tweet Table'),
                                    DT::dataTableOutput('table1')),
                             column(6, 
                                    htmlOutput('htmltable')))
                           
                         )),
                
                tabPanel('Analyze',
                         fluidPage(
                           h1('Account Breakdown', align = 'center'),
                           h5('Analyze and visualize account DURT and overall Twitter use.', align = 'center'),
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
                
)

