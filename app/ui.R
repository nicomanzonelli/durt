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
                                    br(),
                                    br(),
                                    htmlOutput('htmltable')))
                           
                         )),
                
                tabPanel('Analyze',
                         fluidPage(
                           h1('Account Breakdown', align = 'center'),
                           h5('Visualize account DURT and overall Twitter use.', align = 'center'),
                           br(),
                           fluidRow(
                             column(6,
                                    plotOutput('pieplot'),
                                    plotOutput('wordfreq')
                             ),
                             br(),
                             column(6,
                                    plotOutput('cloud'),
                                    plotOutput('wordfreqd')
                                    
                             ),
                           ),
                           br(),
                           fluidRow(plotlyOutput('plotly'), width = 1),
                         )),
                
                tabPanel('Download',
                         h1("Download the Data", align = 'center'),
                         h5("Select the the number of tweets you wish to sample and download the raw data.", align = 'center'),
                         fluidRow(column(12, align = 'center', downloadButton("downloadData","Download"))),
                         fluidRow((column(12, align = 'center', sliderInput("slide", "", min = 0, max = 1000, value = 500)))),
                         br(),
                         DT::DTOutput('tbl') %>% withSpinner()),
                
                tabPanel('Info',
                          h1(tags$b('D.U.R.T.'), align = 'center'),
                          h5(em('Derogatory Use of Regular Text'), align = 'center'),
                          br(),
                          h5("DURT is a R Shiny application that allows the user to check twitter accounts for potentially unprofessional, derogatory, or otherwise offensive content.", align = 'center'),
                          h5("It also gives general insights into a person's twitter use. DURT provides a straightforward open source solution designed to allow individuals or organizations to screen twitters.", align = 'center'),
                          br(),
                          h5('Read more about DURT on dscoe, or visit github/nicomanzonelli/durt to learn how DURT works.', align = 'center'),
                          br(),
                          h5(tags$b('Getting errors?'), align = 'center'),
                          h5('First, check to make sure you searched a Twitter user in the "Dig" panel.
                            If you have not searched a user on the first tab, the other panels will automatically return errors.', align = 'center'),
                          h5('If you encounter other errors in the app, please raise the issue on github or contact nico.manzonelli@westpoint.edu', align = "center")
                         )
                
                
                
)

