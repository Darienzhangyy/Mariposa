library(shiny)

locs <- availableTrendLocations()
usid <- locs[which(locs$country == "United States"), c(1,3)]
rownames(usid) = NULL
colnames(usid) = c("City", "woeid")
city <- usid$City

shinyUI(
  fluidPage(
    titlePanel(
      "Twitter API: Sentimental analysis and geographial statistics"
    ),
    sidebarPanel(
      numericInput('n_tweets', h4('Number of Tweets:'), value=10, min=100, step=1),
      hr(),
      h4('Keyword:'),
      textInput('Keywd', "Search keyword", value = "Paris"),
      hr(),

      selectInput('State', 'State:', c("Alabama", "Alaska", "Arizona", "Arkansas",
                                       "California", "Colorado", "Connecticut",
                                       "Delaware", "Florida", "Georgia", "Hawaii",
                                       "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
                                       "Kentucky", "Louisiana", "Maine", "Maryland",
                                       "Massachusetts", "Michigan", "Minnesota",
                                       "Mississippi", "Missouri", "Montana", "Nebraska",
                                       "Nevada", "New Hampshire", "New Jersey",
                                       "New Mexico", "New York", "North Carolina",
                                       "North Dakota", "Ohio", "Oklahoma", "Oregon",
                                       "Pennsylvania", "Rhode Island", "South Carolina",
                                       "South Dakota", "Tennessee", "Texas", "Utah",
                                       "Vermont", "Virginia", "Washington", "West Virginia",
                                       "Wisconsin", "Wyoming")),
      hr(),
      actionButton("go", "Let's Search"),
      hr(),
      selectInput('City', 'City:', city),
      hr()
    ),
    mainPanel(
      h4('Overall word cloud:'),
      plotOutput('overall_cloud'),
      br(),
      h4('State word cloud:'),
      plotOutput('state_cloud'),
      br(),
      h4('State Sentiments:'),
      plotOutput('state_Sent'),
      br(),
      h4('Trends:'),
      tags$head(tags$style( HTML('#mytable table {border-collapse:collapse; } 
                             #mytable table th { transform: rotate(-0deg)}'))),
      column(12,tableOutput("trendtable"))

#       h4('Posteriors:'),
#       plotOutput('all_posterior'),
#       br(),
#       h4('Posterior Statistics:'),
#       tableOutput('postTable'),
#       textOutput('text1'),
#       textOutput('text2')
    )
  )
)
