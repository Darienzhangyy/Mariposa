list.of.packages <- c("parallel","truncnorm","ggplot2","gridExtra","twitteR","stringr","httr","RCurl",
                      "rjson","tm","wordcloud","foreach","googleVis","shiny")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, suppressPackageStartupMessages(library),character.only = TRUE)

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
      numericInput('n_tweets', h4('Number of Tweets:'), value=10, min=1, step=1),
      hr(),
      h4('Keyword:'),
      textInput('Keywd', "Search Keyword", value = "Paris"),
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
      sliderInput('n_topKey_State', h4('Number of State Top Words:'),
                  value=20, min=1,max=50,step=1),
      hr(),
      checkboxInput('showcitytrend', "Show me City Tweet Trend !"),
      hr(),
      conditionalPanel(
        condition="input.showcitytrend == true",
        selectInput('City', 'City:', city),
        sliderInput('n_topKey', h4('Number of Top Tweets:'), value=20, min=1,max=50,step=1)
      ),
      hr(),
      actionButton("go", "Let's Search!"),
      hr(),
      img(src="http://www.timeslive.co.za/incoming/2012/05/10/twitter-bird/ALTERNATES/crop_630x400/twitter+bird",
          height = 170, width = 245)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Keyword percentage",h4("Keyword percentage:"),htmlOutput('state_map'),
                 h4("Keyword Bubbleplot:"),htmlOutput('keywd_bubble')),
        tabPanel("Overall word cloud with Sentiments",h4("Overall word cloud:"),plotOutput('overall_cloud'),
                 h4('State word cloud:'), br(), plotOutput('state_cloud')),
        tabPanel("Hot Keywords",h4("State Keyword related Hot Words:"),htmlOutput("Statetable"),
                 textOutput('text1'),
                 tags$head(tags$style("#text1{color: black;
                                      font-size: 20px;
                                      font-weight: bold;
                                      }")),
      br(),
      tags$head(tags$style( HTML('#mytable table {border-collapse:collapse; }
                                 #mytable table th { transform: rotate(-0deg)}'))),
      column(12,htmlOutput("trendtable")))
      )
    )
  )
)
