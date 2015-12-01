library(shiny)

total_priors = c('Negative Binomial'='nbinom', 'Poisson'='pois')
prop_priors = c('Beta'='beta', 'Truncated Normal'='tnorm')

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
      h4('Priors:'),
      selectInput('State', 'State:', "North Carolina"),
      hr()
    ),
    mainPanel(
      h4('Overall word cloud:'),
      plotOutput('overall_cloud'),
      br(),
      h4('State word cloud:'),
      plotOutput('state_cloud'),
      br()
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
