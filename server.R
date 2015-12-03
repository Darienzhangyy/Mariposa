require(truncnorm)
require(parallel)
require(ggplot2)
require(gridExtra)
library(twitteR)
library(stringr)
library(httr)
library(RCurl)
library(rjson)
library(tm)
library(wordcloud)
library(foreach)

api_key <- "hftNGPFqzf9GMnHLMQVnNV7Vz"

api_secret <- "HjTPQByqFDdriKKEixRbm4UNn2golHR5qGPbrPogRDm0tQkFua"

access_token <- "296897722-PllUrWEvYediPUkYtJf8dwr2vmZe0p5O0mWP46Cc"

access_token_secret <- "1NySI44qSZW0Ie6SEesd1T28NOun3NiOPyhe3QxLaNvuY"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

key = "acf0a65de0967a683856519de8b80921"

geoinfo_state = data.frame("State" = c("Alabama", "Alaska", "Arizona", "Arkansas",
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
                                       "Wisconsin", "Wyoming"),
                           "Geo" = c("32.31823,-86.9023,350mi","64.20084,-149.4937,350mi",
                                     "34.04893,-111.0937,350mi","35.20105,-91.83183,350mi",
                                     "36.77826,-119.4179,350mi","39.55005,-105.7821,350mi",
                                     "41.60322,-73.08775,350mi","38.91083,-75.52767,350mi",
                                     "27.66483,-81.51575,350mi","32.16562,-82.90008,350mi",
                                     "19.89677,-155.5828,350mi","44.0682,-114.742,350mi",
                                     "40.63312,-89.39853,350mi","40.26719,-86.1349,350mi",
                                     "41.878,-93.0977,350mi","39.0119,-98.48425,350mi",
                                     "37.83933,-84.27002,350mi","30.9843,-91.96233,350mi",
                                     "45.25378,-69.44547,350mi","39.04575,-76.64127,350mi",
                                     "42.40721,-71.38244,350mi","44.31484,-85.60236,350mi",
                                     "46.72955,-94.6859,350mi","32.35467,-89.39853,350mi",
                                     "37.96425,-91.83183,350mi","46.87968,-110.3626,350mi",
                                     "41.49254,-99.90181,350mi","38.80261,-116.4194,350mi",
                                     "43.19385,-71.5724,350mi","40.05832,-74.40566,350mi",
                                     "34.51994,-105.8701,350mi","40.71278,-74.00594,350mi",
                                     "35.75957,-79.0193,350mi","47.55149,-101.002,350mi",
                                     "40.41729,-82.90712,350mi","35.46756,-97.51643,350mi",
                                     "43.80413,-120.5542,350mi","41.20332,-77.19452,350mi",
                                     "41.58009,-71.47743,350mi","33.83608,-81.16372,350mi",
                                     "43.96951,-99.90181,350mi","35.51749,-86.58045,350mi",
                                     "31.9686,-99.90181,350mi","39.32098,-111.0937,350mi",
                                     "44.5588,-72.57784,350mi","37.43157,-78.65689,350mi",
                                     "38.90719,-77.03687,350mi","38.59763,-80.4549,350mi",
                                     "43.78444,-88.78787,350mi","43.07597,-107.2903,350mi"),
                           "StateAbb" = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","IA",
                                       "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
                                       "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK",
                                       "OG","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV",
                                       "WI","WY"))
locs <- availableTrendLocations()
usid <- locs[which(locs$country == "United States"), c(1,3)]
rownames(usid) = NULL
colnames(usid) = c("City", "woeid")

shinyServer(
  function(input, output, session) {
#     observe(
#       {
#         updateSliderInput(session, 'n_paired', max=round(floor(input$n_total/2)))
#       }
#     )

    overall = reactive(
      {

        getSentiment <- function (text, key){

          text <- URLencode(text);

          #save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
          text <- str_replace_all(text, "%20", " ");
          text <- str_replace_all(text, "%\\d\\d", "");
          text <- str_replace_all(text, " ", "%20");


          if (str_length(text) > 360){
            text <- substr(text, 0, 359);
          }
          ##########################################

          data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", key, "&text=",text, sep=""))

          js <- fromJSON(data);

          # get mood probability
          sentiment = js$output$result

          ###################################


          return(list(sentiment=sentiment))
        }

        clean.text <- function(some_txt)
        {
          some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
          some_txt = gsub("@\\w+", "", some_txt)
          some_txt = gsub("[[:punct:]]", "", some_txt)
          some_txt = gsub("[[:digit:]]", "", some_txt)
          some_txt = gsub("http\\w+", "", some_txt)
          some_txt = gsub("[ \t]{2,}", "", some_txt)
          some_txt = gsub("^\\s+|\\s+$", "", some_txt)
          some_txt = gsub("amp", "", some_txt)
          # define "tolower error handling" function
          try.tolower = function(x)
          {
            y = NA
            try_error = tryCatch(tolower(x), error=function(e) e)
            if (!inherits(try_error, "error"))
              y = tolower(x)
            return(y)
          }

          some_txt = sapply(some_txt, try.tolower)
          some_txt = some_txt[some_txt != ""]
          names(some_txt) = NULL
          return(some_txt)
        }



        ###########################################################

        db_key <- "804ab73cf3d214af5dc587c8198daa33"

        print("Getting tweets...")
        keyword = input$Keywd
        # get some tweets
        tweets = searchTwitter(keyword, input$n_tweets, lang="en")
        # get text
        tweet_txt = sapply(tweets, function(x) x$getText())

        # clean text
        tweet_clean = clean.text(tweet_txt)
        tweet_num = length(tweet_clean)
        # data frame (text, sentiment)
        tweet_df = data.frame(text=tweet_clean, sentiment=rep("", tweet_num),stringsAsFactors=FALSE)

        print("Getting sentiments...")
        # apply function getSentiment
        sentiment = rep(0, tweet_num)
        
        foreach(i=1:tweet_num) %dopar%
        {
          tmp = getSentiment(tweet_clean[i], db_key)

          tweet_df$sentiment[i] = tmp$sentiment

          print(paste(i," of ", tweet_num))


        }

        # delete rows with no sentiment
        tweet_df <- tweet_df[tweet_df$sentiment!="",]


        #separate text by sentiment
        sents = levels(factor(tweet_df$sentiment))
        #emos_label <- emos


        # get the labels and percents

        labels <-  mclapply(sents, function(x) paste(x,format(round((length((tweet_df[tweet_df$sentiment ==x,])$text)/length(tweet_df$sentiment)*100),2),nsmall=2),"%"), mc.cores=8)



        nemo = length(sents)
        emo.docs = rep("", nemo)
        
        foreach(i=1:nemo) %dopar%
        {
          tmp = tweet_df[tweet_df$sentiment == sents[i],]$text

          emo.docs[i] = paste(tmp,collapse=" ")
        }



        # remove stopwords
        emo.docs = removeWords(emo.docs, stopwords("german"))
        emo.docs = removeWords(emo.docs, stopwords("english"))
        corpus = Corpus(VectorSource(emo.docs))
        tdm = TermDocumentMatrix(corpus)
        tdm = as.matrix(tdm)
        colnames(tdm) = labels




        # comparison word cloud
        cpCloud = comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                         scale = c(3,.5), random.order = FALSE, title.size = 1.5)
        cpCloud
      }
    )



####################################################################################


#Based on different states

    statewise = reactive(
      {
        getSentiment <- function (text, key){

          text <- URLencode(text);

          #save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
          text <- str_replace_all(text, "%20", " ");
          text <- str_replace_all(text, "%\\d\\d", "");
          text <- str_replace_all(text, " ", "%20");


          if (str_length(text) > 360){
            text <- substr(text, 0, 359);
          }
          ##########################################

          data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", key, "&text=",text, sep=""))

          js <- fromJSON(data);

          # get mood probability
          sentiment = js$output$result

          ###################################


          return(list(sentiment=sentiment))
        }

        clean.text <- function(some_txt)
        {
          some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
          some_txt = gsub("@\\w+", "", some_txt)
          some_txt = gsub("[[:punct:]]", "", some_txt)
          some_txt = gsub("[[:digit:]]", "", some_txt)
          some_txt = gsub("http\\w+", "", some_txt)
          some_txt = gsub("[ \t]{2,}", "", some_txt)
          some_txt = gsub("^\\s+|\\s+$", "", some_txt)
          some_txt = gsub("amp", "", some_txt)
          # define "tolower error handling" function
          try.tolower = function(x)
          {
            y = NA
            try_error = tryCatch(tolower(x), error=function(e) e)
            if (!inherits(try_error, "error"))
              y = tolower(x)
            return(y)
          }

          some_txt = sapply(some_txt, try.tolower)
          some_txt = some_txt[some_txt != ""]
          names(some_txt) = NULL
          return(some_txt)
        }



        ###########################################################
        geocode_state = geoinfo_state[which(geoinfo_state$State==input$State),2]
        ###########################################################

        db_key <- "804ab73cf3d214af5dc587c8198daa33"

        print("Getting tweets...")
        keyword = input$Keywd
        # get some tweets
        tweets = searchTwitter(keyword, input$n_tweets, lang="en",geocode = paste(geocode_state))
        # get text
        tweet_txt = sapply(tweets, function(x) x$getText())

        # clean text
        tweet_clean = clean.text(tweet_txt)
        tweet_num = length(tweet_clean)
        # data frame (text, sentiment)
        tweet_df = data.frame(text=tweet_clean, sentiment=rep("", tweet_num),stringsAsFactors=FALSE)

        print("Getting sentiments...")
        # apply function getSentiment
        sentiment = rep(0, tweet_num)
        foreach(i=1:tweet_num) %dopar%
        {
          tmp = getSentiment(tweet_clean[i], db_key)

          tweet_df$sentiment[i] = tmp$sentiment

          print(paste(i," of ", tweet_num))


        }

        # delete rows with no sentiment
        tweet_df <- tweet_df[tweet_df$sentiment!="",]


        #separate text by sentiment
        sents = levels(factor(tweet_df$sentiment))
        #emos_label <- emos


        # get the labels and percents

        labels <-  mclapply(sents, function(x) paste(x,format(round((length((tweet_df[tweet_df$sentiment ==x,])$text)/length(tweet_df$sentiment)*100),2),nsmall=2),"%"), mc.cores = 8)



        nemo = length(sents)
        emo.docs = rep("", nemo)
        foreach (i=1:nemo) %dopar%
        {
          tmp = tweet_df[tweet_df$sentiment == sents[i],]$text

          emo.docs[i] = paste(tmp,collapse=" ")
        }



        # remove stopwords
        emo.docs = removeWords(emo.docs, stopwords("german"))
        emo.docs = removeWords(emo.docs, stopwords("english"))
        corpus = Corpus(VectorSource(emo.docs))
        tdm = TermDocumentMatrix(corpus)
        tdm = as.matrix(tdm)
        colnames(tdm) = labels




        # comparison word cloud
        cpCloud = comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                                   scale = c(3,.5), random.order = FALSE, title.size = 1.5)
        cpCloud
      })




####################################################################################
 #all state sentiment

#     statewiseSent = reactive(
#       {
# 
#         getSentiment <- function (text, key){
# 
#           text <- URLencode(text);
# 
#           #save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
#           text <- str_replace_all(text, "%20", " ");
#           text <- str_replace_all(text, "%\\d\\d", "");
#           text <- str_replace_all(text, " ", "%20");
# 
# 
#           if (str_length(text) > 360){
#             text <- substr(text, 0, 359);
#           }
#           ##########################################
# 
#           data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", key, "&text=",text, sep=""))
# 
#           js <- fromJSON(data);
# 
#           # get mood probability
#           sentiment = js$output$result
# 
#           ###################################
# 
# 
#           return(list(sentiment=sentiment))
#         }
# 
#         clean.text <- function(some_txt)
#         {
#           some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
#           some_txt = gsub("@\\w+", "", some_txt)
#           some_txt = gsub("[[:punct:]]", "", some_txt)
#           some_txt = gsub("[[:digit:]]", "", some_txt)
#           some_txt = gsub("http\\w+", "", some_txt)
#           some_txt = gsub("[ \t]{2,}", "", some_txt)
#           some_txt = gsub("^\\s+|\\s+$", "", some_txt)
#           some_txt = gsub("amp", "", some_txt)
#           # define "tolower error handling" function
#           try.tolower = function(x)
#           {
#             y = NA
#             try_error = tryCatch(tolower(x), error=function(e) e)
#             if (!inherits(try_error, "error"))
#               y = tolower(x)
#             return(y)
#           }
# 
#           some_txt = sapply(some_txt, try.tolower)
#           some_txt = some_txt[some_txt != ""]
#           names(some_txt) = NULL
#           return(some_txt)
#         }
# 
#         db_key <- "da81488559b8434a537834dd9bc09e8e"
# 
# 
# 
#         ##########################################################
# 
# 
#         #GEO_senti = data.frame(State = NULL, negative = NA, neutral = NA, positive = NA)
#         GEO_senti = matrix(NA, 50, 4)
# 
#         for (i in 1:length(geoinfo_state)){
# 
#         geocode_state = geoinfo_state[i,2]
#         ###########################################################
# 
# 
# 
#         print("Getting tweets...")
#         keyword = input$Keywd
#         # get some tweets
#         tweets = searchTwitter(keyword, 10, lang="en",geocode = paste(geocode_state) )
#         # get text
#         tweet_txt = sapply(tweets, function(x) x$getText())
# 
#         # clean text
#         tweet_clean = clean.text(tweet_txt)
#         tweet_num = length(tweet_clean)
#         # data frame (text, sentiment)
#         tweet_df = data.frame(text=tweet_clean, sentiment=rep("", tweet_num),stringsAsFactors=FALSE)
# 
#         print("Getting sentiments...")
#         # apply function getSentiment
#         sentiment = rep(0, tweet_num)
#         for (j in 1:tweet_num)
#         {
#           tmp = getSentiment(tweet_clean[j], db_key)
# 
#           tweet_df$sentiment[j] = tmp$sentiment
# 
#           print(paste(j," of ", tweet_num))
# 
# 
#         }
# 
#         # delete rows with no sentiment
#         tweet_df <- tweet_df[tweet_df$sentiment!="",]
# 
# 
#         #separate text by sentiment
#         sents = levels(factor(tweet_df$sentiment))
#         #emos_label <- emos
# 
# 
#         # get the labels and percents
# 
#         labels <-  lapply(sents, function(x) paste(x,format(round((length((tweet_df[tweet_df$sentiment ==x,])$text)/length(tweet_df$sentiment)*100),2),nsmall=2),"%"))
# 
#         #Extract the sentiments percentages
#         labels_extract = as.numeric(gsub("\\D", "", labels))/10000
#         GEO_senti[i,] = c(geoinfo_state[i,3],labels_extract[1],labels_extract[2],labels_extract[3])
#         }
# 
#         GEO_senti = as.data.frame(GEO_senti)
#         colnames(GEO_senti) = c("State", "Negative", "Neutral", "Positive")
# 
# 
# #Draw the map
# 
# 
#         suppressPackageStartupMessages(library(googleVis))
#         op=options(gvis.plot.tag='chart')
#         Senti_heat = gvisGeoChart( GEO_senti, locationvar='State', colorvar = 'Positive',
#                                 options=list(title="Heated map of State Sentiments",
#                                              titlevar="Title",region='US',projection="kavrayskiy-vii",numvar="Positive",
#                                              displayMode="regions", resolution="provinces",
#                                              colorAxis="{colors:['yellow','red']}",width=650, height=400))
# 
#         T <- gvisTable(GEO_senti,
#                        options=list(width=270, height=400))
# 
#         GT <- gvisMerge(Senti_heat,T, horizontal=TRUE)
#         plot(GT)
#       }
#     )

    # Simulate the sock-picking process under user-specified priors for later comparison.
#     sims = reactive (
#       {
#         # Simulate the sock-picking process and return the number of pairs.
#         gen_model = function(prior_n_socks, prior_prop_pairs, prior_pair, prior_odd) {
#           socks = rep(seq_len(prior_pair + prior_odd),
#                       rep(c(2, 1), c(prior_pair, prior_odd)))
#           picked_socks = sample(socks, size=min(input$n_total, prior_n_socks))
#           sock_counts = table(picked_socks)
#           return(sum(sock_counts==2))
#         }
#
#         # Separate the data into 8 subsets for parallel processing via mclapply.
#         n_chunks = 8
#         steps = floor(seq(1, input$n_sims, len=n_chunks+1))
#         l = list()
#         l[[1]] = priors()[steps[1]:steps[2],]
#         for(i in 2:n_chunks) {
#           l[[i]] = priors()[(steps[i]+1):steps[i+1],]
#         }
#
#         # Run the data, broken into subsets, through the generative model in parallel.
#         unlist(mclapply(l, function(x) {
#           apply(x, 1, function(x) {
#             gen_model(x[1],x[2],x[3],x[4])
#           })
#         }, mc.cores=8))
#       }
#     )
#
#     # Match the simulated sock data to the user-specified number of found pairs.
#     posterior = reactive(
#       {
#         priors()[sims()==input$n_paired,]
#       }
#     )
#
#     # Create a data frame containing the 95% CI and median for all priors.
#     pp = reactive(
#       {
#         p = as.data.frame(apply(priors(), 2, quantile, c(0.025, 0.5, 0.975)))
#         colnames(p) = c('V1', 'V2', 'V3', 'V4')
#         p
#       }
#     )

    # Plot all prior distributions, with lines marking the 95% CI and median values.
    output$overall_cloud = renderPlot(
      {
        if (input$go){
        print(overall())}
      }
    )

    output$state_cloud = renderPlot(
      {
        if (input$go){
        print(statewise())}
      }
    )
    
    output$trendtable = renderTable(
      {
        trends <- getTrends(usid$woeid[which(usid$City == input$City)])
        Trend <- matrix(head(trends$name, 5))
        colnames(Trend) <- input$City
        Trend
        
      }
    )
    

    # Create a data frame containing the 95% CI and median for all posteriors.
#     qq = reactive(
#       {
#         q = as.data.frame(apply(posterior(), 2, quantile, c(0.025, 0.5, 0.975)))
#         colnames(q) = c('V1', 'V2', 'V3', 'V4')
#         q
#       }
#     )
#
#     # Plot all posterior distributions, with lines marking the 95% CI and median values.
#     output$all_posterior = renderPlot(
#       {
#         q1 = ggplot(posterior(), aes(x=total)) +
#           geom_histogram(fill='#619CFF') +
#           labs(x=NULL, y=NULL, title='Total number of socks') +
#           geom_vline(data=qq(), aes(xintercept=V1), linetype=c(3,5,3))
#         q2 = ggplot(posterior(), aes(x=prop)) +
#           geom_histogram(fill='#619CFF') +
#           labs(x=NULL, y=NULL, title='Paired proportion of socks') +
#           geom_vline(data=qq(), aes(xintercept=V2), linetype=c(3,5,3))
#         q3 = ggplot(posterior(), aes(x=n_pair)) +
#           geom_histogram(fill='#619CFF') +
#           labs(x=NULL, y=NULL, title='Number of sock pairs') +
#           geom_vline(data=qq(), aes(xintercept=V3), linetype=c(3,5,3))
#         q4 = ggplot(posterior(), aes(x=n_odd)) +
#           geom_histogram(fill='#619CFF') +
#           labs(x=NULL, y=NULL, title='Number of singletons') +
#           geom_vline(data=qq(), aes(xintercept=V4), linetype=c(3,5,3))
#         q_all = grid.arrange(q1, q2, q3, q4, ncol=2)
#         print(q_all)
#       }
#     )
#
#     # Summarize ABC output in tabular format.
#     output$postTable = renderTable(
#       {
#         df = data.frame(low=apply(posterior(), 2, quantile, 0.025),
#                         med=c(as.integer(median(posterior()[,1])),
#                               median(posterior()[,2]),
#                               as.integer(median(posterior()[,3])),
#                               as.integer(median(posterior()[,4]))),
#                         high=apply(posterior(), 2, quantile, 0.975))
#         colnames(df) = c('95% Credible Interval Lower Bound', 'Median',
#                          '95% Credible Interval Upper Bound')
#         rownames(df) = c('Total Number of Socks', 'Paired Proportion', 'Number of Pairs',
#                          'Number of Singletons')
#         df
#       },
#       align='cccc'
#     )
#
#     # Summarize ABC output in verbal form.
#     output$text1 = renderText(
#       {
#         paste('After pulling', input$n_total, "socks out of the dryer, you've found", input$n_paired,
#               'pairs. Your best guess is', round(median(posterior()[,1])), 'socks in total.')
#       }
#     )
#
#     # Reveal the outcome from our motivating example.
#     output$text2 = renderText(
#       {
#         if(input$showtruevalue==T) {
#           'There were 21 pairs and 3 singletons, for a total of 45 socks.'
#         } else {
#           "(Check the box on the side panel in order to reveal how many socks were in Broman's laundry.)"
#         }
#       }
#     )

  }
)