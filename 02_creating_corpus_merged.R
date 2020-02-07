
#--------------------------------Social Media Analytics-------------------------------#
####                                                                               ####
####        This file reads in previously downloaded tweets from RData files       ####
####  Team members: Stephanie Beyer Diaz, Manoj Purushothaman, Edward Vrijghem     ####
####                                 Company: Sprint                               ####
####                                                                               ####
#-------------------------------------------------------------------------------------#



#-----------------------------------Loading Packages----------------------------------#

for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','tidyr',
            'purrr','rtweet','ggplot2','wordcloud','textdata','syuzhet','leaflet',
            'topicmodels','viridis','hrbrthemes', 'ggridges','ggthemes','widyr',
            'textstem','corrplot','geojsonio','rgeos','data.table','igraph','ggraph',
            'shinyWidgets','shinydashboard','shiny')){
  if (!require(i, character.only=TRUE)) install.packages(i
                      , repos = "http://cran.us.r-project.org")
                      require(i, character.only=TRUE) }


#------------------------------------Loading Data-------------------------------------#

#Loading tweets by FOLLOWERS OF SPRINT ACCOUNTS
followers <- get(load("followers.RData"))

#Loading tweets by SPRINT ACCOUNTS
corp_accounts <- get(load("corp_accounts.RData"))
corpSentiment <- get(load("corpSentiment.RData"))

#Loading general tweets mentioning SPRINT-RELATED KEYWORDS
tweets_df <- get(load("tweets_df.RData"))

#Loading general tweets mentioning SPRINT-RELATED KEYWORDS, excluding corporate accounts
user_tweets<- get(load("user_tweets.RData"))

#Loading LDA output
lda_df <- get(load("lda_df.RData"))

#Loading wordcloud input
freqtable <- get(load("freqtable.RData"))

#Loading hashtag input
hashtag_dist <- get(load("hashtag_dist.RData"))

#loading dictionaries
afinn <- get(load("afinn.RData"))
bing <- get(load("bing.RData"))
loughran <- get(load("loughran.RData"))
nrc <- get(load("nrc.RData"))
syuzhet <- get(load("syuzhet.RData"))

#-----------------------------------Tokenizing Data-----------------------------------#

# Tokenization function, with stemming, lemmatization and adding custom stopwords optional
# requires clean_tweet column
tokenize <- function(data,stem=FALSE,lemma=TRUE,custom=c(),dtm=TRUE){
  stoppers <- get_stopwords()
  if (!is.null(custom)){
    for (cust in custom){
      stoppers <- rbind(stoppers,c(cust,'custom'))}
  }
  tokenized <- data %>% unnest_tokens(output = "word", 
                                      input = clean_tweet, 
                                      token = "words", 
                                      drop=FALSE,to_lower=TRUE) %>%
    anti_join(stoppers) %>%
    mutate(word_stem = wordStem(word)) 
  
  if (stem==FALSE){ #stemming not always necessary
    tokenized$word_stem <- NULL}
  
  else {
    tokenized$word <- tokenized$word_stem
    tokenized$word_stem <- NULL}
  
  if (lemma==TRUE){
    tokenized$word <- lemmatize_words(tokenized$word)
  }
  
  
  if (dtm==TRUE) {
    tokenized <- tokenized %>%
      count(status_id,word , sort=TRUE) %>%
      cast_dtm(document = status_id, term = word,
               value = n, weighting = tm::weightTf)
  }
  return(tokenized)
}


#----------------------------Mapping Avg Sentiment by State---------------------------#
#overall buzz
tweets_token <- tokenize(tweets_df,dtm = FALSE)

#map downloaded from https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

#reformatting data
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_format <- tidy(spdf, region = "google_name")

#Calculate the centroid of each hexagon to add state label:
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

state_sentiment <-inner_join(tweets_token,afinn) 
state_sentiment <- state_sentiment %>% group_by(status_id) %>% 
  transmute(sentiment=mean(afinn)) %>% unique()

state_sentiment <- inner_join(tweets_df,state_sentiment) 
state_sentiment <- state_sentiment %>% group_by(state) %>% 
  transmute(sentiment = mean(sentiment)) %>% unique()

#----------------------------------------SHINY-----------------------------------------#
ui <- dashboardPage( skin = "yellow",
                     dashboardHeader(title = "Sprint Twitter Analytics Dashboard"),
                     #creation of the sidebar menu   
                     dashboardSidebar(sidebarMenu(
                       menuItem("Topic Modelling", tabName = "TOPIC_MODELLING1", icon = icon("chart-line", lib = "font-awesome")),
                       menuItem("Sentiment Analysis", tabName = "Sentiment_Analysis2", icon = icon("smile", lib = "font-awesome")),
                       menuItem("Sentiment By Engagement", tabName = "Word_Correlation3", icon = icon("caret-right", lib = "font-awesome")),
                       menuItem("Map Of The USA", tabName = "USA", icon = icon("flag", lib = "font-awesome")),
                       menuItem("Token Frequency", tabName = "Token_Frequency", icon = icon("dashboard")))),
                     
                     #creation of dashboard body       
                     dashboardBody(
                       tabItems(
                         tabItem(tabName = "Token_Frequency",
                          fluidRow(
                           box(plotOutput("frequencyplot")),
                           box(plotOutput("wordcloud1"))),
                           fluidRow(
                             box (sliderInput("frequency",label = "Select number of words:",min = 0, max = 500, value = 500)))),
                         tabItem(tabName = "Word_Correlation3", fluidRow(
                           box(plotOutput("sentimentscore2")),
                           box(plotOutput("sentimentscore"))),
                           fluidRow(
                             box(sliderInput("topN2",label = "Select number of words:",min = 2, max = 5, value = 5),
                                 dateRangeInput("date",label= "specify date range", start = "2018-05-01", end = "2020-02-28",format = "yyyy-mm-dd",
                                                startview = "month", weekstart = 0,language = "en", separator = " till ")))),
                         tabItem(tabName = "Sentiment_Analysis2", fluidRow( 
                           box(plotOutput("hashtag")),
                           box(plotOutput("wordcontribute"))),
                           fluidRow(
                             box(sliderInput("topN",label = "Select number of popular hashtags",min = 1, max = 5, value = 5),
                                 pickerInput("word","Select a hashtag:",c('sprint','iot','mysprintrewards','hire','5g'),selected =c('sprint','iot','mysprintrewards','hire','5g'),options = list(`actions-box` = TRUE),multiple = T),width = 3),
                             box(plotOutput("hashtag2")))),
                         tabItem(tabName = "USA", fluidRow(
                           box(pickerInput("state","Select a state:", c("All",state_sentiment[!is.na(state_sentiment$state),]$state),selected = "All", 
                                           options = list(`actions-box` = TRUE),multiple = T),width = 3)),
                           fluidRow(
                             box(plotOutput("mapusa"),width = "75%")),
                           fluidRow(
                             box(plotOutput("graph1"),width = "100%"))),
                         tabItem(tabName = "TOPIC_MODELLING1", fluidRow(
                           box (sliderInput("kslider",label = "Select the number of topics:",min = 2, max = 10, value = 10),width = 5)),
                           fluidRow(
                             infoBoxOutput("nusers"),
                             infoBoxOutput("ntweets"),
                             infoBoxOutput("avgage"),
                           fluidRow(
                             box(plotOutput("LDA",width = "100%"), width= "100%"),
                             fluidRow(
                               box(plotOutput("wordcloudtopic",width = "100%"), width= "100%"))))
                         ))))

server <- function(input, output) {
  
  output$wordcloud1 <- renderPlot({wordcloud(words = freqtable$word, freq = freqtable$wordfreq, min.freq = 1,
              max.words=input$frequency, rot.per=0.35, 
              colors=(brewer.pal(9,"Spectral")),random.color = TRUE,scale=c(4,.3))
  })
  
  output$frequencyplot <- renderPlot({
    
    if (input$frequency==""){
      x <- freqtable
    } else if (input$frequency>=25){
      x <- freqtable[1:25,]
    } else {
      x <- freqtable[1:input$frequency,]
    }
    ggplot(x, aes(x=reorder(word,wordfreq), y=wordfreq,fill=wordfreq)) + geom_bar(stat = "identity") + xlab("top words (max 25)") + ylab("Frequency")+ scale_fill_distiller(palette="Spectral")+theme_bw() +coord_flip() 
  })
  
  output$LDA <- renderPlot({
    #TOPIC MODELING (tweets with keywords)
    #loading LDA model df, filtering by number of topics
    topics <- lda_df[lda_df$k == input$kslider,]
    
    topics <- topics[complete.cases(topics),c("topic","date","term1","term2","term3","term4","term5","term6","term7",
                            "term8","term9","term10")]
    
    ggplot(data = topics, 
           aes(x = date, y = paste0("term",topic), color=paste0("term",topic), fill=paste0("term",topic))) +
      geom_density_ridges(alpha = 0.8, scale = 5) +
      scale_fill_viridis(option = "A", discrete = TRUE, direction=-1,
                         labels=topics[1,c("term1","term2","term3","term4","term5","term6","term7",
                                           "term8","term9","term10")]) +
      scale_color_viridis(option = "A", discrete = TRUE, direction=-1) + 
      theme_few() +
      labs(y=NULL,x=NULL, fill='Topics') +
      guides(colour=FALSE) +
      scale_y_discrete(
        labels=topics[1,c("term1","term2","term3","term4","term5","term6","term7",
                          "term8","term9","term10")]) +
      ggtitle("Topic Density Distribution in Time")
  })
  
  output$nusers <- renderInfoBox({
    user_filt <- seq(1,input$kslider)
    
    #cleaning description text
    
    user_tweets_filt <- user_tweets[user_tweets$topic %in% user_filt,]
    
    #creating df with unique rows of user info
    user_info <- user_tweets_filt %>% group_by(user_id,screen_name,clean_desc,location,
                                               account_created_at,topic) %>%
      dplyr::summarise(
        #columns with max are because tweets were downloaded on different dates:
        #there may be differences in data saved, which would result in duplicates
        statuses_count = max(statuses_count), 
        followers_count = max(followers_count),
        friends_count = max(friends_count),
        favourites_count = max(favourites_count),
        tweets = n(), #total tweets, including retweets
        rts = sum(is_retweet), #total retweets (of other users)
        og_tweets = length(is_retweet[is_retweet==FALSE]), #total original tweets
        n_rts_rt = sum(retweet_count[is_retweet==TRUE]), #number of retweets of retweeted tweets
        n_rts_tw = sum(retweet_count[is_retweet==FALSE]),
        n_source = n_distinct(source) #number of retweets of original tweets
      )
    
    #account age in years
    user_info$acc_age <- as.double(difftime(Sys.Date(),user_info$account_created_at,units="days")/365)
    
    #Final profile of followers
    profile <- user_info %>% ungroup() %>% transmute("Account Age" = mean(acc_age),
                                                     "Avg Tweets" = mean(statuses_count),
                                                     "% Original Tweets" = mean(og_tweets/tweets),
                                                     "Avg Followers" = mean(followers_count),
                                                     "Avg Faves" = mean(favourites_count),
                                                     "Avg RTs" = mean(rts),
                                                     "N Users" = n_distinct(user_id)
    ) %>% unique()
    
    #TODO: table to be added in shiny
    profile <- unique(profile)
    profile
    
    infoBox(
      "Number of users", paste0(round(profile$`N Users`)), icon = icon("user-friends", lib = "font-awesome"))
    
  })
  
  output$ntweets <- renderInfoBox({
    user_filt <- seq(1,input$kslider)
    
    #cleaning description text
    
    user_tweets_filt <- user_tweets[user_tweets$topic %in% user_filt,]
    
    #creating df with unique rows of user info
    user_info <- user_tweets_filt %>% group_by(user_id,screen_name,clean_desc,location,
                                               account_created_at,topic) %>%
      dplyr::summarise(
        #columns with max are because tweets were downloaded on different dates:
        #there may be differences in data saved, which would result in duplicates
        statuses_count = max(statuses_count), 
        followers_count = max(followers_count),
        friends_count = max(friends_count),
        favourites_count = max(favourites_count),
        tweets = n(), #total tweets, including retweets
        rts = sum(is_retweet), #total retweets (of other users)
        og_tweets = length(is_retweet[is_retweet==FALSE]), #total original tweets
        n_rts_rt = sum(retweet_count[is_retweet==TRUE]), #number of retweets of retweeted tweets
        n_rts_tw = sum(retweet_count[is_retweet==FALSE]),
        n_source = n_distinct(source) #number of retweets of original tweets
      )
    
    #account age in years
    user_info$acc_age <- as.double(difftime(Sys.Date(),user_info$account_created_at,units="days")/365)
    
    #Final profile of followers
    profile <- user_info %>% ungroup() %>% transmute("Account Age" = mean(acc_age),
                                                     "Avg Tweets" = mean(statuses_count),
                                                     "% Original Tweets" = mean(og_tweets/tweets),
                                                     "Avg Followers" = mean(followers_count),
                                                     "Avg Faves" = mean(favourites_count),
                                                     "Avg RTs" = mean(rts),
                                                     "N Users" = n_distinct(user_id)
    ) %>% unique()
    
    #TODO: table to be added in shiny
    profile <- unique(profile)
    profile
    
    infoBox(
      "Number of Tweets", paste0(round(profile$`Avg Tweets`)), icon = icon("twitter", lib = "font-awesome"))
  })
  
  
  output$avgage <- renderInfoBox({
    user_filt <- seq(1,input$kslider)
    
    #cleaning description text
    
    user_tweets_filt <- user_tweets[user_tweets$topic %in% user_filt,]
    
    #creating df with unique rows of user info
    user_info <- user_tweets_filt %>% group_by(user_id,screen_name,clean_desc,location,
                                               account_created_at,topic) %>%
      dplyr::summarise(
        #columns with max are because tweets were downloaded on different dates:
        #there may be differences in data saved, which would result in duplicates
        statuses_count = max(statuses_count), 
        followers_count = max(followers_count),
        friends_count = max(friends_count),
        favourites_count = max(favourites_count),
        tweets = n(), #total tweets, including retweets
        rts = sum(is_retweet), #total retweets (of other users)
        og_tweets = length(is_retweet[is_retweet==FALSE]), #total original tweets
        n_rts_rt = sum(retweet_count[is_retweet==TRUE]), #number of retweets of retweeted tweets
        n_rts_tw = sum(retweet_count[is_retweet==FALSE]),
        n_source = n_distinct(source) #number of retweets of original tweets
      )
    
    #account age in years
    user_info$acc_age <- as.double(difftime(Sys.Date(),user_info$account_created_at,units="days")/365)
    
    #Final profile of followers
    profile <- user_info %>% ungroup() %>% transmute("Account Age" = mean(acc_age),
                                                     "Avg Tweets" = mean(statuses_count),
                                                     "% Original Tweets" = mean(og_tweets/tweets),
                                                     "Avg Followers" = mean(followers_count),
                                                     "Avg Faves" = mean(favourites_count),
                                                     "Avg RTs" = mean(rts),
                                                     "N Users" = n_distinct(user_id)
    ) %>% unique()
    
    #TODO: table to be added in shiny
    profile <- unique(profile)
    profile
    
    infoBox(
      "Number of Tweets", paste0(round(profile$`Account Age`,2)), icon = icon("birthday-cake", lib = "font-awesome"))
  })
    
  output$wordcloudtopic <- renderPlot({
    topics <- lda_df[lda_df$k == input$kslider,]
    topic_df <- as.data.frame(topics)
    topic_df$status_id <- rownames(topic_df)
    user_tweets_filt <- left_join(user_tweets,topic_df)
    bio <- unique(user_tweets_filt[,c("screen_name","clean_desc","location","topic")])
    wordcloud(bio$clean_desc,max.words = 250,
              colors=brewer.pal(input$kslider, "Spectral")[factor(bio$topic)], scale=c(4,.3))}
    )

  output$hashtag <- renderPlot({
    #top5 hashtags and some sentiment information
    hashtag_top <- hashtag_dist %>% group_by(word) %>% transmute(count=n_distinct(status_id),
                                                                 afinn=mean(afinn[!is.na(afinn)]),
                                                                 syuzhet=mean(syuzhet[!is.na(syuzhet)]),
                                                                 bing_pos=n_distinct(status_id[bing=='positive']),
                                                                 bing_neg=n_distinct(status_id[bing=='negative'])) %>% unique()
    
    ht_n <- input$topN
    hashtag_filt <- hashtag_top[order(-hashtag_top$count),][1:ht_n,] 
    
    #preparing plot data
    hashtag_plot <- rbind(hashtag_filt[c("word","bing_pos")],hashtag_filt[c("word","bing_neg")])
    hashtag_plot$label <- ifelse(is.na(hashtag_plot$bing_pos),"negative","positive")
    hashtag_plot$bing <- ifelse(is.na(hashtag_plot$bing_pos),hashtag_plot$bing_neg,hashtag_plot$bing_pos)
    hashtag_plot$bing_pos <- NULL
    hashtag_plot$bing_neg <- NULL
    
    ggplot(hashtag_plot, aes(word)) + 
      geom_bar(aes(weight = bing, fill=label) ,position = "dodge") +
      coord_flip() +
      ggtitle("Most Popular Hashtags With Bing Sentiment Dictionary")
  })
  
  output$hashtag2 <- renderPlot({
    hashtag_top <- hashtag_dist %>% group_by(word) %>% transmute(count=n_distinct(status_id),
                                                                 afinn=mean(afinn[!is.na(afinn)]),
                                                                 syuzhet=mean(syuzhet[!is.na(syuzhet)]),
                                                                 bing_pos=n_distinct(status_id[bing=='positive']),
                                                                 bing_neg=n_distinct(status_id[bing=='negative'])) %>% unique()
    
    ht_n <- input$topN
    hashtag_filt <- hashtag_top[order(-hashtag_top$count),][1:ht_n,] 
    
    hashtag_jitter <- hashtag_dist[!(is.na(hashtag_dist$bing) | is.na(hashtag_dist$nrc)),
                                   c("word","nrc","bing","engagement")]
    hashtag_jitter <- hashtag_jitter[hashtag_jitter$word %in% hashtag_filt$word,]
    
    if (input$word==""){
      hashtag_jitter 
    } else {
      hashtag_jitter <- hashtag_jitter[hashtag_jitter$word %in% input$word,]
    }
    
    
    ggplot(hashtag_jitter, aes(bing,nrc)) +
      geom_jitter(aes(color = word, alpha=0.8, size = engagement)) +
      ggtitle("Most Popular Hashtags, NRC vs Bing Classifications")
  })
    
  output$wordcontribute <- renderPlot({
    
    user_token <- left_join(user_tweets,hashtag_dist[!is.na(hashtag_dist$word),c("status_id","word")]) 
    user_token$hashtags <- user_token$word
    user_token$word <- NULL
    
    if (input$word==""){
      x <- hashtag_top$word
    } else {
      x <- input$word
    }
    user_token <- user_token[user_token$hashtags %in% x,]
    
    #tokenizing tweets
    user_token <- tokenize(user_token,dtm=FALSE)
    user_token <- user_token %>% inner_join(bing) %>%
      group_by(word,bing) %>%
      count(word, bing, sort = TRUE) %>%
      ungroup()
    
    #plotting word count
    user_token <- user_token  %>%
      group_by(bing) %>%
      top_n(10) %>%
      ungroup()
    
    user_token$score <- user_token$n
    user_token[user_token$bing=="negative","score"] <- user_token[user_token$bing=="negative","score"]*(-1)  
    user_token %>% mutate(word = reorder(word, score)) %>%
      ggplot(aes(word, score, fill = bing)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      facet_wrap(~bing, scales = "free") +
      labs(y = NULL,
           x = NULL) +
      coord_flip() +
      ggtitle("Word Contributions to Top Hashtag Sentiment")})
  
  
  
  output$sentimentscore <- renderPlot({ 
    
    hashtag_sprint <- corpSentiment #creating copy of data
    hashtag_sprint$created_at <- as.Date(hashtag_sprint$created_at) #modifying date format
    hashtag_sprint$word <- gsub(".*sprint.*", "sprint", hashtag_sprint$word)  #grouping keywords under sprint
    
    hashtag_sprint <- hashtag_sprint %>% group_by(word,created_at) %>% transmute(afinn=mean(afinn[!is.na(afinn)]),
                                                                                 syuzhet=mean(syuzhet[!is.na(syuzhet)]),
                                                                                 engagement = mean(engagement)) %>% unique()
    
    corpSentimentAgg_plot <- hashtag_sprint[hashtag_sprint$word=="sprint" &
                                              !(is.na(hashtag_sprint$syuzhet)|is.na(hashtag_sprint$afinn)),]
    corpSentimentAgg_plot <- rbind(corpSentimentAgg_plot[c("word","created_at","afinn","engagement")],
                                   corpSentimentAgg_plot[c("word","created_at","syuzhet","engagement")])
    corpSentimentAgg_plot$label <- ifelse(is.na(corpSentimentAgg_plot$afinn),"syuzhet","afinn")
    corpSentimentAgg_plot$sentiment <- ifelse(is.na(corpSentimentAgg_plot$syuzhet),corpSentimentAgg_plot$afinn,corpSentimentAgg_plot$syuzhet)
    corpSentimentAgg_plot$afinn <- NULL
    corpSentimentAgg_plot$syuzhet <- NULL
    
    x <- corpSentimentAgg_plot %>% filter((as.Date(created_at) >= as.Date(input$date[1])) & (as.Date(created_at) <= as.Date(input$date[2])))
    ggplot(data = x, aes(x = created_at, y=sentiment)) +
      geom_line(aes(color=label,size = engagement)) +
      ggtitle("Average Sentiment Score for Sprint")})
  
  
  output$sentimentscore2 <- renderPlot({
    corp_n <- input$topN2
    
    hashtag_sprint <- corpSentiment #creating copy of data
    hashtag_sprint$created_at <- as.Date(hashtag_sprint$created_at) #modifying date format
    hashtag_sprint$word <- gsub(".*sprint.*", "sprint", hashtag_sprint$word)  #grouping keywords under sprint
    
    hashtag_sprint <- hashtag_sprint %>% group_by(word,created_at) %>% transmute(afinn=mean(afinn[!is.na(afinn)]),
                                                                                 syuzhet=mean(syuzhet[!is.na(syuzhet)]),
                                                                                 engagement = mean(engagement)) %>% unique()
    
    corpSentimentAgg_plot <- hashtag_sprint[hashtag_sprint$word=="sprint" &
                                              !(is.na(hashtag_sprint$syuzhet)|is.na(hashtag_sprint$afinn)),]
    corpSentimentAgg_plot <- rbind(corpSentimentAgg_plot[c("word","created_at","afinn","engagement")],
                                   corpSentimentAgg_plot[c("word","created_at","syuzhet","engagement")])
    corpSentimentAgg_plot$label <- ifelse(is.na(corpSentimentAgg_plot$afinn),"syuzhet","afinn")
    corpSentimentAgg_plot$sentiment <- ifelse(is.na(corpSentimentAgg_plot$syuzhet),corpSentimentAgg_plot$afinn,corpSentimentAgg_plot$syuzhet)
    corpSentimentAgg_plot$afinn <- NULL
    corpSentimentAgg_plot$syuzhet <- NULL
    
    hashtag_ctop <- corpSentiment %>% group_by(word) %>% transmute(count=n_distinct(status_id),
                                                                  afinn=mean(afinn[!is.na(afinn)]),
                                                                  syuzhet=mean(syuzhet[!is.na(syuzhet)]),
                                                                  bing_pos=n_distinct(status_id[bing=='positive']),
                                                                  bing_neg=n_distinct(status_id[bing=='negative']),
                                                                  engagement=mean(engagement)) %>% unique()
    
    hashtag_ctop <- hashtag_ctop[order(-hashtag_ctop$count),][1:corp_n,] 
    
    #filtering top hashtags
    hashtag_filt <- corpSentiment[corpSentiment$word %in% hashtag_ctop$word,c("status_id","created_at","word")]
    hashtag_filt$created_at <- as.Date(hashtag_filt$created_at)
    
    #getting averages per tweet (status_id)
    status_agg <- corpSentiment %>% group_by(status_id) %>% transmute(afinn=mean(afinn[!is.na(afinn)]),
                                                                     syuzhet=mean(syuzhet[!is.na(syuzhet)]),
                                                                     engagement=mean(engagement)) %>% unique()
    
    #filtering top hashtags to append tweet average info
    corpSentimentAgg <- inner_join(hashtag_filt,status_agg)
    corpSentimentAgg <- corpSentimentAgg %>% group_by(word,created_at) %>% transmute(
      status_count=n_distinct(status_id),
      afinn=mean(afinn[!is.na(afinn)]),
      syuzhet=mean(syuzhet[!is.na(syuzhet)]),
      engagement=mean(engagement)) %>% unique()
    ggplot(corpSentimentAgg, aes(x=afinn, y=syuzhet)) + 
      geom_point(aes(size=engagement,color=word)) + geom_smooth() +
      ggtitle("Sentiment Score for Top Hashtags x Engagement")})
  
  output$mapusa <- renderPlot({
    tweets_token <- tokenize(tweets_df,dtm = FALSE)
    
    #map downloaded from https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
    spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")
    
    #reformatting data
    spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
    spdf_format <- tidy(spdf, region = "google_name")
    
    #Calculate the centroid of each hexagon to add state label:
    centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))
    
    state_sentiment <-inner_join(tweets_token,afinn) 
    state_sentiment <- state_sentiment %>% group_by(status_id) %>% 
      transmute(sentiment=mean(afinn)) %>% unique()
    
    state_sentiment <- inner_join(tweets_df,state_sentiment) 
    state_sentiment <- state_sentiment %>% group_by(state) %>% 
      transmute(sentiment = mean(sentiment)) %>% unique()
    
    #adding avg sentiment to map data
    spdf_format <- spdf_format %>%
      left_join(. , state_sentiment, by=c("id"="state")) 
    
    if (input$state=="All"){
      x <- spdf_format
    } else {
      x <- spdf_format
      x$sentiment <- ifelse(spdf_format$id %in% input$state,spdf_format$sentiment,NA)
    }
    ggplot() +
      geom_polygon(data = x, aes( x = long, y = lat, group = group, fill=sentiment), color="black") +
      geom_text(data=centers, aes(x=x, y=y, label=id)) +
      scale_fill_viridis(option="magma", alpha=0.7) +
      theme_void() +
      ggtitle("Average Afinn Sentiment Score by State")})
  
  
  output$graph1 <- renderPlot({
    lda_df2 <- lda_df
    topics <- lda_df2[lda_df2$k == input$kslider,]
    topic_df <- as.data.frame(topics)
    topic_df$status_id <- rownames(topic_df)
    #unigrams: nrc
    grams_token <- left_join(tweets_token,topic_df)
    #TODO: change state input
    if (input$state=="All"){
      grams_token <- grams_token
    } else {
      grams_token <- grams_token[grams_token$state==input$state,]
    }
    
    nrc_counts_unigram <- grams_token %>%
      inner_join(nrc) %>%
      count(word, nrc, sort = TRUE) %>%
      ungroup()
    
    
    nrc_counts_unigram <- left_join(nrc_counts_unigram,grams_token[c("word","topic")]) %>% unique()
    nrc_counts_unigram <- nrc_counts_unigram[!is.na(nrc_counts_unigram$topic),]
    
    # data: edge list
    bing_dend <- nrc_counts_unigram %>% 
      top_n(100,wt=n)
    d11 <- data.frame(from="origin", to=paste(seq(1,4), sep=""),stringsAsFactors = F)
    d22 <- data.frame(unique(bing_dend[,c("topic","word")]),stringsAsFactors = F)
    colnames(d22) <- c("from","to")
    edges <- rbind(d11, d22)
    
    # We can add a second data frame with information for each node!
    name <- unique(c(as.character(edges$from), as.character(edges$to)))
    vertices <- data.frame(
      name=name,
      group="",
      cluster="",
      value=1
    )
    
    vertices$cluster <- ifelse(vertices$name %in% bing_dend$word,bing_dend$topic,NA)
    vertices$group <- ifelse(vertices$name %in% bing_dend$word,bing_dend$nrc,NA)
    vertices$value <- ifelse(vertices$name %in% bing_dend$word,bing_dend$n,NA)
    
    # Create a graph object
    mygraph <- graph_from_data_frame(edges, vertices=vertices)
    
    ggraph(mygraph, layout = 'dendrogram') + 
      geom_edge_diagonal() +
      geom_node_text(aes( label=name, filter=leaf, color=group) , angle=90 , hjust=1, nudge_y=-0.1) +
      geom_node_point(aes(filter=leaf, size=value, color=group) , alpha=0.6) +
      ylim(-.6, NA) +
      theme(legend.position="right") +
      guides(size=F) +
      ggtitle("Topic Dendogram, with NRC color code")
  })
  
}

shinyApp(ui, server)
