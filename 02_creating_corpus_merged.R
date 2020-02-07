
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


#------------------------------------Loading Tweets-----------------------------------#

#Loading tweets by FOLLOWERS OF SPRINT ACCOUNTS
followers <- get(load("followers.RData"))

#Loading tweets by SPRINT ACCOUNTS
corp_accounts <- get(load("corp_accounts.RData"))

#Loading general tweets mentioning SPRINT-RELATED KEYWORDS
tweets_df <- get(load("tweets_df.RData"))


#--------------------Functions for Cleaning and Preprocessing Tweets------------------#

#Creating function to clean text column, using a hybrid aproach
clean_text <- function(data,col,output) {
  ## removing urls
  data <- mutate(data, clean = gsub(x = col, pattern = "(s?)(f|ht)tp(s?)://\\S+\\b", replacement = ""))

  ## removing new lines
  data <- mutate(data, clean = gsub(x = clean, pattern = "[\r\n]", replacement = " "))
  
  ## removing hashtags and mentions
  data <- mutate(data, clean = gsub(x = clean, pattern = "#|@", replacement = " "))
  
  ## removing apostrophes separately to keep string as one word
  data <- mutate(data, clean = gsub(x = clean, pattern = "'|'", replacement = ""))
  
  ## removing punctuation manually to add a space and avoid merging words
  data <- mutate(data, clean = gsub(x = clean, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = " "))
  
  ## removing emojis
  data$clean <- sapply(data$clean,function(row) iconv(row, "latin1", "ASCII", sub=" "))
  
  ## final column name
  names(data)[names(data) == 'clean'] <- output
    
  return(data)
}


#Creating function for creating a corpus, to be used after cleaning function
tw_corpus <- function(data, col){
  
  ## build a corpus, and specify the source to be character vectors
  myCorpus <- Corpus(VectorSource(col))
  
  ## convert to lower case
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  
  ## removing numbers as they lose meaning
  myCorpus <- tm_map(myCorpus, removeNumbers)
  
  ## remove stopwords from corpus
  stoppers <- stopwords("english")
  stoppers <- append(stoppers,c("https","com","can","http"))
  myCorpus <- tm_map(myCorpus, removeWords, stoppers)
  
  ## adding metadata, suppressing warning in case columns are not available
  suppressWarnings(meta(myCorpus, 'author') <- data$screen_name)
  suppressWarnings(meta(myCorpus, 'datetimestamp') <- data$created_at)
  suppressWarnings(meta(myCorpus, 'description') <- data$description)
  suppressWarnings(meta(myCorpus, 'origin') <- data$source)
  suppressWarnings(meta(myCorpus, 'location') <- data$location)
  
  return(myCorpus)
}


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


#-----------------------Creating a Corpus for each set of tweets----------------------#

## Applying cleaning and corpus functions for each dataframe
followers <- clean_text(followers,followers$tweet,"clean_tweet")
followers_corpus <- tw_corpus(followers, followers$clean_tweet)

corp_accounts <- clean_text(corp_accounts,corp_accounts$tweet,"clean_tweet")
corp_accounts_corpus <- tw_corpus(corp_accounts, corp_accounts$clean_tweet)

tweets_df <- clean_text(tweets_df, tweets_df$tweet,"clean_tweet")
tweets_df_corpus <- tw_corpus(tweets_df, tweets_df$clean_tweet)

#Creating df with tweets excluding tweets by Sprint accounts
handles <- c("sprint","sprintcare","sprintnews")
user_tweets <- tweets_df[!tweets_df$screen_name %in% handles,] 

#Creating dataframe with mentions of COMPETITOR BRANDS for later use
competition <- c("verizon","tmobile","ATT","xfinity","comcast")
competition_df <- c()
for (i in competition){
  #getting tweets mentioning the competition
  prep_df <- user_tweets[grep(i, user_tweets$mentions_screen_name),] 
  competition_df <- rbind(competition_df, prep_df)
}

#Creating corpus for each new dataframe, tweets were cleaned in original tweets_df
user_tweets_corpus <- tw_corpus(user_tweets, user_tweets$clean_tweet)
competition_corpus <- tw_corpus(competition_df, competition_df$clean_tweet)


#----------------------------------------TAB:1----------------------------------------#
#------------Topic Modeling for Sprint keywords, excluding company accounts-----------#

#TOPIC MODELING (tweets with keywords)
#tokenizing tweets with additional custom stopwords
tweetsTokenized <- tokenize(user_tweets,custom=c('sprint','can','get','t','amp','s','im','cant'))

#LDA model, VEM method was also tested, results as very close
#TODO: make k flexible with input 
k <- 4
tweets_lda <- LDA(tweetsTokenized, k = k,method="gibbs")

#getting first 3 words in topic
term <- terms(tweets_lda, 3) 
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
topic <- topics(tweets_lda, 1)

#preparing data frame to plot topics, filtering tweets not in docs to use date as axis
docs <- names(topic)
tw_x <- user_tweets[user_tweets$status_id %in% docs, "created_at"]
topics <- data.frame(date=as.IDate(tw_x$created_at), topic) 

#TODO: plot to be added in shiny
# Plot
# ggplot(data = topics, 
#        aes(x = date, y = term[topic], color=term[topic], fill=term[topic])) +
#   geom_density_ridges(alpha = 0.8, scale = 5) +
#   scale_fill_viridis(option = "A", discrete = TRUE, direction=-1) +
#   scale_color_viridis(option = "A", discrete = TRUE, direction=-1) + 
#   theme_few() +
#   labs(y=NULL,x=NULL, fill='Topics') +
#   guides(colour=FALSE) +
#   ggtitle("Topic Density Distribution in Time")

#----------Creating a profile for Sprint tweets (excludes Sprint accounts)------------#

#FOLLOWERS PROFILE (around top 100 followers by amount of tweets)
#creating topic df to add topic to user status
topic_df <- as.data.frame(topic)
topic_df$status_id <- names(topic)
user_tweets <- left_join(user_tweets,topic_df)

#TODO: filter user_tweets$topic with shiny input, same as previous plot (tab 1)
user_filt <- seq(1,k)

#cleaning description text
user_tweets_filt <- user_tweets[user_tweets$topic %in% user_filt,]

user_tweets_filt <- clean_text(user_tweets_filt,user_tweets_filt$description,"clean_bio")

#creating df with unique rows of user info
user_info <- user_tweets_filt %>% group_by(user_id,screen_name,clean_bio,location,
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
profile <- unique(profile) #add account filter

#TODO: wordcloud to be added in shiny
#wordcloud of followers' descriptions
# bio <- unique(user_tweets_filt[,c("screen_name","description","location")])
# tweets_buzz <- clean_text(bio,bio$description,"clean_desc")
# tweets_buzz <- tw_corpus(tweets_buzz, tweets_buzz$clean_desc)
# wordcloud(tweets_buzz,colors=brewer.pal(5, "Dark2"),max.words = 250,
#           random.color = TRUE, scale=c(4,.3))

#----------------------------------------TAB:2----------------------------------------#
#----------Sentiment Analysis for Sprint keywords, excluding company accounts---------#
#HASHTAG ANALYSIS (from regular user tweets)

#creating potential engagement formula: using account data
user_tweets$engagement <- (user_tweets$followers_count/user_tweets$statuses_count) +
                            (user_tweets$favourites_count/user_tweets$statuses_count) +
                              (user_tweets$retweet_count/user_tweets$statuses_count)
  
#getting relevant columns and cleaning hashtag column
hashtag_df <- user_tweets[,c("status_id","hashtags","created_at","clean_tweet","engagement")]
hashtag_df$hashtags <- ifelse(hashtag_df$hashtags=="NA","",hashtag_df$hashtags)

#tokenizing tweet column to get sentiments
hashtags <- tokenize(hashtag_df,dtm=FALSE)#,custom=c('can','get','t','e','s'))

#loading dictionaries
afinn <- get_sentiments(lexicon = "afinn") %>% rename("afinn"="value")
bing <- get_sentiments(lexicon = "bing") %>% rename("bing"="sentiment")
loughran <- get_sentiments(lexicon = "loughran") %>% rename("loughran"="sentiment")
nrc <- get_sentiments(lexicon = "nrc") %>% rename("nrc"="sentiment")
syuzhet <- get_sentiment_dictionary() %>% rename("syuzhet"="value")

#adding tweet sentiment columns
tweetSentiment <- left_join(hashtags,afinn)
tweetSentiment <- left_join(tweetSentiment,bing)
tweetSentiment <- left_join(tweetSentiment,loughran)
tweetSentiment <- left_join(tweetSentiment,nrc)
tweetSentiment <- left_join(tweetSentiment,syuzhet)

#getting distinct hashtags: to see what sentiments are associated with top hashtags
hashtag_dist <- tweetSentiment
hashtag_dist$clean_tweet <- hashtag_dist$hashtags
hashtag_dist$hashtags<- NULL
hashtag_dist <- tokenize(hashtag_dist,dtm=FALSE)

#top5 hashtags and some sentiment information
hashtag_top <- hashtag_dist %>% group_by(word) %>% transmute(count=n_distinct(status_id),
                                                             afinn=mean(afinn[!is.na(afinn)]),
                                                             syuzhet=mean(syuzhet[!is.na(syuzhet)]),
                                                             bing_pos=n_distinct(status_id[bing=='positive']),
                                                             bing_neg=n_distinct(status_id[bing=='negative'])) %>% unique()

#TODO: create input to filter top x, used instead of 5 
ht_n <- 5
hashtag_top <- hashtag_top[order(-hashtag_top$count),][1:ht_n,] 

#preparing plot data
hashtag_plot <- rbind(hashtag_top[c("word","bing_pos")],hashtag_top[c("word","bing_neg")])
hashtag_plot$label <- ifelse(is.na(hashtag_plot$bing_pos),"negative","positive")
hashtag_plot$bing <- ifelse(is.na(hashtag_plot$bing_pos),hashtag_plot$bing_neg,hashtag_plot$bing_pos)
hashtag_plot$bing_pos <- NULL
hashtag_plot$bing_neg <- NULL

#TODO: plot to be added in shiny
#bar chart: bing dictionary
# ggplot(hashtag_plot, aes(word)) + 
#   geom_bar(aes(weight = bing, fill=label) ,position = "dodge") +
#   coord_flip() +
#   ggtitle("Most Popular Hashtags With Bing Sentiment Dictionary")

#looking at sentiments in more detail: bing and nrc: positive/negative in more detail
hashtag_jitter <- hashtag_dist[!(is.na(hashtag_dist$bing) | is.na(hashtag_dist$nrc)),
                                c("word","nrc","bing","engagement")]
hashtag_jitter <- hashtag_jitter[hashtag_jitter$word %in% hashtag_top$word,]
#too many words lost if filtered:# hashtag_jitter <- hashtag_jitter[!(hashtag_jitter$nrc %in% c("negative","positive")),]

#TODO: plot to be added in shiny
# ggplot(hashtag_jitter, aes(bing,nrc)) +
#   geom_jitter(aes(color = word, alpha=0.8, size = engagement)) +
#   ggtitle("Most Popular Hashtags, NRC vs Bing Classifications")

#Looking at word contributions to sentiment
#adding hashtag information
user_token <- left_join(user_tweets,hashtag_dist[!is.na(hashtag_dist$word),c("status_id","word")]) 
user_token$hashtags <- user_token$word
user_token$word <- NULL

#filtering by top n hashtags
#TODO: possibility to select specific hashtag instead of hashtag_top$word
user_token <- user_token[user_token$hashtags %in% hashtag_top$word,]

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

# user_token %>% mutate(word = reorder(word, score)) %>%
# ggplot(aes(word, score, fill = bing)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   labs(y = NULL,
#        x = NULL) +
#   coord_flip() +
#   ggtitle("Word Contributions to Top Hashtag Sentiment")

#----------------------------------------TAB:3----------------------------------------#

#COMPANY TWEET ANALYSIS (from Sprint account tweets)
token_prep <- corp_accounts[,c("status_id","hashtags","retweet_count","created_at","clean_tweet")]
token_prep$hashtags <- ifelse(token_prep$hashtags=="NA","",token_prep$hashtags)

#tokenizing corporate tweets
corp_token <- tokenize(token_prep,dtm=FALSE,custom = c("s","us","can","g"))
corpSentiment <- left_join(corp_token,afinn)
corpSentiment <- left_join(corpSentiment,bing)
corpSentiment <- left_join(corpSentiment,loughran)
corpSentiment <- left_join(corpSentiment,nrc)
corpSentiment <- left_join(corpSentiment,syuzhet)

#hashtags
hashtag_corp <- corpSentiment
hashtag_corp$clean_tweet <- hashtag_corp$hashtags
hashtag_corp$hashtags<- NULL
hashtag_corp <- tokenize(hashtag_corp,dtm=FALSE)

#preparing line plot data
hashtag_sprint <- hashtag_corp #creating copy of data
hashtag_sprint$created_at <- as.Date(hashtag_sprint$created_at) #modifying date format
hashtag_sprint$word <- gsub(".*sprint.*", "sprint", hashtag_sprint$word)  #grouping keywords under sprint

hashtag_sprint <- hashtag_sprint %>% group_by(word,created_at) %>% transmute(afinn=mean(afinn[!is.na(afinn)]),
                                              syuzhet=mean(syuzhet[!is.na(syuzhet)]),
                                              engagement = mean(retweet_count)) %>% unique()

corpSentimentAgg_plot <- hashtag_sprint[hashtag_sprint$word=="sprint" &
                                        !(is.na(hashtag_sprint$syuzhet)|is.na(hashtag_sprint$afinn)),]
corpSentimentAgg_plot <- rbind(corpSentimentAgg_plot[c("word","created_at","afinn","engagement")],
                               corpSentimentAgg_plot[c("word","created_at","syuzhet","engagement")])
corpSentimentAgg_plot$label <- ifelse(is.na(corpSentimentAgg_plot$afinn),"syuzhet","afinn")
corpSentimentAgg_plot$sentiment <- ifelse(is.na(corpSentimentAgg_plot$syuzhet),corpSentimentAgg_plot$afinn,corpSentimentAgg_plot$syuzhet)
corpSentimentAgg_plot$afinn <- NULL
corpSentimentAgg_plot$syuzhet <- NULL

#plotting line
#TODO: filter by date (corpSentimentAgg_plot$created_at)
# corpSentimentAgg_plot %>% 
# ggplot(aes(x = created_at, y=sentiment)) +
#   geom_line(aes(color=label),size=0.8) +
#   ggtitle("Average Sentiment Score for Sprint")

#grouping hashtags for sentiment information
hashtag_ctop <- hashtag_corp %>% group_by(word) %>% transmute(count=n_distinct(status_id),
                                                              afinn=mean(afinn[!is.na(afinn)]),
                                                              syuzhet=mean(syuzhet[!is.na(syuzhet)]),
                                                              bing_pos=n_distinct(status_id[bing=='positive']),
                                                              bing_neg=n_distinct(status_id[bing=='negative'])) %>% unique()

#TODO: create input to filter top x, used instead of 5
corp_n <- 5
hashtag_ctop <- hashtag_ctop[order(-hashtag_ctop$count),][1:corp_n,] 

#filtering top hashtags
hashtag_filt <- hashtag_corp[hashtag_corp$word %in% hashtag_ctop$word,c("status_id","created_at","word","engagement")]
hashtag_filt$created_at <- as.Date(hashtag_filt$created_at)

#getting averages per tweet (status_id)
status_agg <- hashtag_corp %>% group_by(status_id) %>% transmute(afinn=mean(afinn[!is.na(afinn)]),
                                                                  syuzhet=mean(syuzhet[!is.na(syuzhet)]),
                                                                  engagement=mean(engagement)) %>% unique()

#filtering top hashtags to append tweet average info
corpSentimentAgg <- inner_join(hashtag_filt,status_agg)
corpSentimentAgg <- corpSentimentAgg %>% group_by(word,created_at) %>% transmute(
                                                   status_count=n_distinct(status_id),
                                                   afinn=mean(afinn[!is.na(afinn)]),
                                                   syuzhet=mean(syuzhet[!is.na(syuzhet)]),
                                                   retweets=mean(engagement)) %>% unique()


#TODO: add plot to shiny
#engagement
# ggplot(corpSentimentAgg, aes(x=afinn, y=syuzhet)) + 
#   geom_point(aes(size=retweets,color=word)) + geom_smooth() +
#   ggtitle("Sentiment Score for Top Hashtags x Retweets")


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

#adding avg sentiment to map data
spdf_format <- spdf_format %>%
  left_join(. , state_sentiment, by=c("id"="state")) 

#TODO: filter by state: state_sentiment$state
#TODO: add plot to shiny
# ggplot() +
#   geom_polygon(data = spdf_format, aes( x = long, y = lat, group = group, fill=sentiment), color="white") +
#   geom_text(data=centers, aes(x=x, y=y, label=id)) +
#   scale_fill_viridis(option="magma", alpha=0.7) +
#   theme_void() +
#   ggtitle("Average Afinn Sentiment Score by State")


#unigrams: nrc
grams_token <- left_join(tweets_token,topic_df)
#TODO: change state input
grams_token <- grams_token[grams_token$state=="STATE",]
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
# 
# ggraph(mygraph, layout = 'dendrogram') + 
#   geom_edge_diagonal() +
#   geom_node_text(aes( label=name, filter=leaf, color=group) , angle=90 , hjust=1, nudge_y=-0.1) +
#   geom_node_point(aes(filter=leaf, size=value, color=group) , alpha=0.6) +
#   ylim(-.6, NA) +
#   theme(legend.position="right") +
#   guides(size=F) +
#   ggtitle("Topic Dendogram, with NRC color code")

#frequency table for wordclouds
follow_buzz <- clean_text(tweets_df,tweets_df$tweet,"clean_desc")
follow_buzz <- tw_corpus(follow_buzz, follow_buzz$clean_desc)
tdm<-TermDocumentMatrix(follow_buzz)
tdmatrix<-as.matrix(tdm)
wordfreq<-sort(rowSums(tdmatrix), decreasing = TRUE)
freqtable <- as.data.frame(wordfreq)
freqtable$word <- row.names(freqtable)
rownames(freqtable)<- NULL

#-------------------------SHINY APP
ui <- dashboardPage( skin = "yellow",
                     dashboardHeader(title = "Sprint Twitter Analytics Dashboard"),
                     #creation of the sidebar menu   
                     dashboardSidebar(sidebarMenu(
                       menuItem("Topic Modelling", tabName = "TOPIC_MODELLING1", icon = icon("chart-line", lib = "font-awesome")),
                       menuItem("Sentiment Analysis", tabName = "Sentiment_Analysis2", icon = icon("smile", lib = "font-awesome")),
                       menuItem("Sentiment By Engagement", tabName = "Word_Correlation3", icon = icon("caret-right", lib = "font-awesome")),
                       menuItem("Map Of The USA", tabName = "USA", icon = icon("flag", lib = "font-awesome")),
                       menuItem("Token Frequency", tabName = "Token_Frequency", icon = icon("dashboard")))),
                     
                     
                     dashboardBody(
                       tabItems(
                         tabItem(tabName = "Token_Frequency",fluidRow(
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
                             infoBox("Number of users",round(profile$`N Users`), icon = icon("user-friends", lib = "font-awesome")),
                             infoBox("Avg number of tweets",round(profile$`Avg Tweets`), icon = icon("twitter", lib = "font-awesome")),
                             infoBox("Avg account age",round(profile$`Account Age`), icon = icon("birthday-cake", lib = "font-awesome"))),
                           fluidRow(
                             box(plotOutput("LDA",width = "100%"), width= "100%")),
                           fluidRow(
                             box(plotOutput("wordcloud2",width = "100%"), width= "100%")))
                       )
                     )
)


server <- function(input, output) {
  
  output$wordcloud1 <- renderPlot({
    
    wordcloud(words = freqtable$word, freq = freqtable$wordfreq, min.freq = 1,
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
  
  dataInput <- reactive({
    tweets_lda <- LDA(tweetsTokenized, k = input$kslider,method="gibbs")
  })
  
  output$LDA <- renderPlot({
    #TOPIC MODELING (tweets with keywords)
    #tokenizing tweets with additional custom stopwords
    tweetsTokenized <- tokenize(user_tweets,custom=c('sprint','can','get','t','amp','s','im','cant'))
    
    #LDA model, VEM method was also tested, results as very close
    #TODO: make k flexible with input 
    
    
    #getting first 3 words in topic
    term <- terms(dataInput(), 3) 
    term <- apply(term, MARGIN = 2, paste, collapse = ", ")
    topic <- topics(dataInput(), 1)
    
    #preparing data frame to plot topics, filtering tweets not in docs to use date as axis
    docs <- names(topic)
    tw_x <- user_tweets[user_tweets$status_id %in% docs, "created_at"]
    topics <- data.frame(date=as.IDate(tw_x$created_at), topic) 
    
    ggplot(data = topics, 
           aes(x = date, y = term[topic], color=term[topic], fill=term[topic])) +
      geom_density_ridges(alpha = 0.8, scale = 5) +
      scale_fill_viridis(option = "A", discrete = TRUE, direction=-1) +
      scale_color_viridis(option = "A", discrete = TRUE, direction=-1) + 
      theme_few() +
      labs(y=NULL,x=NULL, fill='Topics') +
      guides(colour=FALSE) +
      ggtitle("Topic Density Distribution in Time")
  })
  
  output$table <- renderTable({
    user_filt <- seq(1,input$kslider)
    
    #cleaning description text
    
    user_tweets_filt <- user_tweets[user_tweets$topic %in% user_filt,]
    
    user_tweets_filt <- clean_text(user_tweets_filt,user_tweets_filt$description,"clean_bio")
    
    #creating df with unique rows of user info
    user_info <- user_tweets_filt %>% group_by(user_id,screen_name,clean_bio,location,
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
    profile})
  
  
  output$hashtag <- renderPlot({
    ht_n <- input$topN
    hashtag_top <- hashtag_top[order(-hashtag_top$count),][1:ht_n,] 
    
    #preparing plot data
    hashtag_plot <- rbind(hashtag_top[c("word","bing_pos")],hashtag_top[c("word","bing_neg")])
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
    hashtag_jitter <- hashtag_dist[!(is.na(hashtag_dist$bing) | is.na(hashtag_dist$nrc)),
                                   c("word","nrc","bing","engagement")]
    hashtag_jitter <- hashtag_jitter[hashtag_jitter$word %in% hashtag_top$word,]
    
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
    
    x <- corpSentimentAgg_plot %>% filter((as.Date(created_at) >= as.Date(input$date[1])) & (as.Date(created_at) <= as.Date(input$date[2])))
    ggplot(data = x, aes(x = created_at, y=sentiment)) +
      geom_line(aes(color=label,size = engagement)) +
      ggtitle("Average Sentiment Score for Sprint")})
  
  
  output$sentimentscore2 <- renderPlot({
    corp_n <- input$topN2
    hashtag_ctop <- hashtag_ctop[order(-hashtag_ctop$count),][1:corp_n,] 
    
    #filtering top hashtags
    hashtag_filt <- hashtag_corp[hashtag_corp$word %in% hashtag_ctop$word,c("status_id","created_at","word")]
    hashtag_filt$created_at <- as.Date(hashtag_filt$created_at)
    
    #getting averages per tweet (status_id)
    status_agg <- hashtag_corp %>% group_by(status_id) %>% transmute(afinn=mean(afinn[!is.na(afinn)]),
                                                                     syuzhet=mean(syuzhet[!is.na(syuzhet)]),
                                                                     engagement=mean(retweet_count)) %>% unique()
    
    #filtering top hashtags to append tweet average info
    corpSentimentAgg <- inner_join(hashtag_filt,status_agg)
    corpSentimentAgg <- corpSentimentAgg %>% group_by(word,created_at) %>% transmute(
      status_count=n_distinct(status_id),
      afinn=mean(afinn[!is.na(afinn)]),
      syuzhet=mean(syuzhet[!is.na(syuzhet)]),
      retweets=mean(engagement)) %>% unique()
    ggplot(corpSentimentAgg, aes(x=afinn, y=syuzhet)) + 
      geom_point(aes(size=retweets,color=word)) + geom_smooth() +
      ggtitle("Sentiment Score for Top Hashtags x Retweets")})
  
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
