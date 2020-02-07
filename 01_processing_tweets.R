#--------------------------------Social Media Analytics-------------------------------#
####                                                                               ####
####   This file reads is used to download tweets and save them as RData files     ####
####  Team members: Stephanie Beyer Diaz, Manoj Purushothaman, Edward Vrighjem     ####
####                                 Company: Sprint                               ####
####                                                                               ####
#-------------------------------------------------------------------------------------#

for (c in c('tidytext','dplyr','rtweet','jsonlite','base64enc','httr')){
  if (!require(c, character.only=TRUE)) install.packages(c, repos = "http://cran.us.r-project.org")
  require(c, character.only=TRUE)
}

source("tokens.R")
twitter_token <- create_token(
  app = appname,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret,
  set_renv=FALSE)

handles <- c("@Sprint","@sprintcare","@sprintnews","@sprintbusiness")

term_list <- c("#MySprintRewards",
              "sprint.com",
              "Sprint Store",#%20
              "#SprintSizzlingWeek",#%23
              "Sprint phone",
              "Sprint account",
              "Sprint mobile",
              "Sprint Corp",
              "@Sprint",#%40
              "@sprintcare",
              "@sprintnews",
              "@sprintbusiness")


#creating function to access tweets with 3 modalities: 
## search="user" to get a user's timeline
## search="followers" to get a user's top (default 20) followers by total amount of tweets
## search="string" to get all tweets containing specific keyword(s)
## keyword allows for single string or a vector of strings for all search modalities
get_tweets <- function(days=90, top=20, n=1000, keyword, search){
              
              fromDate <- format(Sys.time() - 60 * 60 * 24 * days, "%Y%m%d%0000")
              toDate <- format(Sys.time(), "%Y%m%d%0000")
              
              keyword <- c(keyword)
              
              if (search=="string"){
                rtweets<-c()
                for (i in keyword){
                  tryCatch( 
                    #attempting to run rtweet function
                      {suppressWarnings(term <- search_fullarchive(q = i,
                                                 n=n,
                                                  fromDate = fromDate,
                                                  env_name="fullarchive"
                                                    ))},
                    #else running manual tweet download
                      finally = {
                        # modifying search term for search url
                        i <- gsub("@","%40",gsub("#","%23",gsub("\\s","%20",i)))
                        
                        # base64 encoding
                        kands <- paste(consumer_key, consumer_secret, sep=":")
                        base64kands <- base64encode(charToRaw(kands))
                        base64kandsb <- paste("Basic", base64kands, sep=" ")
                        
                        # request bearer token
                        resToken <- POST(url = "https://api.twitter.com/oauth2/token",
                                         add_headers("Authorization" = base64kandsb, "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"),
                                         body = "grant_type=client_credentials")
                        
                        # get bearer token
                        bearer <- content(resToken)
                        bearerToken <- bearer[["access_token"]]
                        bearerTokenb <- paste("Bearer", bearerToken, sep=" ")
                        
                        url_string <- paste0("https://api.twitter.com/1.1/tweets/search/fullarchive/",env_name,".json?query=",i,"&maxResults=100&fromDate=",fromDate,"&toDate=",toDate)
                        
                        resTweets <- GET(url = url_string,
                                         add_headers("authorization" = bearerTokenb))
                        term <- fromJSON(httr::content(resTweets, "text", encoding="latin1"),flatten = TRUE) %>% data.frame()
                        
                        colnames(term) <- gsub(x = colnames(term), pattern = "[[:punct:]]", replacement = "_")
                        
                        term <- term[,c("results_user_id_str","results_id_str","results_created_at","results_user_screen_name",
                                        "results_user_favourites_count","results_reply_count","results_retweet_count","results_retweeted",
                                        "results_lang","results_source","results_entities_user_mentions","results_entities_hashtags",#results_entities: lists
                                        "results_user_location","results_user_description","results_user_followers_count","results_user_friends_count",
                                        "results_user_statuses_count","results_user_created_at","results_extended_tweet_full_text")]
                        
                        colnames(term) <- c("user_id","status_id","created_at","screen_name","favourites_count","reply_count",
                                          "retweet_count","is_retweet","lang","source","mentions_screen_name",
                                          "hashtags","location","description","followers_count","friends_count",
                                          "statuses_count","account_created_at","text")
                        #formatting dates
                        format.str <- "%a %b %d %H:%M:%S %z %Y"
                        
                        rtweets$created_at <- as.POSIXct(strptime(rtweets$created_at, format.str, tz = "GMT"), tz = "GMT")
                        rtweets$account_created_at <- as.POSIXct(strptime(rtweets$account_created_at, format.str, tz = "GMT"), tz = "GMT")
                        
                        })
                            
                      rtweets <- rbind(rtweets,term)
                      
                }
                
                #deleting urls to then filter by text to drop duplicate tweets
                #as most are from bots or shared posts from external platforms like LinkedIn
                      rtweets <- mutate(rtweets, tweet = gsub(x = text, pattern = "(s?)(f|ht)tp(s?)://\\S+\\b", replacement = ""))
                      rtweets <- rtweets[!duplicated(rtweets$tweet),]
                      
                #dropping retweets (using flag and substring) to only have original tweets
                      rtweets <- rtweets[rtweets$is_retweet==FALSE,]
                      rtweets <- rtweets[substring(rtweets$tweet,1,2)!='RT',]
                      
                      output <- rtweets
                }
              
              else if (search=="user"){
                tl_append <- c()
              
               for (j in keyword){
                tl <- get_timeline(user=j, n=n)
                tl_append <- rbind(tl_append,tl)
               }
                tl_append <- mutate(tl_append, tweet = gsub(x = text, pattern = "(s?)(f|ht)tp(s?)://\\S+\\b", replacement = ""))
                tl_append <- tl_append[!duplicated(tl_append$tweet),]
                output <- tl_append
              }
              
              else if (search=="followers"){
                followers <- c()
                for (g in keyword){
                followers <- rbind(get_followers(g))
                }
                followers <- unique(followers)
                followers_info <- lapply(followers,lookup_users)$user_id
              
              #ordering by amount of tweets to have most active users at the top
                followers_info <- followers_info[order(-followers_info$statuses_count ),]
              
              #dropping duplicate tweets by text, done to minimize chances of getting only bots
                followers_info <- mutate(followers_info, tweet = gsub(x = text, pattern = "(s?)(f|ht)tp(s?)://\\S+\\b", replacement = ""))
                followers_info <- followers_info[!duplicated(followers_info$tweet),]
                
                top_followers <- followers_info[1:top,"screen_name"]
              
              #getting last n tweets of top_followers
                for (h in top_followers){
                  topf_tweets <- get_timeline(user=h, n=n)
                }
              
              #tweet urls cleaned, to keep a consistent output but no duplicates dropped
              #this time, as it may be interesting to see how many users fall for spam apps/links
                topf_tweets <- mutate(topf_tweets, tweet = gsub(x = text, pattern = "(s?)(f|ht)tp(s?)://\\S+\\b", replacement = ""))
                output <- topf_tweets
              }
              
              else {
                print("search parameter must be 'user', 'followers' or 'string'")
                output<-NULL
              }
              
              #dropping rows with no tweets, as they may be protected accounts
              output <- output[!is.na(output$text),]
              
              #dropping columns that will not be used
              output <- output[,c("user_id","status_id","created_at","screen_name","favourites_count","reply_count",
                           "retweet_count","is_retweet","lang","source","mentions_screen_name",
                           "hashtags","location","description","followers_count","friends_count",
                           "statuses_count","account_created_at","tweet")]
              
              
              return(output)
}

#running function to get followers of company accounts
followers <- get_tweets(keyword=handles, search="followers", n=100)

#running function to get tweets by company accounts
corp_accounts <- get_tweets(keyword=handles, search="user")

#running function to get tweets mentioning Sprint-related keywords
tweets_df <- get_tweets(keyword=term_list, search="string")

#removing new lines and punctuation from tweets and descriptions,
#collapsing lists (hashtags and mentions) to save data
prepare_csv <- function(dataframe) {
  
  ## removing new lines
  dataframe <- mutate(dataframe, tweet = gsub(x = tweet, pattern = "[\r\n]", replacement = " "))
  dataframe <- mutate(dataframe, description = gsub(x = description, pattern = "[\r\n]", replacement = " "))
  
  ## removing punctuation manually to add a space and avoid merging words
  dataframe <- mutate(dataframe, tweet = gsub(x = tweet, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = " "))
  dataframe <- mutate(dataframe, description = gsub(x = description, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = " "))
  
  ## collapsing lists
  dataframe$hashtags <- vapply(dataframe$hashtags, paste, collapse = ", ", character(1L))
  dataframe$hashtags <- gsub('\"', "", dataframe$hashtags, fixed = TRUE)
  dataframe$hashtags <- gsub("c(", "",dataframe$hashtags,fixed=TRUE)
  dataframe$hashtags <- gsub(")", "",dataframe$hashtags,fixed=TRUE)
  dataframe$hashtags <- gsub(",.list.*", "",dataframe$hashtags)
  
  dataframe$mentions_screen_name <- vapply(dataframe$mentions_screen_name, paste, collapse = ", ", character(1L))
  dataframe$mentions_screen_name <- gsub('\"', "", dataframe$mentions_screen_name, fixed = TRUE)
  dataframe$mentions_screen_name <- gsub(",.list.*", "",dataframe$mentions_screen_name)
  dataframe$mentions_screen_name <- gsub("c(", "",dataframe$mentions_screen_name,fixed=TRUE)
  dataframe$mentions_screen_name <- gsub(")", "",dataframe$mentions_screen_name,fixed=TRUE)
  
  ## returning dataframe
  return(dataframe)
}
followers <- prepare_csv(followers)
corp_accounts <- prepare_csv(corp_accounts)
tweets_df <- prepare_csv(tweets_df)

#cleaning location data by using external data from https://simplemaps.com/data/us-cities
uscities <- read.csv("uscities.csv", header = TRUE)

#simple string searches to add state column
tweets_df$state <- ifelse(trimws(tweets_df$location) %in% uscities$city_ascii,levels(uscities$state_name),
                   ifelse(trimws(tweets_df$location) %in% uscities$state_name,levels(uscities$state_name),
                    ifelse(trimws(tweets_df$location) %in% uscities$state_id,levels(uscities$state_name),
                     ifelse(trimws(tweets_df$location) %in% paste0(uscities$city_ascii,", ",uscities$statstate_namee_id),levels(uscities$state_name),
                      ifelse(trimws(tweets_df$location) %in% paste0(uscities$city_ascii," ",uscities$statstate_namee_id),levels(uscities$state_name),
          ifelse(sub('.*,\\s*', '', tweets_df$location) %in% uscities$city_ascii,levels(uscities$state_name),
            ifelse(sub('.*,\\s*', '', tweets_df$location) %in% uscities$state_id,levels(uscities$state_name),
              ifelse(sub('.*,\\s*', '', tweets_df$location) %in% uscities$state_name,levels(uscities$state_name),
            ifelse(gsub(",.*$", "", tweets_df$location) %in% uscities$city_ascii,levels(uscities$state_name),
             ifelse(gsub(",.*$", "", tweets_df$location) %in% uscities$state_name,levels(uscities$state_name),
                    ifelse(uscities$city_ascii %in% tweets_df$location,levels(uscities$state_name),
                           ifelse(uscities$state_id %in% toupper(tweets_df$location),levels(uscities$state_name),
                                  ifelse(toupper(uscities$state_name) %in% toupper(tweets_df$location),levels(uscities$state_name),NA)))))))))))))

#saving dataframes in case more tweets are downloaded later
save(followers,file="followers.RData")
save(corp_accounts,file="corp_accounts.RData")
save(tweets_df,file="tweets_df.RData")

#saving dictionaries
afinn <- get_sentiments(lexicon = "afinn") %>% rename("afinn"="value")
bing <- get_sentiments(lexicon = "bing") %>% rename("bing"="sentiment")
loughran <- get_sentiments(lexicon = "loughran") %>% rename("loughran"="sentiment")
nrc <- get_sentiments(lexicon = "nrc") %>% rename("nrc"="sentiment")
syuzhet <- get_sentiment_dictionary() %>% rename("syuzhet"="value")

save(afinn,file="afinn.RData")
save(bing,file="bing.RData")
save(loughran,file="loughran.RData")
save(nrc,file="nrc.RData")
save(syuzhet,file="syuzhet.RData")
