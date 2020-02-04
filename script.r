################################################################################
#//////////////////////////////////////////////////////////////////////////////#
#    ///// SETTING UP SCRIPT //////////////////////////////////////////////////#
#//////////////////////////////////////////////////////////////////////////////#
################################################################################


########################################
# INSTALL PACKAGES & LOAD LIBRARIES    #
########################################

## Install packages (if necessary) to your R *(uncomment these lines to run)
#install.packages("rtweet")
#install.packages("tidyverse")

## Load libraries
library(rtweet)
library(tidyverse)

## Quick overviews for getting started with rtweet *(uncomment these lines to run)
#vignette("auth", package = "rtweet")
#vignette("intro", package = "rtweet")


###################################################
# CHECK AND SET WORKING DIRECTORY *(IF NECESSARY) #
###################################################

## Working directory should be the home level ("./") folder for the app and end with: twitter-account-visualizer-app/

## Check that your path is correct and ends in "/twitter-account-visualizer-app" *(uncomment these lines to run)
#getwd()

## If path incorrect, run setwd("YOUR/PATH/TO/THE/APP/HERE") to set your path correctly *(uncomment these lines to run)
#setwd("")


########################################
# READ & LOAD ENVIRONMENT VARIABLES    #
########################################

## First must type in your API details from Twitter into the .Renviron file
## Otherwise readRenviron will only read empty values

## Read this .Renviron file for rtweet API details *(Uncomment these lines to run)
## Remember to first input your rtweet API details
#readRenviron(".Renviron")

## *(Keep commented)
## Line is commented because this .Renviron file doesn't exist in the repo (it was "gitignored")
## .Renviron_master was used for production version of app hosted on https://www.shinyapps.io/
readRenviron(".Renviron_master")

## Load rtweet API details from .Renviron into global environment
api_key <- Sys.getenv("API_KEY")
api_secret_key <- Sys.getenv("API_SECRET_KEY")
access_token <- Sys.getenv("ACCESS_TOKEN")
access_token_secret <- Sys.getenv("ACCESS_TOKEN_SECRET")


################################################################################
# CREATE NEW RTWEET TWITTER API TOKEN *(only run if first time running script)  #
################################################################################

## Create an rtweet_token needed to authorize Twitter account API *(uncomment these lines to run)
## Read more: https://rtweet.info/articles/auth.html
#rtweet_token <- create_token(
#  app              = "twitter-account-visualizer",
#  consumer_key     = api_key,
#  consumer_secret  = api_secret_key,
#  access_token     = access_token,
#  access_secret    = access_token_secret,
#  set_renv         = FALSE)

## Construct a file name and path for saving rtweet token
## Define path of project home directory *(uncomment these lines to run)
#app_directory <- path.expand("./")

## Define file name and combine with path from above *(uncomment these lines to run)
#file_name <- file.path(app_directory, ".rtweet_token.rds")

## Save token to app home directory *(uncomment these lines to run)
#saveRDS(rtweet_token, file = file_name)

## Read the token file just to check it out *(uncomment these lines to run)
#readRDS(".rtweet_token.rds")

## Write the path to the token into the .Renviron file (so it can be found and loaded in next time) *(uncomment these lines to run)
#cat(paste0("TWITTER_PAT=", file_name), file = file.path(app_directory, ".Renviron_master"), append = TRUE)


################################################################################
#//////////////////////////////////////////////////////////////////////////////#
#    ///// BUILDING DATA SETS /////////////////////////////////////////////////#
#//////////////////////////////////////////////////////////////////////////////#
################################################################################


########################################
# TYLER PROFILE DATA                   #
########################################

# look_up Tyler
ty <- lookup_users("TylaBillz30")

# Get tweets off Ty's profile timeline
ty_timeline <- get_timeline("TylaBillz30", n = 1000)


########################################
# WHO TYLER FOLLOWS                    #
########################################

# Get user IDs of accounts followed by Ty
followed_by_ty_ids <- get_friends("TylaBillz30", n = 1000)

# Lookup data (by user_id) on accounts followed by Ty
followed_by_ty_accounts <- lookup_users(followed_by_ty_ids$user_id)


########################################
# WHO FOLLOWS TYLER                    #
########################################

# Get user IDs of accounts following Ty
following_ty_ids <- get_followers("TylaBillz30", n = 1000)

## Lookup data (by user_id) on accounts following Ty
following_ty_accounts <- lookup_users(following_ty_ids$user_id)


########################################
# TYLER'S FAVORITED TWEETS             #
########################################

# lookup data on tweets favorited by Ty
favorited_by_ty <- get_favorites("TylaBillz30", n = 1000)


################################################################################
#//////////////////////////////////////////////////////////////////////////////#
#    ///// VISUALIZATION PLOTS ////////////////////////////////////////////////#
#//////////////////////////////////////////////////////////////////////////////#
################################################################################


########################################
# TYLER'S TWEET FREQUENCY              #
########################################

# plot the frequency of tweets over time
plot_tweet_frequency <- ty_timeline %>%
  dplyr::filter(created_at > "2020-01-01") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days") +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = "Date", y = "Count",
    title = "Frequency of Tweets by Tyler",
    subtitle = "Number of tweets per day over the past week",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


##############################################################
# LOCATION OF TY'S FOLLOWERS (ACCORDING TO THEIR PROFILES)   #
##############################################################

# Count of unique locations for accounts following Ty
following_ty_location_count <- length(unique(following_ty_accounts$location))

# Where Ty's followers are located
plot_following_ty_accounts <- following_ty_accounts %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Where Ty's Twitter followers are from - unique locations ")


#######################################################################
# LOCATION OF ACCOUNTS FOLLOWED BY TY (ACCORDING TO THEIR PROFILES)   #
#######################################################################

# Where accounts Ty's follows are located
plot_followed_by_ty_accounts <- followed_by_ty_accounts %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Where accounts followed by Ty's are located - unique locations ")


########################################
# WHO TYLER FAVORITED THE MOST         #
########################################

# Who Ty favorites the most
plot_favorited_by_ty <- favorited_by_ty %>%
  count(screen_name, sort = TRUE) %>%
  mutate(screen_name = reorder(screen_name, n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = screen_name, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "User",
       y = "Count",
       title = "Who Ty favorited the most")


########################################
# WHO TYLER REPLIED TO THE MOST        #
########################################

# Who Ty replied to the most
plot_ty_replied <- ty_timeline %>%
  count(reply_to_screen_name, sort = TRUE) %>%
  mutate(reply_to_screen_name = reorder(reply_to_screen_name, n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = reply_to_screen_name, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "User",
       y = "Count",
       title = "Who Ty replied to most")


########################################
# WHO TYLER RETWEETED THE MOST         #
########################################

# Who Ty retweeted the most
plot_ty_retweeted <- ty_timeline %>%
  count(retweet_screen_name, sort = TRUE) %>%
  mutate(retweet_screen_name = reorder(retweet_screen_name, n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = retweet_screen_name, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "User",
       y = "Count",
       title = "Who Ty retweeted the most")


################################################################################
# WHO TWEETED @ TYLER THE MOST  (INCLUDES TWEETS, RETWEETS, AND QUOTE TWEETS)  #
################################################################################

## Search for tweets containing the text "@TylaBillz30"
tweeted_at_ty <- search_tweets("@TylaBillz30", n = 2500, retryonratelimit = TRUE)

# Who tweeted @TylerBillz30 the most
plot_tweeted_at_ty <- tweeted_at_ty %>%
  count(screen_name, sort = TRUE) %>%
  mutate(screen_name = reorder(screen_name, n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = screen_name, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "User",
       y = "Count",
       title = "Who tweeted @Ty the most")


######################################################################
# WHO TYLER MENTIONED VIA TWEETS, RETWEETS, AND QUOTE TWEETS         #
######################################################################

# Get list of mentioned accounts in Ty's tweets (then unlist because some tweets have multiple mentions)
unlisted_mentions_screen_name <- unlist(ty_timeline$mentions_screen_name)

# Turn unlisted_mentions_screen_name into a new data frame
mentioned_by_ty <- data.frame(unlisted_mentions_screen_name)

# Who Ty mentioned the most
plot_mentioned_by_ty <- mentioned_by_ty %>%
  count(unlisted_mentions_screen_name, sort = TRUE) %>%
  mutate(unlisted_mentions_screen_name = reorder(unlisted_mentions_screen_name, n)) %>%
  na.omit() %>%
  top_n(10) %>%
  ggplot(aes(x = unlisted_mentions_screen_name, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "User",
       y = "Count",
       title = "Who Ty mentioned the most in Tweets, Retweets, and Quote Tweets")


########################################
# CALL ALL PLOTS                       #
########################################
plot_tweet_frequency
plot_following_ty_accounts
plot_followed_by_ty_accounts
plot_favorited_by_ty
plot_ty_replied
plot_ty_retweeted
plot_tweeted_at_ty
plot_mentioned_by_ty


########################################
# END SCRIPT                           #
########################################


################################################################################
#//////////////////////////////////////////////////////////////////////////////#
# ///// USEFUL RESOURCES CONSULTED TO BUILD THIS SCRIPT AND APPLICATION /////  #
#//////////////////////////////////////////////////////////////////////////////#
################################################################################

# https://www.rdocumentation.org/packages/rtweet/versions/0.4.0/vignettes/auth.Rmd
# https://williambork.shinyapps.io/twitter-account-visualizer/
# https://deanattali.com/blog/building-shiny-apps-tutorial/
# https://db.rstudio.com/best-practices/deployment/
# https://www.dartistics.com/renviron.html
# https://shiny.rstudio.com/tutorial/
# https://github.com/ropensci/rtweet
# https://rtweet.info/index.html
