################################################################################
#//////////////////////////////////////////////////////////////////////////////#
#///////// SETTING UP SHINY APP ENVIRONMENT ///////////////////////////////////#
#//////////////////////////////////////////////////////////////////////////////#
################################################################################

######################################
## INSTALL PACKAGES *(if necessary) ##
######################################
## Install packages to your local R (if first time using these packages) *(uncomment below lines to run)
#install.packages("rtweet")
#install.packages("tidyverse")

#######################################
## LOAD INSTALLED PACKAGE LIBRARIES  ##
#######################################
## Load libraries
library(rtweet)
library(tidyverse)
library(shiny)

## Quick overviews for getting started with rtweet *(uncomment below lines to run)
#vignette("auth", package = "rtweet")
#vignette("intro", package = "rtweet")


#####################################################
## CHECK AND SET WORKING DIRECTORY *(if necessary) ##
#####################################################
## Working directory should be the home level ("./") folder for the app and end with: twitter-account-visualizer-app/
## Check that your path is correct and ends in "/twitter-account-visualizer-app" *(uncomment below line to run)
#getwd()

## If path incorrect, run setwd("YOUR/PATH/TO/THE/APP/HERE") to set your path correctly to end in "/twitter-account-visualizer-app" *(uncomment below line to run)
#setwd("./twitter-account-visualizer-app")


########################################
## READ & LOAD ENVIRONMENT VARIABLES  ##
########################################
## Read this .Renviron file and set environment variables as defined in file
readRenviron(".Renviron")
#readRenviron(".Renviron_master") ##(Keep this line commented) because .Renviron_master file doesn't exist in the repo (it was "gitignored"). Is used for production version of app hosted on https://www.shinyapps.io/

## Load rtweet API details from .Renviron into global environment
api_key <- Sys.getenv("API_KEY")
api_secret_key <- Sys.getenv("API_SECRET_KEY")
access_token <- Sys.getenv("ACCESS_TOKEN")
access_token_secret <- Sys.getenv("ACCESS_TOKEN_SECRET")
twitter_pat <- Sys.getenv("TWITTER_PAT")


#######################################################
## MANUALLY GENERATE API AUTHENTICATION TOKEN        ##
## USING A SELF-CREATED TOKEN                        ##
## *(if first time running script)                   ##
## Read more: https://rtweet.info/articles/auth.html ## 
#######################################################
## Create an rtweet_token needed to authorize Twitter account API *(uncomment below lines to run)
## Must have Twitter Developer Account to obtain your own API details for setting up application.
rtweet_token <- create_token(
  app              = "twitter-account-visualizer",
  consumer_key     = api_key,
  consumer_secret  = api_secret_key,
  access_token     = access_token,
  access_secret    = access_token_secret,
  set_renv         = FALSE)

## Construct a file name and path for saving rtweet token
## Define path of project home directory *(uncomment below line to run)
app_directory <- path.expand("./")

## Define file name and combine with path from above *(uncomment below line to run)
file_name <- file.path(app_directory, ".rtweet_token.rds")

## Save token to app home directory *(uncomment below line to run)
saveRDS(rtweet_token, file = file_name)

## Read the token file just to check it out *(uncomment below line to run)
readRDS(".rtweet_token.rds")

## Just another way of viewing the token by calling its name *(uncomment below line to run)
rtweet_token

## Write the path to the token into the .Renviron file (so it can be found and loaded in next time) *(uncomment below line to run)
cat(paste0("TWITTER_PAT=", file_name), file = file.path(app_directory, ".Renviron"), append = TRUE)

## This function will search for tokens using R, internal, and global environment variables (in that order).
## It will prompt your web browser to allow your rstat2twitter to access your account. Only need to run once.
get_token()


## Docs for Shiny Apps and deploying Shiny Apps
## https://www.shinyapps.io/
## https://shiny.rstudio.com/articles/shinyapps.html


################################################################################
#//////////////////////////////////////////////////////////////////////////////#
#    ///// BUILDING DATA SETS /////////////////////////////////////////////////#
#//////////////////////////////////////////////////////////////////////////////#
################################################################################

#########################################
## BUILD DATASET: USER PROFILE         ##
#########################################
## Create variable to hold user's Twitter name
twitter_username <- "github"

## look_up user by searching for their Twitter username
user <- lookup_users(twitter_username)

## Get tweets off user's profile timeline
user_timeline <- get_timeline(user$screen_name, n = 1000)


############################################
## BUILD DATASET: USER'S FAVORITED TWEETS ##
############################################
## Look_up data on tweets favorited by User
favorited_by_user <- get_favorites(user$user_id, n = 1000)


########################################
## BUILD DATASET: WHO USER FOLLOWS    ##
########################################
## Get user IDs of accounts followed by user
followed_by_user_ids <- get_friends(user$user_id, n = 1000)

## Look_up data (by user_id) on accounts followed by user
followed_by_user_accounts <- lookup_users(followed_by_user_ids$user_id)


########################################
## BUILD DATASET: WHO FOLLOWS USER    ##
########################################
## Get user IDs of accounts following User
following_user_ids <- get_followers(user$user_id, n = 1000)

## Lookup data (by user_id) on accounts following User
following_user_accounts <- lookup_users(following_user_ids$user_id)


################################################################################
#//////////////////////////////////////////////////////////////////////////////#
#///////// BUILD VISUALIZATION PLOTS //////////////////////////////////////////#
#//////////////////////////////////////////////////////////////////////////////#
################################################################################

########################################
## PLOT: USER'S TWEET FREQUENCY       ##
########################################
## Create plot for the frequency of tweets over time
plot_tweet_frequency <- user_timeline %>%
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
      title = "Frequency of Tweets by User",
      subtitle = "Number of tweets per day over the past week",
      caption = "\nSource: Data collected from Twitter's REST API via rtweet")


########################################
## PLOT: WHO USER FAVORITED THE MOST  ##
########################################
## Create plot for who User favorited the most
plot_favorited_by_user <- favorited_by_user %>%
    count(screen_name, sort = TRUE) %>%
    mutate(screen_name = reorder(screen_name, n)) %>%
    na.omit() %>%
    top_n(10) %>%
    ggplot(aes(x = screen_name, y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = "User",
         y = "Count",
         title = "Who User Favorited the Most")


########################################
## PLOT: WHO USER RETWEETED THE MOST  ##
########################################
## Create plot for who User retweeted the most
plot_user_retweeted <- user_timeline %>%
    count(retweet_screen_name, sort = TRUE) %>%
    mutate(retweet_screen_name = reorder(retweet_screen_name, n)) %>%
    na.omit() %>%
    top_n(10) %>%
    ggplot(aes(x = retweet_screen_name, y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = "User",
         y = "Count",
         title = "Who User Retweeted the Most")


########################################
## PLOT: WHO USER REPLIED TO THE MOST ##
########################################
## Create plot for who User replied to the most
plot_user_replied <- user_timeline %>%
    count(reply_to_screen_name, sort = TRUE) %>%
    mutate(reply_to_screen_name = reorder(reply_to_screen_name, n)) %>%
    na.omit() %>%
    top_n(10) %>%
    ggplot(aes(x = reply_to_screen_name, y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = "User",
         y = "Count",
         title = "Who User Replied to the Most")


######################################################################################
## PLOT: WHO TWEETED @ USER THE MOST (INCLUDES TWEETS, RETWEETS, AND QUOTE TWEETS)  ##
######################################################################################
## BREAKS (APP WON'T LAUNCH) IF NO @'s WITHIN API TIME FRAME
#########################################
## Concatenate the "@" symbol and the user's screen name into a variable used for searching tweets
#user_screen_name_string <- paste("@", toString(user$screen_name), sep="")

## Search for tweets containing "@" + user's screen_name
#tweeted_at_user <- search_tweets(user_screen_name_string, n = 2500, retryonratelimit = TRUE)

## Create plot for who tweeted @User the most
#  plot_tweeted_at_user <- tweeted_at_user %>%
#    count(screen_name, sort = TRUE) %>%
#    mutate(screen_name = reorder(screen_name, n)) %>%
#    na.omit() %>%
#    top_n(10) %>%
#    ggplot(aes(x = screen_name, y = n)) +
#    geom_col() +
#    coord_flip() +
#    labs(x = "User",
#         y = "Count",
#         title = "Who Tweeted @User the Most")


######################################################################
## WHO USER MENTIONED VIA TWEETS, RETWEETS, AND QUOTE TWEETS        ##
######################################################################
## THROWS ERROR AND WON'T PROVIDE DATA BUT DOESN'T BREAK APP
#########################################
## Get list of mentioned accounts in User's tweets (then unlist them because some tweets have multiple mentions per tweet)
unlisted_mentions_screen_name <- unlist(user_timeline$mentions_screen_name)

## Turn unlisted_mentions_screen_name into a new data frame which is used in make subsequent plot
mentioned_by_user <- data.frame(unlisted_mentions_screen_name)

## Create plot for who User mentioned the most via tweets, retweets, and quote tweets
plot_mentioned_by_user <- mentioned_by_user %>%
    count(unlisted_mentions_screen_name, sort = TRUE) %>%
    mutate(unlisted_mentions_screen_name = reorder(unlisted_mentions_screen_name, n)) %>%
    na.omit() %>%
    top_n(10) %>%
    ggplot(aes(x = unlisted_mentions_screen_name, y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = "User",
         y = "Count",
         title = "Who User Mentioned the Most in Tweets, Retweets, and Quote Tweets")


######################################################################
## PLOT: LOCATION OF USER'S FOLLOWERS (ACCORDING TO THEIR PROFILES) ##
######################################################################
## Count of unique locations for accounts following User
following_user_location_count <- length(unique(following_user_accounts$location))

## Create plot for where User's followers are located
plot_following_user_accounts <- following_user_accounts %>%
    count(location, sort = TRUE) %>%
    mutate(location = reorder(location, n)) %>%
    na.omit() %>%
    top_n(10) %>%
    ggplot(aes(x = location, y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = "Location",
         y = "Count",
         title = "Where User's Twitter Followers Are From - Unique Locations ")


###############################################################################
## PLOT: LOCATION OF ACCOUNTS FOLLOWED BY USER (ACCORDING TO THEIR PROFILES) ##
###############################################################################
## Create plot for where accounts followed by User are located
plot_followed_by_user_accounts <- followed_by_user_accounts %>%
    count(location, sort = TRUE) %>%
    mutate(location = reorder(location, n)) %>%
    na.omit() %>%
    top_n(10) %>%
    ggplot(aes(x = location, y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = "Location",
         y = "Count",
         title = "Where Accounts Followed by User Are Located - Unique Locations ")


########################################
## CALL ALL PLOTS                     ##
########################################
## Call all plots
plot_tweet_frequency
plot_favorited_by_user
plot_user_retweeted
plot_user_replied
plot_tweeted_at_user
plot_mentioned_by_user
plot_following_user_accounts
plot_followed_by_user_accounts


########################################
## END SCRIPT                         ##
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

