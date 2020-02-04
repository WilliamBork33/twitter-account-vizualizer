########################################
# SETTING UP APP                       #
########################################
library(shiny)
source("script.r")

# Docs for Shiny Apps and deploying Shiny Apps
#https://www.shinyapps.io/
#https://shiny.rstudio.com/articles/shinyapps.html

########################################
# CONSTRUCTING USER INTERFACE (UI)     #
########################################
ui <- fluidPage(
  # App title ----
  titlePanel("Twitter Data Insights App"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "choose_plot", 
                   label = "Choose Plot", 
                    choiceNames = c("MY Tweet Frequency",
                      "Location of MY followers",
                      "Location of accounts I followed",
                      "Most favorited by ME",
                      "Who I replied to the most",
                      "Who I retweeted the most",
                      "Who tweeted @ME the most",
                      "Who I mentioned the most"),
                    choiceValues = c("1",
                       "2",
                       "3",
                       "4",
                       "5",
                       "6",
                       "7",
                       "8")
      )
    ),

    mainPanel(
      plotOutput("plotOutputArea")
    )
  )
)

########################################
# SETTING UP SERVER                    #
########################################
server <- function(input, output) {
  output$plotOutputArea <- renderPlot({
    switch(input$choose_plot, 
           "1" = plot_tweet_frequency,
           "2" = plot_following_ty_accounts,
           "3" = plot_followed_by_ty_accounts,
           "4" = plot_favorited_by_ty,
           "5" = plot_ty_replied,
           "6" = plot_ty_retweeted,
           "7" = plot_tweeted_at_ty,
           "8" = plot_mentioned_by_ty
     )
  })
}

########################################
# RUN SHINYAPP                         #
########################################
shinyApp(ui = ui, server = server)