library(shinydashboard)
library(ggplot2)
library(reshape2)
source("global.R")


ui <- dashboardPage(
  dashboardHeader(title = "Basketball statistics"),
  dashboardSidebar(sidebarMenu(
      menuItem("Championship Outlook", tabName = "allTeams", icon = icon("bar-chart-o")),
      menuItem("Individual Team Statistics", tabName = "singleTeam", icon = icon("bar-chart-o")),
      menuItem("Individual Player Statistics", tabName = "singlePlayer", icon = icon("bar-chart-o")),
      menuItem("About", tabName = "info", icon = icon("info-circle"))
    )),
  dashboardBody(
   tabItems (
	# First tab content
	tabItem(tabName="allTeams",
	 fluidRow(
          column(width=8,
            box(
              title="Points scored from all teams",status="primary",solidHeader=TRUE,collapsible=FALSE,width=NULL,
	      plotOutput("allBoxPlot", height = 250)
            ),
	    box(
	     title="Current Standings",status="primary",solidHeader=TRUE,collapsible=FALSE,width=NULL,
	     dataTableOutput("standings")
            )
          ) #end column
         ) # end fluidRow
        ),

	# Second tab content
	tabItem(tabName="singleTeam",
	 fluidRow(
            box(
              title="Team's score",status="primary",solidHeader=TRUE,collapsible=FALSE,color="red",
              plotOutput("teamScorePlotA",height = 250)
            ),
            box(
              title="Choose the team",status="warning",solidHeader=FALSE,collapsible=FALSE,
	      selectInput("teamListA", "Choose a team for the statistics",choices=teamsNames)
            )
         ), # end fluidRow

         fluidRow(
           box(
             title="Team's score Home vs Away",status="primary",solidHeader=TRUE,collapsible=FALSE,
             plotOutput("teamHomeAwayScore",height=250)
           )
         ) # end fluidRow
	 
	),

	# Third tab content
	tabItem(tabName="singlePlayer",
	 fluidRow(
            
            box(
              title="Points scored from the player",status="primary",solidHeader=TRUE,collapsible=FALSE,
	      plotOutput("playerScorePlot", height = 250)
            ),
            box(
              title="Choose the player",status="warning",
	      selectInput("playerList", "Choose a player for the statistics",choices=playerNames)
            )
         ) # end fluidRow
        ),


	# Fourth tab content
	tabItem(tabName="info",
	   h4("App Description"),
	   br(),
	   h4("This app features some statistics from Women's Basketball League (2017-2018)"),
           br(),
           h4("The data were obtained from www.basket.gr"),
           br(),
           h4("For any further information, please contact danaifimereli@hotmail.com")
	)
   )
  ) 
)

server <- function(input, output) {

# plot for all teams points
output$allBoxPlot<- renderPlot ({
allTeamScores(scores,teamsNames)
})

# plot for each team points 
output$teamScorePlotA<- renderPlot ({
getTeam(scores,input$teamListA)
}) 

# plot team home vs away points 
output$teamHomeAwayScore<- renderPlot ({
teamHomeAwayScore(scores,input$teamListA)
})

# table with the final score table 
output$standings <- renderDataTable({
standings
})


# plot for each player points 
output$playerScorePlot<- renderPlot ({
getPlayer(playerScores,input$playerList)
}) 



}

shinyApp(ui=ui,server=server)
