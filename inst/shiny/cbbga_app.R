library(shiny)
library(tidyverse)
library(cbbga)

# UI
ui <- fluidPage(
  titlePanel("College Basketball Team Stats"),
  sidebarLayout(
    sidebarPanel(
      textInput("team_name", "Enter Team Name:"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      h3("Team Stats"),
      tableOutput("team_stats_table"),
      plotOutput("win_loss_pie"),
      verbatimTextOutput("invalid_team_message"),
      verbatimTextOutput("teams_name"),
      tableOutput("all_teams_table")
      
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observeEvent(input$submit, {
    req(input$team_name)
    
    # Reset outputs
    output$team_stats_table <- renderTable(NULL)
    output$win_loss_pie <- renderPlot(NULL)
    output$all_teams_table <- renderTable(NULL)
    
    # Check if team name is valid
    if (!input$team_name %in% unique(c(cbbga_2023$Away_Team, cbbga_2023$Home_Team))) {
      output$invalid_team_message <- renderText({
        "Invalid team name. Please enter a valid team name!"
      })
      output$teams_name <- renderText({
        "All Teams in alphabetical order:"
      })
      
      all_teams <- sort(unique(c(cbbga_2023$Away_Team, cbbga_2023$Home_Team)))
      all_teams_data <- tibble(Team_Name = all_teams)
      output$all_teams_table <- renderTable({
        all_teams_data
      })
    } else {
      # Get team stats using the team_stats function from your package
      team_stats_data <- team_stats(cbbga_2023, input$team_name)
      
      # Output table of team stats
      output$team_stats_table <- renderTable({
        team_stats_data
      })
      
      # Output pie chart for win/loss
      output$win_loss_pie <- renderPlot({
        pie(c(team_stats_data$Wins, team_stats_data$Losses), 
            labels = c(paste("Wins (", round(100*team_stats_data$Winning_Percentage, 1), "%)"), 
                       paste("Losses (", round(100*(1-team_stats_data$Winning_Percentage), 1), "%)")), 
            col = c("skyblue", "#FF6666"))
        title(main = "Win-Loss Distribution")
      })
      output$invalid_team_message <- renderText({
        ""
      })  
      output$teams_name <- renderText({
        ""
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
