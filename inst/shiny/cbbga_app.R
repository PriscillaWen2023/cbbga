{r}
library(shiny)
library(tidyverse)

# Read the data
data <- read_csv("https://raw.githubusercontent.com/PriscillaWen2023/cbbga/main/data-raw/cbbga_2023.csv")

# Define function to compute win/loss record and winning percentage for a given team
team_stats <- function(data, team_name) {
  # Filter games by the given team
  team_games <- data %>%
    filter(Away_Team == team_name | Home_Team == team_name)
  
  # Calculate win/loss record and winning percentage
  stats <- team_games %>%
    mutate(outcome = case_when(
      (Away_Team == team_name & Away_Score > Home_Score) ~ "Win",
      (Home_Team == team_name & Home_Score > Away_Score) ~ "Win",
      TRUE ~ "Loss"
    )) %>%
    summarize(
      Team = team_name,
      Wins = sum(outcome == "Win"),
      Losses = sum(outcome == "Loss"),
      Total_Games = Wins + Losses,
      Winning_Percentage = Wins / Total_Games
    )
  
  return(stats)
}

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
      #h4("All Teams in alphabetical order:"),
      tableOutput("all_teams_table")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$submit, {
    req(input$team_name)
    
    # Check if team name is valid
    if (!input$team_name %in% unique(c(data$Away_Team, data$Home_Team))) {
      output$invalid_team_message <- renderPrint({
        "Invalid team name. Please enter a valid team name!"
      })
      output$teams_name <- renderPrint({
        "All Teams in alphabetical order:"
      })
      output$team_stats_table <- renderTable(NULL)
      output$win_loss_pie <- renderPlot(NULL)
      
      all_teams <- sort(unique(c(data$Away_Team, data$Home_Team)))
      all_teams_data <- tibble(Team_Name = all_teams)
      output$all_teams_table <- renderTable({
        all_teams_data
      })
    } else {
      # Get team stats
      team_stats_data <- team_stats(data, input$team_name)
      
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
      
      output$invalid_team_message <- renderPrint(NULL)
      output$teams_name <- renderPrint(NULL)
      output$all_teams_table <- renderTable(NULL)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
