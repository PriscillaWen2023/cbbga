#' Create stacked box plot for the 20 teams who played the most games
#' 
#' This function generates a stacked box plot showing the distribution of wins and losses for the top 20 teams who played the most games.
#' 
#' @param data A tibble containing basketball game data, including team statistics such as wins, losses, and total games played.
#' 
#' @return A ggplot object representing the stacked box plot for the top 20 teams.
#' 
#' @examples
#' # Generate stacked box plot for the provided data
#' stacked_box_plot(team_stats_df)
#' 
#' @import ggplot2
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal
#' @export
stacked_box_plot <- function(data) {
  # Filter for top 20 teams based on total games played
  top_20_teams <- head(arrange(data, desc(Total_Games)), 20)
  
  # Create stacked bar plot
  ggplot(top_20_teams, aes(x = reorder(Team, -Total_Games))) +
    geom_bar(aes(y = Wins, fill = "Wins"), stat = "identity", position = "stack") +
    geom_bar(aes(y = Losses, fill = "Losses"), stat = "identity", position = "stack") +
    labs(title = "Stacked Box Plot for Top 20 Winning Percentage Teams",
         x = "Team",
         y = "Count",
         fill = "Outcome") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

