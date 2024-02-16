#' Compute win/loss record and winning percentage for a given team
#'
#' This function calculates the win/loss record and winning percentage for a specified team based on the provided data.
#'
#' @param data A tibble containing basketball game data, including dates, teams, and scores.
#' @param team_name A character string specifying the name of the team for which statistics are to be computed.
#' 
#' @return A tibble with the following columns:
#'   \describe{
#'     \item{Team}{The name of the team.}
#'     \item{Wins}{The number of wins for the team.}
#'     \item{Losses}{The number of losses for the team.}
#'     \item{Total_Games}{The total number of games played by the team.}
#'     \item{Winning_Percentage}{The winning percentage of the team, calculated as the ratio of wins to total games.}
#'   }
#' 
#' @examples
#' # Compute team statistics for "Southern Utah"
#' team_stats(cbbga_2023, "Southern Utah")
#' 
#' @import dplyr
#' @export
#' 
team_stats <- function(data, team_name) {
  # Filter games by the given team
  team_games <- data %>%
    dplyr::filter(Away_Team == team_name | Home_Team == team_name)
  
  # Calculate win/loss record and winning percentage
  stats <- team_games %>%
    dplyr::mutate(outcome = case_when(
      (Away_Team == team_name & Away_Score > Home_Score) ~ "Win",
      (Home_Team == team_name & Home_Score > Away_Score) ~ "Win",
      TRUE ~ "Loss"
    )) %>%
    dplyr::summarize(
      Team = team_name,
      Wins = sum(outcome == "Win"),
      Losses = sum(outcome == "Loss"),
      Total_Games = Wins + Losses,
      Winning_Percentage = Wins / Total_Games
    )
  
  return(stats)
}
