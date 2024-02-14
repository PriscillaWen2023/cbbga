#' Filter games by a given team
#'
#' This function filters a tibble containing basketball games data to include only
#' the games played by a specified team.
#'
#' @param data A tibble containing basketball games data.
#' @param team_name The name of the team for which games are to be filtered.
#'
#' @return A tibble containing only the games played by the specified team.
#'
#' @examples
#' # Filter games for the team named "Duke"
#' duke_games <- games_by_team(cbbga, "Duke")
#'
#' @import dplyr
#' @export
games_by_team <- function(data, team_name) {
  data %>%
    filter(Away_Team == team_name | Home_Team == team_name)
}