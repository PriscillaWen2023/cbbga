#' Create Histogram of Games for each team
#'
#' This function generates a histogram showing the distribution of total games played by each team.
#'
#' @param data A tibble containing basketball game data, including dates, teams, and scores.
#' 
#' @return A ggplot object representing the histogram of total games played by each team.
#' 
#' @examples
#' # Generate histogram of total games for the provided data
#' total_games_histogram(cbbga_2023)
#' 
#' @import ggplot2
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal
#' @export
total_games_histogram <- function(data) {
  ggplot(data, aes(x = Total_Games)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(title = "Total Games Histogram for Each Team",
         x = "Total Games",
         y = "Frequency") +
    theme_minimal()
}
