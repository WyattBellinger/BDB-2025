# Function to calculate and plot total distance per team for a specific play
plot_total_distance_per_team <- function(game_id, play_id, data) {
  # Subset data for the specific game and play
  play_data <- subset(data, gameId == game_id & playId == play_id & !is.na(nflId))
  
  if (nrow(play_data) == 0) {
    warning(paste("No data found for game", game_id, "and play", play_id))
    return(NULL)
  }
  
  # Identify teams
  teams <- unique(play_data$club)
  
  # Initialize a vector to store total distance per team
  total_distance <- numeric(length(teams))
  names(total_distance) <- teams
  
  # Calculate distance for each player and sum by team
  for (team in teams) {
    team_players <- unique(play_data$nflId[play_data$club == team])
    team_distance <- 0
    for (player in team_players) {
      player_data <- subset(play_data, nflId == player)
      player_data <- player_data[order(player_data$frameId), ]
      # Calculate distance between consecutive frames
      if (nrow(player_data) > 1) {
        dx <- diff(player_data$x)
        dy <- diff(player_data$y)
        distances <- sqrt(dx^2 + dy^2)
        team_distance <- team_distance + sum(distances, na.rm = TRUE)
      }
    }
    total_distance[team] <- team_distance
  }
  
  # Plot the total distances
  barplot(total_distance, 
          col = rainbow(length(teams)), 
          ylim = c(0, max(total_distance) * 1.1),
          main = paste("Total Distance Covered by Each Team in Game", game_id, "Play", play_id),
          ylab = "Total Distance (yards)",
          las = 2)  # Rotate x-axis labels for readability
  
  # Add numeric labels on top of the bars
  text(x = seq_along(total_distance), 
       y = total_distance, 
       label = round(total_distance, 1), 
       pos = 3, cex = 0.8)
}

# Example usage
plot_total_distance_per_team(2022092508, 2939, data)
