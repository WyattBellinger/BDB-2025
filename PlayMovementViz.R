plot_player <- function(player_name, game_id, play_id, data, player_color) {
  player_data <- subset(data, displayName == player_name & gameId == game_id & playId == play_id)
  if (nrow(player_data) == 0) {
    warning(paste("No data found for", player_name, "in game", game_id, "and play", play_id))
    return(NULL)
  }
  lines(player_data$x, player_data$y, col = player_color, lwd = 2)
}

plot_play_movement <- function(game_id, play_id, data) {
  play_data <- subset(data, gameId == game_id & playId == play_id)
  
  if (nrow(play_data) == 0) {
    warning(paste("No data found for game", game_id, "and play", play_id))
    return(NULL)
  }
  teams <- unique(play_data$club[play_data$displayName != "football"])
  
  team_colors <- rainbow(length(teams))
  names(team_colors) <- teams
  
  football_color <- "brown"
  
  plot(NULL, xlim = c(0, 120), ylim = c(0, 53.3),
       xlab = 'X Position (yards)', ylab = 'Y Position (yards)',
       main = paste("Player Movements for Game", game_id, "Play", play_id))
  
  rect(0, 0, 120, 53.3, border = "green", lwd = 2)
  rect(0, 0, 10, 53.3, border = "black", lwd = 2)
  rect(110, 0, 120, 53.3, border = "black", lwd = 2)
  
  for (player in unique(play_data$displayName)) {
    if (player == "football") {
      player_color <- football_color
    } else {
      player_team <- unique(play_data$club[play_data$displayName == player])
      player_color <- team_colors[player_team]
    }
    plot_player(player, game_id, play_id, data, player_color)
  }
  
  legend_labels <- c(teams, "Football")
  legend_colors <- c(team_colors, football_color)
  legend("topright", legend = legend_labels, col = legend_colors, lwd = 2, bg = "white")
}

plot_play_movement(2022101603, 346, data)
