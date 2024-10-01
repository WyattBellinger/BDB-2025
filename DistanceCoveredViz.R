plot_total_distance_per_team <- function(game_id, data) {
  game_data <- subset(data, gameId == game_id & !is.na(nflId))
  teams <- unique(game_data$club)

  total_distance <- numeric(2)
  names(total_distance) <- teams

  for (team in teams) {
    team_players <- unique(game_data$nflId[game_data$club == team])
    team_distance <- 0
    
    for (player in team_players) {
      player_data <- subset(game_data, nflId == player)
      player_data <- player_data[order(player_data$playId, player_data$frameId), ]
      if (nrow(player_data) > 1) {
        dx <- diff(player_data$x)
        dy <- diff(player_data$y)
        distances <- sqrt(dx^2 + dy^2)
        team_distance <- team_distance + sum(distances, na.rm = TRUE)
      }
    }
    total_distance[team] <- team_distance
  }
  total_yards <- sum(total_distance)
  percentages <- (total_distance / total_yards) * 100
  
  labels <- paste0(names(total_distance), "\n",
                   round(total_distance, 0), " yards\n",
                   round(percentages, 1), "%")

  pie_colors <- terrain.colors(2)
  pie(
    x = total_distance,
    labels = labels,
    col = pie_colors,
    main = paste("Total Distance Covered by Each Team, GameID: ", game_id),
    border = "white",
    cex = 0.8
  )
}

plot_total_distance_per_team(2022091106, data)
