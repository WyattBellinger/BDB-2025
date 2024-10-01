plot_average_max_speed_per_team_all <- function(data) {
  required_cols <- c("nflId", "displayName", "club", "s")
  missing_cols <- setdiff(required_cols, names(data))
  if(length(missing_cols) > 0){
    stop(paste("The following required columns are missing in the data:", paste(missing_cols, collapse = ", ")))
  }
  valid_data <- subset(data, !is.na(club) & !is.na(s))

  max_speed_per_player <- aggregate(s ~ nflId + displayName + club, data = valid_data, FUN = max)
  if(nrow(max_speed_per_player) == 0){
    warning("No valid speed data available to plot.")
    return(NULL)
  }
  average_max_speed_per_team <- aggregate(s ~ club, data = max_speed_per_player, FUN = mean)
  average_max_speed_per_team <- average_max_speed_per_team[order(average_max_speed_per_team$s, decreasing = TRUE), ]
  barplot_heights <- average_max_speed_per_team$s
  bar_names <- average_max_speed_per_team$club
  old_par <- par(no.readonly = TRUE)
  par(mar = c(12, 4, 4, 2) + 0.1) 
  bar_positions <- barplot(
    height = barplot_heights,
    names.arg = bar_names,
    ylim = c(0, max(barplot_heights) * 1.3), 
    main = "Average Maximum Speed per Team (All Games and Plays)",
    ylab = "Average Max Speed (yards/second)",
    xlab = "Teams",
    las = 2, 
    border = "black"
  )
  text(
    x = bar_positions,
    y = barplot_heights + max(barplot_heights)*0.05,
    labels = round(barplot_heights, 2),
    srt = 45,
    adj = c(0,0),
    cex = 0.8,
    col = "black"
  )
  par(old_par)
}

plot_average_max_speed_per_team_all(data)