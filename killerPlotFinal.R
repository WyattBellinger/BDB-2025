# Install required packages if not already installed
# install.packages("farver")
# install.packages("teamcolors")
# install.packages("grid")

library(farver)
library(teamcolors)
library(grid)

# Team name mapping
team_name_mapping <- list(
  ARI = "Arizona Cardinals", ATL = "Atlanta Falcons", BAL = "Baltimore Ravens",
  BUF = "Buffalo Bills", CAR = "Carolina Panthers", CHI = "Chicago Bears",
  CIN = "Cincinnati Bengals", CLE = "Cleveland Browns", DAL = "Dallas Cowboys",
  DEN = "Denver Broncos", DET = "Detroit Lions", GB  = "Green Bay Packers",
  HOU = "Houston Texans", IND = "Indianapolis Colts", JAX = "Jacksonville Jaguars",
  KC  = "Kansas City Chiefs", LAC = "Los Angeles Chargers", LAR = "Los Angeles Rams",
  LV  = "Las Vegas Raiders", MIA = "Miami Dolphins", MIN = "Minnesota Vikings",
  NE  = "New England Patriots", NO  = "New Orleans Saints", NYG = "New York Giants",
  NYJ = "New York Jets", PHI = "Philadelphia Eagles", PIT = "Pittsburgh Steelers",
  SEA = "Seattle Seahawks", SF  = "San Francisco 49ers", TB  = "Tampa Bay Buccaneers",
  TEN = "Tennessee Titans", WAS = "Washington Commanders"
)

# Function to convert hex color to RGB
hex_to_rgb <- function(hex) {
  rgb <- col2rgb(hex) / 255
  return(as.numeric(rgb))
}

# Function to calculate Lab distance between two colors
lab_distance <- function(color1, color2) {
  rgb1 <- as.numeric(col2rgb(color1))
  rgb2 <- as.numeric(col2rgb(color2))
  
  lab1 <- farver::convert_colour(matrix(rgb1, nrow = 1), from = "rgb", to = "lab")
  lab2 <- farver::convert_colour(matrix(rgb2, nrow = 1), from = "rgb", to = "lab")
  
  return(sqrt(sum((lab1 - lab2) ^ 2)))
}

# Extract NFL team colors
nfl_teams <- subset(teamcolors, league == "nfl")
nfl_team_colors <- setNames(nfl_teams$primary, nfl_teams$name)
nfl_team_secondary_colors <- setNames(nfl_teams$secondary, nfl_teams$name)

# Helper functions for time conversion
# Convert "M:SS" to total seconds
time_to_seconds <- function(time_str) {
  parts <- unlist(strsplit(time_str, ":"))
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(parts[2])
  total_seconds <- minutes * 60 + seconds
  return(total_seconds)
}

# Convert total seconds to "M:SS" format
seconds_to_time <- function(total_seconds) {
  if (total_seconds < 0) total_seconds <- 0
  minutes <- floor(total_seconds / 60)
  seconds <- floor(total_seconds %% 60)
  # Ensure two digits for seconds
  time_str <- sprintf("%d:%02d", minutes, seconds)
  return(time_str)
}

# Modified plot_play_movement function
plot_play_movement <- function(game_id, play_id, data, plays, games, college_data = NULL, 
                               num_high_offense = 0, num_low_offense = 0, 
                               num_high_defense = 0, num_low_defense = 0) {
  
  # Subset play data for the specific game and play
  play_data <- subset(data, gameId == game_id & playId == play_id)
  
  # Subset plays data to get possession and defensive teams
  play_info <- subset(plays, gameId == game_id & playId == play_id)
  
  # Ensure that play_info is not empty
  if (nrow(play_info) == 0) {
    stop("No matching play found in 'plays' data frame.")
  }
  
  possession_team_code <- play_info$possessionTeam[1]
  defensive_team_code <- play_info$defensiveTeam[1]
  
  # Subset games data to get team abbreviations and scores
  game_info <- subset(games, gameId == game_id)
  
  if (nrow(game_info) == 0) {
    stop("No matching game found in 'games' data frame.")
  }
  
  home_team_abbr <- toupper(game_info$homeTeamAbbr[1])
  away_team_abbr <- toupper(game_info$visitorTeamAbbr[1])
  
  preSnapVisitorScore <- play_info$preSnapVisitorScore[1]
  preSnapHomeScore <- play_info$preSnapHomeScore[1]
  
  if (possession_team_code == home_team_abbr) {
    offense_score <- preSnapHomeScore
    defense_score <- preSnapVisitorScore
    offense_team <- team_name_mapping[[home_team_abbr]]
    defense_team <- team_name_mapping[[away_team_abbr]]
  } else if (possession_team_code == away_team_abbr) {
    offense_score <- preSnapVisitorScore
    defense_score <- preSnapHomeScore
    offense_team <- team_name_mapping[[away_team_abbr]]
    defense_team <- team_name_mapping[[home_team_abbr]]
  } else {
    stop("Possession team does not match home or away team in 'games' data frame.")
  }
  
  # Map team codes to full team names
  possession_team <- team_name_mapping[[possession_team_code]]
  defensive_team <- team_name_mapping[[defensive_team_code]]
  
  # Get team colors and create a named vector
  team_colors <- nfl_team_colors[c(possession_team, defensive_team)]
  names(team_colors) <- c(possession_team, defensive_team)
  
  # Adjust team colors to ensure sufficient contrast
  team_names <- c(possession_team, defensive_team)
  for (i in 1:(length(team_colors) - 1)) {
    for (j in (i + 1):length(team_colors)) {
      if (lab_distance(team_colors[i], team_colors[j]) < 40) {
        team_name <- team_names[i]
        team_colors[i] <- nfl_team_secondary_colors[[team_name]]
      }
    }
  }
  
  football_color <- "brown"
  
  frame_ids <- sort(unique(play_data$frameId))
  
  # Prepare static components once
  grid.newpage()
  
  # Adjust yscale to add padding on top and bottom
  y_padding <- 5  # Adjust this value to increase/decrease padding
  y_min <- -y_padding
  y_max <- 53.3 + y_padding
  
  # Set up the main viewport with adjusted yscale
  pushViewport(viewport(x = 0.5, y = 0.5, width = 1, height = 1,
                        xscale = c(0, 140), yscale = c(y_min, y_max), name = "main_vp"))
  
  # Remove field background colors (set to white)
  grid.rect(x = unit(0, "native"), y = unit(0, "native"), 
            width = unit(120, "native"), height = unit(53.3, "native"),
            just = c("left", "bottom"), gp = gpar(fill = "white", col = "darkgreen", lwd = 2))
  
  # Draw end zones borders
  grid.rect(x = unit(0, "native"), y = unit(0, "native"), 
            width = unit(10, "native"), height = unit(53.3, "native"),
            just = c("left", "bottom"), gp = gpar(fill = NA, col = "black", lwd = 2))
  grid.rect(x = unit(110, "native"), y = unit(0, "native"), 
            width = unit(10, "native"), height = unit(53.3, "native"),
            just = c("left", "bottom"), gp = gpar(fill = NA, col = "black", lwd = 2))
  
  yard_labels <- c(10, 20, 30, 40, 50, 40, 30, 20, 10)
  yard_positions <- seq(20, 100, by = 10)
  
  # Draw yard lines and labels over the background
  for (i in seq_along(yard_positions)) {
    yard <- yard_positions[i]
    grid.lines(x = unit(c(yard, yard), "native"),
               y = unit(c(0, 53.3), "native"),
               gp = gpar(col = "lightgray", lty = "dashed"))
    grid.text(label = yard_labels[i], x = unit(yard, "native"), 
              y = unit(1.7, "native"), gp = gpar(col = "darkgray", fontsize = 13, fontface = "bold"))
    grid.text(label = yard_labels[i], x = unit(yard, "native"), 
              y = unit(51.6, "native"), gp = gpar(col = "darkgray", fontsize = 13, fontface = "bold"))
  }
  
  # Title (adjusted for padding)
  title_label <- paste0(offense_team, ": ", offense_score, " vs. ", 
                        defense_team, ": ", defense_score)
  grid.text(label = title_label,
            x = unit(60, "native"), y = unit(y_max - 1, "native"),
            gp = gpar(fontsize = 14, fontface = "bold"))
  
  # Legend (adjusted for padding)
  legend_labels <- c(possession_team, defensive_team, "Football")
  legend_colors <- c(team_colors, football_color)
  
  legend_x <- unit(122, "native")
  legend_y <- unit(y_max - 2, "native")
  
  n_legend_items <- length(legend_labels)
  legend_spacing <- unit(1.5, "lines")
  
  for (i in seq_len(n_legend_items)) {
    y_pos <- legend_y - (i - 1) * legend_spacing
    grid.lines(x = unit.c(legend_x, legend_x + unit(5, "mm")), y = y_pos, 
               gp = gpar(col = legend_colors[i], lwd = 2))
    grid.text(label = legend_labels[i], x = legend_x + unit(7, "mm"), y = y_pos, 
              just = "left", gp = gpar(col = "black", fontsize = 10))
  }
  
  # Create a dynamic viewport for moving elements
  dynamic_vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1,
                         xscale = c(0, 140), yscale = c(y_min, y_max), name = "dynamic_vp")
  pushViewport(dynamic_vp)
  
  # Identify offense and defense players
  offense_players <- subset(play_data, club == possession_team_code & displayName != "football")
  defense_players <- subset(play_data, club == defensive_team_code & displayName != "football")
  
  # Merge with college_data to get 'final_score' (if provided)
  if (!is.null(college_data)) {
    offense_players <- merge(offense_players, college_data, by.x = "displayName", by.y = "name", all.x = TRUE)
    defense_players <- merge(defense_players, college_data, by.x = "displayName", by.y = "name", all.x = TRUE)
  }
  
  # Aggregate 'final_score' for offense and defense
  if (!is.null(college_data)) {
    offense_scores <- aggregate(final_score ~ displayName, data = offense_players, FUN = mean, na.rm = TRUE)
    defense_scores <- aggregate(final_score ~ displayName, data = defense_players, FUN = mean, na.rm = TRUE)
    
    # Highest and lowest athleticism offense players
    top_offense_players <- head(offense_scores[order(-offense_scores$final_score), "displayName"], num_high_offense)
    low_offense_players <- head(offense_scores[order(offense_scores$final_score), "displayName"], num_low_offense)
    
    # Highest and lowest athleticism defense players
    top_defense_players <- head(defense_scores[order(-defense_scores$final_score), "displayName"], num_high_defense)
    low_defense_players <- head(defense_scores[order(defense_scores$final_score), "displayName"], num_low_defense)
  } else {
    # If college_data is not provided, no highlighting
    top_offense_players <- character(0)
    low_offense_players <- character(0)
    top_defense_players <- character(0)
    low_defense_players <- character(0)
  }
  
  # Initialize gameClock
  initial_gameClock_str <- play_info$gameClock[1]
  total_seconds <- time_to_seconds(initial_gameClock_str)
  
  # Prepare to loop over frames
  for (frame in frame_ids) {
    
    # Clear previous dynamic content
    grid.remove("dynamic_content", warn = FALSE)
    
    # Start a new gTree to hold dynamic content
    dynamic_grob <- gTree(name = "dynamic_content")
    
    # Get data for the current frame (only current positions, no trails)
    frame_data <- subset(play_data, frameId == frame)
    
    unique_players <- unique(frame_data$displayName)
    
    for (player in unique_players) {
      player_data <- subset(frame_data, displayName == player)
      
      if (player == "football") {
        player_color <- football_color
      } else {
        player_team_code <- unique(player_data$club)
        player_team_name <- team_name_mapping[[player_team_code]]
        if (!is.null(player_team_name) && player_team_name %in% names(team_colors)) {
          player_color <- team_colors[player_team_name]
        } else {
          # Assign a default color if team name not found
          player_color <- "black"
        }
      }
      
      # Get the current position
      x_pos <- player_data$x
      y_pos <- player_data$y
      
      if (player == "football") {
        # Draw the football as a standard point
        dynamic_grob <- addGrob(dynamic_grob,
                                pointsGrob(x = unit(x_pos, "native"),
                                           y = unit(y_pos, "native"),
                                           pch = 16, size = unit(2, "native"),
                                           gp = gpar(col = player_color)))
      } else {
        # Check if college_data is provided and retrieve the final_score
        if (!is.null(college_data)) {
          score <- college_data$final_score[college_data$name == player]
          if (length(score) > 0 && !is.na(score[1])) {
            player_final_score <- round(score[1], 1)
            # Draw a larger circle with the score inside
            dynamic_grob <- addGrob(dynamic_grob,
                                    circleGrob(x = unit(x_pos, "native"),
                                               y = unit(y_pos, "native"),
                                               r = unit(4, "mm"),
                                               gp = gpar(fill = player_color, col = player_color)))
            dynamic_grob <- addGrob(dynamic_grob,
                                    textGrob(label = player_final_score,
                                             x = unit(x_pos, "native"),
                                             y = unit(y_pos, "native"),
                                             gp = gpar(col = "white", fontsize = 8, fontface = "bold")))
          } else {
            # Draw the standard point for players without a score
            dynamic_grob <- addGrob(dynamic_grob,
                                    pointsGrob(x = unit(x_pos, "native"),
                                               y = unit(y_pos, "native"),
                                               pch = 16, size = unit(2, "native"),
                                               gp = gpar(col = player_color)))
          }
        } else {
          # Draw the standard point if college_data is not provided
          dynamic_grob <- addGrob(dynamic_grob,
                                  pointsGrob(x = unit(x_pos, "native"),
                                             y = unit(y_pos, "native"),
                                             pch = 16, size = unit(2, "native"),
                                             gp = gpar(col = player_color)))
        }
      }
      
      # Highlighting logic
      if (player != "football" && !is.null(college_data)) {
        if (player %in% top_offense_players || player %in% top_defense_players) {
          # Highlight with green circle
          dynamic_grob <- addGrob(dynamic_grob,
                                  circleGrob(x = unit(x_pos, "native"),
                                             y = unit(y_pos, "native"),
                                             r = unit(6, "mm"),
                                             gp = gpar(fill = NA, col = "green", lwd = 2)))
        } else if (player %in% low_offense_players || player %in% low_defense_players) {
          # Highlight with red circle
          dynamic_grob <- addGrob(dynamic_grob,
                                  circleGrob(x = unit(x_pos, "native"),
                                             y = unit(y_pos, "native"),
                                             r = unit(6, "mm"),
                                             gp = gpar(fill = NA, col = "red", lwd = 2)))
        }
      }
    }
    
    # Update gameClock
    # Calculate the decrement based on frame position
    # Assuming each frame represents 0.1 seconds
    # Total decrement = frame * 0.1
    # Alternatively, adjust based on actual frame rate
    # Here, we decrement by 0.1 seconds per frame
    # You can adjust the decrement value as needed
    
    # Calculate how many frames have been processed so far
    current_frame_index <- which(frame_ids == frame)
    
    # Total decrement in seconds
    total_decrement <- (current_frame_index - 1) * 0.1  # starting from frame 1
    
    # Updated total seconds
    updated_total_seconds <- max(total_seconds - total_decrement, 0)
    
    # Convert back to "M:SS" format
    updated_gameClock_str <- seconds_to_time(updated_total_seconds)
    
    # Add gameClock label to dynamic_grob
    dynamic_grob <- addGrob(dynamic_grob,
                            textGrob(label = paste("Game Clock:", updated_gameClock_str),
                                     x = unit(60, "native"),  # Centered horizontally
                                     y = unit(y_max - 3, "native"),  # Positioned near the top
                                     gp = gpar(col = "black", fontsize = 12, fontface = "bold")))
    
    # Draw the dynamic grob
    grid.draw(dynamic_grob)
    
    Sys.sleep(0.1)  # Adjust the sleep time as needed for animation speed
  }
  
  popViewport()  # pop dynamic_vp
  popViewport()  # pop main_vp
}

# Example function call
# Ensure that 'data', 'plays', 'games', and 'college_data' data frames are loaded and properly structured
plot_play_movement(2022101603, 346, data, plays, games, college_data, 
                    num_high_offense = 2, num_low_offense = 1, 
                    num_high_defense = 2, num_low_defense = 1)
