library(shiny)
library(farver)
library(teamcolors)
library(grid)

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

position_mapping <- c("QB", "T", "TE", "WR", "DE", "NT", "SS", "FS", 
                      "G", "OLB", "DT", "CB", "RB", "C", "ILB", "MLB", 
                      "FB", "LS", "DB")

hex_to_rgb <- function(hex) {
  rgb <- col2rgb(hex) / 255
  return(as.numeric(rgb))
}

lab_distance <- function(color1, color2) {
  rgb1 <- as.numeric(col2rgb(color1))
  rgb2 <- as.numeric(col2rgb(color2))
  
  lab1 <- farver::convert_colour(matrix(rgb1, nrow = 1), from = "rgb", to = "lab")
  lab2 <- farver::convert_colour(matrix(rgb2, nrow = 1), from = "rgb", to = "lab")
  
  return(sqrt(sum((lab1 - lab2) ^ 2)))
}

nfl_teams <- subset(teamcolors, league == "nfl")
nfl_team_colors <- setNames(nfl_teams$primary, nfl_teams$name)
nfl_team_secondary_colors <- setNames(nfl_teams$secondary, nfl_teams$name)

time_to_seconds <- function(time_str) {
  parts <- unlist(strsplit(time_str, ":"))
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(parts[2])
  total_seconds <- minutes * 60 + seconds
  return(total_seconds)
}

seconds_to_time <- function(total_seconds) {
  if (total_seconds < 0) total_seconds <- 0
  minutes <- floor(total_seconds / 60)
  seconds <- floor(total_seconds %% 60)
  time_str <- sprintf("%d:%02d", minutes, seconds)
  return(time_str)
}

plot_play_movement <- function(game_id, play_id, data, plays, games, college_data = NULL, 
                               num_high_offense = 0, num_low_offense = 0, 
                               num_high_defense = 0, num_low_defense = 0,
                               highlight_position_number = NULL,
                               top_age_n = 0,
                               top_height_n = 0,
                               top_weight_n = 0,
                               frame_number = NULL,
                               new_page = TRUE) {
  
  if (new_page) {
    grid.newpage()
  }
  if (!is.null(highlight_position_number)) {
    if (!is.numeric(highlight_position_number) || 
        highlight_position_number < 1 || 
        highlight_position_number > length(position_mapping)) {
      stop(paste("highlight_position_number must be an integer between 1 and", length(position_mapping)))
    }
    highlight_position <- position_mapping[highlight_position_number]
  } else {
    highlight_position <- NULL
  }
  
  if (!is.numeric(top_age_n) || top_age_n < 0) {
    stop("top_age_n must be a non-negative integer.")
  }
  if (!is.numeric(top_height_n) || top_height_n < 0) {
    stop("top_height_n must be a non-negative integer.")
  }
  if (!is.numeric(top_weight_n) || top_weight_n < 0) {
    stop("top_weight_n must be a non-negative integer.")
  }
  
  play_data <- subset(data, gameId == game_id & playId == play_id)
  
  if (nrow(play_data) == 0) {
    stop("No data available for the specified gameId and playId.")
  }
  
  play_info <- subset(plays, gameId == game_id & playId == play_id)
  
  if (nrow(play_info) == 0) {
    stop("No matching play found in 'plays' data frame.")
  }
  
  possession_team_code <- play_info$possessionTeam[1]
  defensive_team_code <- play_info$defensiveTeam[1]
  
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
  
  possession_team <- team_name_mapping[[possession_team_code]]
  defensive_team <- team_name_mapping[[defensive_team_code]]
  
  team_colors <- nfl_team_colors[c(possession_team, defensive_team)]
  names(team_colors) <- c(possession_team, defensive_team)
  
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
  
  if (!is.null(frame_number)) {
    
    if (frame_number < min(frame_ids) || frame_number > max(frame_ids)) {
      stop(paste("frame_number must be between", min(frame_ids), "and", max(frame_ids)))
    }
    frame_ids <- frame_number
  }
  
  pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, widths = unit(c(4, 1), "null"))))
  
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  
  padding_left <- unit(5, "mm")
  padding_right <- unit(5, "mm")
  padding_bottom <- unit(5, "mm")
  padding_top <- unit(5, "mm")
  
  pushViewport(viewport(x = 0.5, y = 0.5, 
                        width = unit(1, "npc") - (padding_left + padding_right),
                        height = unit(1, "npc") - (padding_bottom + padding_top),
                        xscale = c(0, 120), yscale = c(-5, 58.3), name = "main_vp"))
  
  grid.rect(x = unit(0, "native"), y = unit(0, "native"), 
            width = unit(120, "native"), height = unit(53.3, "native"),
            just = c("left", "bottom"), gp = gpar(fill = "white", col = "darkgreen", lwd = 2))
  
  grid.rect(x = unit(0, "native"), y = unit(0, "native"), 
            width = unit(10, "native"), height = unit(53.3, "native"),
            just = c("left", "bottom"), gp = gpar(fill = NA, col = "black", lwd = 2))
  grid.rect(x = unit(110, "native"), y = unit(0, "native"), 
            width = unit(10, "native"), height = unit(53.3, "native"),
            just = c("left", "bottom"), gp = gpar(fill = NA, col = "black", lwd = 2))
  
  yard_labels <- c(10, 20, 30, 40, 50, 40, 30, 20, 10)
  yard_positions <- seq(20, 100, by = 10)
  
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
  
  title_label <- paste0(offense_team, ": ", offense_score, " vs. ", 
                        defense_team, ": ", defense_score)
  grid.text(label = title_label,
            x = unit(60, "native"), y = unit(58.3 - 1, "native"),
            gp = gpar(fontsize = 14, fontface = "bold"))
  
  initial_gameClock_str <- play_info$gameClock[1]
  total_seconds <- time_to_seconds(initial_gameClock_str)
  
  updated_gameClock_str <- seconds_to_time(total_seconds - (frame_ids - 1) * 0.1)
  
  game_clock_grob <- textGrob(label = paste("Game Clock:", updated_gameClock_str),
                              x = unit(60, "native"), y = unit(58.3 - 3, "native"),  # Positioned below the title
                              gp = gpar(col = "black", fontsize = 12, fontface = "bold"),
                              name = "game_clock")
  grid.draw(game_clock_grob)
  
  popViewport()
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
  
  legend_padding <- unit(5, "mm")
  
  legend_frame <- frameGrob(name = "legend_frame", vp = viewport(width = unit(1, "npc") - 2 * legend_padding, 
                                                                 height = unit(1, "npc") - 2 * legend_padding))
  
  legend_labels <- c(possession_team, defensive_team, "Football")
  legend_colors <- c(team_colors, football_color)
  
  highlight_types <- c("Highest Athleticism", "Lowest Athleticism", "Position Highlight",
                       "Top Age", "Top Height", "Top Weight")
  highlight_colors <- c("green", "red", "yellow", "blue", "orange", "purple")
  
  legend_labels <- c(legend_labels, highlight_types)
  legend_colors <- c(legend_colors, highlight_colors)
  
  legend_items <- list()
  
  for (i in seq_along(legend_labels)) {
    
    if (i <= length(c(possession_team, defensive_team, "Football"))) {
      
      symbol_grob <- linesGrob(x = unit.c(unit(0, "mm"), unit(10, "mm")), y = unit(0.5, "npc"), 
                               gp = gpar(col = legend_colors[i], lwd = 2))
    } else {
      symbol_grob <- circleGrob(x = unit(5, "mm"), y = unit(0.5, "npc"), r = unit(3, "mm"),
                                gp = gpar(fill = legend_colors[i], col = legend_colors[i]))
    }
    
    label_grob <- textGrob(label = legend_labels[i], x = unit(15, "mm"), y = unit(0.5, "npc"),
                           just = "left", gp = gpar(col = "black", fontsize = 10))
    
    legend_item <- gTree(children = gList(symbol_grob, label_grob))
    
    legend_items[[i]] <- legend_item
  }
  
  for (i in seq_along(legend_items)) {
    legend_frame <- packGrob(legend_frame, legend_items[[i]], side = "top", height = unit(1, "null"))
  }
  
  grid.draw(legend_frame)
  
  popViewport() 
  
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  pushViewport(viewport(x = 0.5, y = 0.5, 
                        width = unit(1, "npc") - (padding_left + padding_right),
                        height = unit(1, "npc") - (padding_bottom + padding_top),
                        xscale = c(0, 120), yscale = c(-5, 58.3), name = "dynamic_vp"))
  
  frame <- frame_ids
  
  frame_data <- subset(play_data, frameId == frame)
  
  if (nrow(frame_data) == 0) {
    stop(paste("No data available for frame number", frame_number))
  }
  
  unique_players <- unique(frame_data$displayName)
  
  top_age_players <- character(0)
  top_height_players <- character(0)
  top_weight_players <- character(0)
  
  dynamic_grob <- gTree(name = "dynamic_content")
  
  offense_players <- subset(frame_data, club == possession_team_code & displayName != "football")
  defense_players <- subset(frame_data, club == defensive_team_code & displayName != "football")
  
  if (!is.null(college_data)) {
    
    offense_players <- merge(offense_players, college_data, by.x = "displayName", by.y = "name", all.x = TRUE)
    defense_players <- merge(defense_players, college_data, by.x = "displayName", by.y = "name", all.x = TRUE)
    
    if (nrow(offense_players) > 0) {
      offense_scores <- aggregate(final_score ~ displayName, data = offense_players, FUN = mean, na.rm = TRUE)
      top_offense_players <- head(offense_scores[order(-offense_scores$final_score), "displayName"], num_high_offense)
      low_offense_players <- head(offense_scores[order(offense_scores$final_score), "displayName"], num_low_offense)
    } else {
      top_offense_players <- character(0)
      low_offense_players <- character(0)
    }
    
    if (nrow(defense_players) > 0) {
      defense_scores <- aggregate(final_score ~ displayName, data = defense_players, FUN = mean, na.rm = TRUE)
      top_defense_players <- head(defense_scores[order(-defense_scores$final_score), "displayName"], num_high_defense)
      low_defense_players <- head(defense_scores[order(defense_scores$final_score), "displayName"], num_low_defense)
    } else {
      top_defense_players <- character(0)
      low_defense_players <- character(0)
    }
  } else {
    top_offense_players <- character(0)
    low_offense_players <- character(0)
    top_defense_players <- character(0)
    low_defense_players <- character(0)
  }
  
  if (!is.null(college_data)) {
    
    current_players <- subset(frame_data, displayName != "football")
    current_players <- merge(current_players, college_data, by.x = "displayName", by.y = "name", all.x = TRUE)
    
    if (top_age_n > 0) {
      age_data <- current_players[!is.na(current_players$age), ]
      if (nrow(age_data) > 0) {
        top_age_players <- head(age_data[order(-age_data$age), "displayName"], top_age_n)
      }
    }
    
    if (top_height_n > 0) {
      height_data <- current_players[!is.na(current_players$heightIN), ]
      if (nrow(height_data) > 0) {
        top_height_players <- head(height_data[order(-height_data$heightIN), "displayName"], top_height_n)
      }
    }
    
    if (top_weight_n > 0) {
      weight_data <- current_players[!is.na(current_players$weight), ]
      if (nrow(weight_data) > 0) {
        top_weight_players <- head(weight_data[order(-weight_data$weight), "displayName"], top_weight_n)
      }
    }
    
    highlight_top_age_players <- top_age_players
    highlight_top_height_players <- top_height_players
    highlight_top_weight_players <- top_weight_players
  }
  
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
        player_color <- "black"
      }
    }
    
    x_pos <- player_data$x
    y_pos <- player_data$y
    
    if (player == "football") {
      dynamic_grob <- addGrob(dynamic_grob,
                              pointsGrob(x = unit(x_pos, "native"),
                                         y = unit(y_pos, "native"),
                                         pch = 16, size = unit(2, "native"),
                                         gp = gpar(col = player_color)))
    } else {
      if (!is.null(college_data)) {
        score <- college_data$final_score[college_data$name == player]
        player_position <- college_data$position[college_data$name == player]
        
        if (length(score) > 0 && !is.na(score[1])) {
          player_final_score <- round(score[1], 1)
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
          dynamic_grob <- addGrob(dynamic_grob,
                                  pointsGrob(x = unit(x_pos, "native"),
                                             y = unit(y_pos, "native"),
                                             pch = 16, size = unit(2, "native"),
                                             gp = gpar(col = player_color)))
        }
      } else {
        dynamic_grob <- addGrob(dynamic_grob,
                                pointsGrob(x = unit(x_pos, "native"),
                                           y = unit(y_pos, "native"),
                                           pch = 16, size = unit(2, "native"),
                                           gp = gpar(col = player_color)))
      }
    }
    
    if (player != "football" && !is.null(college_data) && !is.null(highlight_position)) {
      player_position <- college_data$position[college_data$name == player]
      if (length(player_position) > 0 && player_position[1] == highlight_position) {
        
        dynamic_grob <- addGrob(dynamic_grob,
                                circleGrob(x = unit(x_pos, "native"),
                                           y = unit(y_pos, "native"),
                                           r = unit(6, "mm"),
                                           gp = gpar(fill = NA, col = "yellow", lwd = 2)))
      }
    }
    
    if (player != "football" && !is.null(college_data)) {
      if (player %in% top_offense_players || player %in% top_defense_players) {
        dynamic_grob <- addGrob(dynamic_grob,
                                circleGrob(x = unit(x_pos, "native"),
                                           y = unit(y_pos, "native"),
                                           r = unit(6, "mm"),
                                           gp = gpar(fill = NA, col = "green", lwd = 2)))
      } else if (player %in% low_offense_players || player %in% low_defense_players) {
        dynamic_grob <- addGrob(dynamic_grob,
                                circleGrob(x = unit(x_pos, "native"),
                                           y = unit(y_pos, "native"),
                                           r = unit(6, "mm"),
                                           gp = gpar(fill = NA, col = "red", lwd = 2)))
      }
    }
    
    if (player != "football" && !is.null(college_data)) {
      
      if (top_age_n > 0 && player %in% highlight_top_age_players) {
        dynamic_grob <- addGrob(dynamic_grob,
                                circleGrob(x = unit(x_pos, "native"),
                                           y = unit(y_pos, "native"),
                                           r = unit(6, "mm"),
                                           gp = gpar(fill = NA, col = "blue", lwd = 2)))
      }
      
      if (top_height_n > 0 && player %in% highlight_top_height_players) {
        dynamic_grob <- addGrob(dynamic_grob,
                                circleGrob(x = unit(x_pos, "native"),
                                           y = unit(y_pos, "native"),
                                           r = unit(6, "mm"),
                                           gp = gpar(fill = NA, col = "orange", lwd = 2)))
      }
      
      if (top_weight_n > 0 && player %in% highlight_top_weight_players) {
        dynamic_grob <- addGrob(dynamic_grob,
                                circleGrob(x = unit(x_pos, "native"),
                                           y = unit(y_pos, "native"),
                                           r = unit(6, "mm"),
                                           gp = gpar(fill = NA, col = "purple", lwd = 2)))
      }
    }
  }
  
  grid.draw(dynamic_grob)
  
  popViewport()  
  popViewport() 
  popViewport()  
}

ui <- fluidPage(
  titlePanel("NFL Play Movement Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_high_offense", "Number of Highest Athleticism Offensive Players:",
                  min = 0, max = 3, value = 0),
      sliderInput("num_low_offense", "Number of Lowest Athleticism Offensive Players:",
                  min = 0, max = 3, value = 0),
      sliderInput("num_high_defense", "Number of Highest Athleticism Defensive Players:",
                  min = 0, max = 3, value = 0),
      sliderInput("num_low_defense", "Number of Lowest Athleticism Defensive Players:",
                  min = 0, max = 3, value = 0),
      selectInput("highlight_position_number", "Highlight Position:",
                  choices = setNames(1:length(position_mapping), position_mapping), selected = NULL),
      sliderInput("top_age_n", "Highlight Top N Oldest Players:",
                  min = 0, max = 3, value = 0),
      sliderInput("top_height_n", "Highlight Top N Tallest Players:",
                  min = 0, max = 3, value = 0),
      sliderInput("top_weight_n", "Highlight Top N Heaviest Players:",
                  min = 0, max = 3, value = 0),
      actionButton("prev_frame", "Previous Frame"),
      actionButton("next_frame", "Next Frame"),
      br(),
      textOutput("frame_info")
    ),
    
    mainPanel(
      plotOutput("playPlot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(frame = 1)
  
  total_frames <- reactive({
    max(subset(data, gameId == 2022101603 & playId == 346)$frameId, na.rm = TRUE)
  })
  
  observeEvent(input$next_frame, {
    if (rv$frame < total_frames()) {
      rv$frame <- rv$frame + 1
    }
  })
  
  observeEvent(input$prev_frame, {
    if (rv$frame > 1) {
      rv$frame <- rv$frame - 1
    }
  })
  
  output$frame_info <- renderText({
    paste("Current Frame:", rv$frame, "of", total_frames())
  })
  
  output$playPlot <- renderPlot({
    req(data, plays, games, college_data)
    
    frame <- rv$frame
    print(paste("Rendering frame:", frame))
    
    plot_play_movement(
      game_id = 2022101603, 
      play_id = 346, 
      data = data, 
      plays = plays, 
      games = games, 
      college_data = college_data, 
      num_high_offense = input$num_high_offense, 
      num_low_offense = input$num_low_offense, 
      num_high_defense = input$num_high_defense, 
      num_low_defense = input$num_low_defense,
      highlight_position_number = as.numeric(input$highlight_position_number),
      top_age_n = input$top_age_n,
      top_height_n = input$top_height_n,
      top_weight_n = input$top_weight_n,
      frame_number = frame,
      new_page = FALSE
    )
  }, height = 600, width = 800)
}



shinyApp(ui = ui, server = server)

