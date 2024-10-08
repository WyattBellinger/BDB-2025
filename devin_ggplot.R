library(ggplot2)

setwd("/Users/devin/Desktop/Stat405/final_project")
data <- read.csv('nfl_data.csv')


ggplot(data = diamonds) + aes(carat) + facet_grid(color ~ .) +
  geom_histogram(binwidth = 0.1) + xlim(c(0, 3))


foot <- data[data$displayName == "football", ]

foot <- foot[foot$event == "pass_outcome_caught", ]
foot <- foot[!apply(is.na(foot), 1, all), ]

foot$x <- floor(foot$x / 2) * 2
foot$y <- floor(foot$y / 2) * 2

catch_counts <- aggregate(cbind(catches = rep(1, nrow(foot))), 
                          by = list(x = foot$x, y = foot$y), FUN = sum)

ggplot(catch_counts, aes(x = x, y = y, fill = catches)) + geom_tile() + 
  scale_fill_gradient(low = "white", high = "blue") + 
  coord_fixed(ratio = 1) +
  labs(title = "Heatmap of NFL Passes Caught in 2x2 boxes",
       x = "Field Length",
       y = "Field Width")


football <- data[data$displayName == "football", ]


