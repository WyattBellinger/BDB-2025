ggplot(players) +
  aes(x = reorder(position, weight, median), y = weight) +
  geom_boxplot(fill = 'lightblue') +
  labs(title = "Distribution of Weight by Position",
       x = "Position",
       y = "Weight (lbs)")