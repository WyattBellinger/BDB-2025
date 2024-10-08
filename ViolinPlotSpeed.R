library(ggplot2)

max_speeds <- aggregate(s ~ nflId, data = data, FUN = max)
max_speeds <- subset(max_speeds, max_speeds$s < 18)
max_speeds_with_positions <- merge(max_speeds, players, by = "nflId")
ggplot(max_speeds_with_positions, aes(x = position, y = s)) +
  geom_violin(trim = FALSE, fill = "skyblue", color = "darkblue") +
  labs(title = "Distribution of Max Speeds by Position",
       x = "Position",
       y = "Max Speed (yars/sec)") +
  theme_minimal()