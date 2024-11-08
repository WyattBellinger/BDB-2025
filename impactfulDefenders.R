library(dplyr)
library(stringr)
library(ggplot2)

plays_defenders <- plays %>%
  mutate(defenders = str_extract(playDescription, "\\(([A-Za-z\\.\\s;'-]+)\\)\\.?$")) %>%
    filter(!is.na(defenders)) %>%
      mutate(
        defenders = str_replace_all(defenders, "[\\(\\)]", ""),
        defenders = str_replace_all(defenders, "(\\b[A-Z])\\.", "\\1\\. "),
      )

defender_list <- strsplit(plays_defenders$defenders, ";\\s*")
defenders_all <- data.frame(defenders = unlist(defender_list))

defender_counts <- defenders_all %>%
  group_by(defenders) %>%
    summarise(count = n()) %>%
      arrange(desc(count)) %>%
        slice_head(n = 10)

ggplot(defender_counts, aes(x = reorder(defenders, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Defenders by Number of Impacts",
    x = "Defender",
    y = "Number of Impacts"
  ) +
  theme_minimal()
