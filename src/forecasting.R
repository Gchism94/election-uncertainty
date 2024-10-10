# Load necessary libraries
library(tidyverse)

# Read the dataset
data <- read.csv("data/generic_ballot_polls.csv")
data
# Convert 'election_date' to a Date type and filter for necessary columns
data_long <- data %>%
  mutate(created_at = as.Date(created_at, format = "%m/%d/%y")) |>
  filter(!is.na(dem) & !is.na(rep)) |>
  select(created_at, dem, rep) |>
  pivot_longer(cols = c("dem","rep"), names_to = "party", values_to = "poll") |>
  group_by(created_at, party) 


# Create the plot
ggplot(data_long, aes(x = created_at, y = poll, color = party)) +
  geom_smooth(alpha = 0.1, linewidth = 0.5, method = "loess") +
  geom_point(alpha = 0.1) +
  scale_color_manual(values = c("dem" = "blue", "rep" = "red")) +
  theme_minimal() +
  scale_x_date(limits = as.Date(c("2024-07-15", "2024-10-07"))) +
  theme_void() +
  theme(legend.position = "none")
 
ggsave(filename = "images/election-uncertainty.png", plot = last_plot(), width = 6, height = 10, units = "in", dpi = 400)
