library(ggplot2)

# Create a data frame (ggplot prefers data frames over loose vectors)
data <- data.frame(
  x = c(2, 4, 6, 8, 10),
  y = c(1, 2, 3, 4, 5)
)

# Build the ggplot
my_plot <- ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 4) +
  labs(
    title = "My Medicaid Plot",
    x = "X Axis",
    y = "Y Axis"
  ) +
  theme_minimal()

# Save the plot as a PNG file (ggsave handles the file creation automatically)
ggsave("my_plot.png", plot = my_plot, width = 8, height = 6, dpi = 100)
