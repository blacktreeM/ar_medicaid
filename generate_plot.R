library(ggplot2)

# 1. Create a realistic, mock Medicaid dataset
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
months_factor <- factor(months, levels = months) # Keeps months in calendar order

data_children <- data.frame(
  Month = months_factor,
  Enrollment = c(320, 322, 325, 324, 328, 330, 335, 333, 331, 336, 338, 342),
  Category = "Children's Program"
)

data_adults <- data.frame(
  Month = months_factor,
  Enrollment = c(210, 212, 215, 213, 211, 216, 218, 220, 219, 223, 225, 228),
  Category = "Adult Expansion"
)

# Combine into a single data frame
medicaid_data <- rbind(data_children, data_adults)

# 2. Build a complex, polished ggplot
complex_plot <- ggplot(medicaid_data, aes(x = Month, y = Enrollment, group = Category, color = Category)) +
  # Add trend lines
  geom_line(linewidth = 1.2) +
  # Add data points on top of lines
  geom_point(size = 3) +
  # Custom clean colors
  scale_color_manual(values = c("Children's Program" = "#0072B2", "Adult Expansion" = "#D55E00")) +
  # Polish titles and text labels
  labs(
    title = "Arkansas Medicaid Enrollment Trends",
    subtitle = "Monthly tracking across major program categories",
    x = "Month",
    y = "Total Enrollment (Thousands)",
    caption = "Source: Mock AR Medicaid Automated Pipeline"
  ) +
  # Apply a crisp layout theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#222222"),
    plot.subtitle = element_text(size = 12, color = "#666666", margin = margin(b = 15)),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank() # Remove vertical gridlines for a cleaner look
  )

# 3. Save the high-resolution visualization
ggsave("my_plot.png", plot = complex_plot, width = 10, height = 6.5, dpi = 150)
