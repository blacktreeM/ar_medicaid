# Save the plot as a PNG file
png("my_plot.png", width = 800, height = 600)

y = c(1, 2, 3, 4, 5)
x = c(2, 4, 6, 8, 10)
plot(x, y, main = "My Medicaid Plot", xlab = "X Axis", ylab = "Y Axis", col = "blue", pch = 19)

dev.off()
