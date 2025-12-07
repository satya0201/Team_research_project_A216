x <- data$Walc   # Weekend alcohol consumption
y <- data$G3     # Final grade

# 3. Scatter plot with regression line

plot(
  x, y,
  xlab = "Weekend Alcohol Consumption (Walc)",
  ylab = "Final Grade (G3)",
  main = "Scatter Plot with Regression Line: Walc vs G3",
  pch = 19
)

# Regression model
model <- lm(G3 ~ Walc, data = data)
abline(model, lwd = 2)

cor.test(x, y, method = "pearson")
