# Load required libraries
library(ggplot2)
library(dplyr)


# 1. DATA LOADING AND PREPARATION

cat("=== LOADING DATASET ===\n\n")

# Read dataset
data <- read.csv("student-mat.csv")

# Display dataset structure
cat("--- STUDENT ALCOHOL DATASET ---\n")
cat("Columns:", paste(colnames(data), collapse=", "), "\n")
cat("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n\n")

str(data)
summary(data)

# checking the unique values of each columns.
for (col in colnames(data)){cat(col,":",length(unique(data[[col]])), "unique values", sep = "")}

# Check for missing values
cat("\n--- MISSING VALUES CHECK ---\n")
cat("Missing values in dataset:", sum(is.na(data)), "\n\n")

hist(data$Walc,
     breaks = seq(0.5, 5.5, by = 1),
     col = "Blue",
     main = "Weekend Alcohol Consumption (Walc)",
     xlab = "Level (1 = very low, 5 = very high)",
     ylab = "Count",
     border = "black")


# Assign colors by sex
sex_colors <- ifelse(data$sex == "F", "red", "blue")

# Plot scatter
plot(data$Walc, data$G3,
     col = adjustcolor(sex_colors, alpha.f = 0.7),
     pch = 19,
     main = "Weekend Alcohol Consumption vs Final Grade",
     xlab = "Weekend Alcohol (Walc)",
     ylab = "Final Grade (G3)")

# Add legend
legend("topright",
       legend = c("Female", "Male"),
       col = c("red", "blue"),
       pch = 19)


# Sex distribution (proportions)
print("Sex distribution:\n")
print(prop.table(table(data$sex)))

# Weekend alcohol consumption distribution (proportions)
print("\nWeekend alcohol distribution:\n")
print(prop.table(table(data$Walc)))


