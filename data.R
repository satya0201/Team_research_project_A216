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

# 2. DATA CLEANING AND SELECTION

cat("=== DATA CLEANING ===\n\n")

# Remove missing values (if any)
data_clean <- na.omit(data)
cat("Rows after removing NA:", nrow(data_clean), "\n\n")

# Select variables relevant to research question
data_clean <- data_clean[, c("sex", "age", "studytime", "failures", "goout","Dalc", "Walc", "G1", "G2", "G3")]

# Renaming columns
colnames(data_clean)[colnames(data_clean) == "Dalc"] <- "Weekday_Alcohol"
colnames(data_clean)[colnames(data_clean) == "Walc"] <- "Weekend_Alcohol"
colnames(data_clean)[colnames(data_clean) == "G3"]   <- "Final_Grade"

cat("--- FINAL CLEANED DATASET ---\n")
str(data_clean)
head(data_clean, 20)


# 3. DESCRIPTIVE STATISTICS

cat("\n\n=== DESCRIPTIVE STATISTICS ===\n\n")

# Weekday Alcohol (Dalc)
cat("--- WEEKDAY ALCOHOL (Dalc) ---\n")
cat("Mean:", round(mean(data_clean$Weekday_Alcohol), 2), "\n")
cat("SD:", round(sd(data_clean$Weekday_Alcohol), 2), "\n")
cat("Median:", median(data_clean$Weekday_Alcohol), "\n")
cat("Min:", min(data_clean$Weekday_Alcohol), "\n")
cat("Max:", max(data_clean$Weekday_Alcohol), "\n\n")

# Weekend Alcohol (Walc)
cat("--- WEEKEND ALCOHOL (Walc) ---\n")
cat("Mean:", round(mean(data_clean$Weekend_Alcohol), 2), "\n")
cat("SD:", round(sd(data_clean$Weekend_Alcohol), 2), "\n")
cat("Median:", median(data_clean$Weekend_Alcohol), "\n")
cat("Min:", min(data_clean$Weekend_Alcohol), "\n")
cat("Max:", max(data_clean$Weekend_Alcohol), "\n\n")

# Final Grade (G3)
cat("--- FINAL GRADE (G3) ---\n")
cat("Mean:", round(mean(data_clean$Final_Grade), 2), "\n")
cat("SD:", round(sd(data_clean$Final_Grade), 2), "\n")
cat("Median:", median(data_clean$Final_Grade), "\n")
cat("Min:", min(data_clean$Final_Grade), "\n")
cat("Max:", max(data_clean$Final_Grade), "\n\n")

# 4. VISUALIZATIONS 
cat("=== CREATING VISUALIZATIONS ===\n\n")

# Assign colors by sex
sex_colors <- ifelse(data_clean$sex == "F", "red", "blue")

# Plot scatter
plot(data_clean$Weekend_Alcohol, data_clean$G3,
     col = adjustcolor(sex_colors, alpha.f = 0.7),
     pch = 19,
     main = "Weekend Alcohol Consumption vs Final Grade",
     xlab = "Weekend Alcohol (Walc)",
     ylab = "Final Grade (G3)")
legend("topright",
       legend = c("Female", "Male"),
       col = c("red", "blue"),
       pch = 19)

hist(data_clean$Weekday_Alcohol,
     main = "Distribution of Weekday Alcohol Consumption",
     xlab = "Dalc Level",
     ylab = "Frequency",
     col = "steelblue",
     border = "white",
     breaks = seq(0.5, 5.5, by = 1))

cat("Displayed: Histogram of Weekday Alcohol\n")

# Sex distribution (proportions)
print("Sex distribution:\n")
print(prop.table(table(data_clean$sex)))

# Weekend alcohol consumption distribution (proportions)
print("\nWeekend alcohol distribution:\n")
print(prop.table(table(data_clean$Weekend_Alcohol)))

# Weekday alcohol consumption distribution (proportions)
print("\nWeekday alcohol distribution:\n")
print(prop.table(table(data_clean$Weekday_Alcohol)))

# 5. CORRELATION ANALYSIS

cat("\n=== PEARSON CORRELATION TESTS ===\n\n")

# Weekend alcohol vs grades
cor_weekend <- cor.test(data_clean$Weekend_Alcohol,
                        data_clean$Final_Grade,
                        method="pearson")

# Weekday alcohol vs grades
cor_weekday <- cor.test(data_clean$Weekday_Alcohol,
                        data_clean$Final_Grade,
                        method="pearson")

cat("--- CORRELATION RESULTS ---\n")
cat("Weekend Alcohol vs Final Grade (r):", round(cor_weekend$estimate, 4), "\n")
cat("P-value:", cor_weekend$p.value, "\n\n")

cat("Weekday Alcohol vs Final Grade (r):", round(cor_weekday$estimate, 4), "\n")
cat("P-value:", cor_weekday$p.value, "\n\n")



