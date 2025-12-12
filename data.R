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

# Check for missing values
cat("\nMissing values per column:\n")
print(colSums(is.na(data)))

# checking the unique values of each columns.
for (col in colnames(data)){
  cat(col,":",length(unique(data[[col]])), "\n", sep = "")
}



# 2. DATA CLEANING AND SELECTION

cat("=== DATA CLEANING ===\n\n")

# Remove missing values (if any)
data_clean <- na.omit(data)
cat("Rows after removing NA:", nrow(data_clean), "\n\n")

# Select variables relevant to research question
data_clean <- data_clean[, c("school","sex", "age", "studytime", "failures", "goout","Dalc", "Walc", "G1", "G2", "G3")]

num_cols <- c("age","studytime","failures","goout",
              "Dalc","Walc","G1","G2","G3")

for (col in num_cols) {
  data_clean[[col]] <- as.numeric(data_clean[[col]])
}

# Renaming columns
colnames(data_clean)[colnames(data_clean) == "Dalc"] <- "Weekday_Alcohol"
colnames(data_clean)[colnames(data_clean) == "Walc"] <- "Weekend_Alcohol"
colnames(data_clean)[colnames(data_clean) == "G3"]   <- "Final_Grade"

cat("--- FINAL CLEANED DATASET ---\n")
str(data_clean)
head(data_clean, 8)


# 3. DESCRIPTIVE STATISTICS

cat("\n\n=== DESCRIPTIVE STATISTICS ===\n\n")

# Weekday Alcohol (Dalc)
describe_var <- function(x, label){
  cat("\n--", label, "--\n")
  cat("n:", sum(!is.na(x)), "\n")
  cat("Mean:", round(mean(x, na.rm = TRUE), 2), "\n")
  cat("SD:", round(sd(x, na.rm = TRUE), 2), "\n")
  cat("Median:", median(x, na.rm = TRUE), "\n")
  cat("Min:", min(x, na.rm = TRUE), "\n")
  cat("Max:", max(x, na.rm = TRUE), "\n")
}

describe_var(data_clean$Weekday_Alcohol, "Weekday Alcohol (Dalc)")
describe_var(data_clean$Weekend_Alcohol, "Weekend Alcohol (Walc)")
describe_var(data_clean$Final_Grade, "Final Grade (G3)")


# 4. VISUALIZATIONS 
cat("=== CREATING VISUALIZATIONS ===\n\n")

# scatter plot categorised by sex (your original graph)

sex_colors <- ifelse(data_clean$sex == "F", "red", "blue")


plot(data_clean$Weekend_Alcohol, data_clean$Final_Grade,
     col = adjustcolor(sex_colors, alpha.f = 0.7),
     pch = 19,
     main = "Weekend Alcohol Consumption vs Final Grade",
     xlab = "Weekend Alcohol (Walc)",
     ylab = "Final Grade (G3)")
legend("topright",
       legend = c("Female", "Male"),
       col = c("red", "blue"),
       pch = 19)


# Histogram plot of Weekday Alcohol.

par(mfrow = c(1, 2))

hist(data_clean$Weekday_Alcohol,
     main = "Distribution of Weekday Alcohol Consumption",
     xlab = "Dalc Level",
     ylab = "Frequency",
     col = "steelblue",
     border = "black",
     breaks = seq(0.5, 5.5, by = 1))

cat("Displayed: Histogram of Weekday Alcohol\n")

hist(data_clean$Weekend_Alcohol,
     main = "Distribution of Weekend Alcohol Consumption",
     xlab = "Walc Level",
     ylab = "Frequency",
     col = "lightblue",
     border = "black",
     breaks = seq(0.5, 5.5, by = 1))
par(mfrow = c(1, 1))


# distribution of final grade(g3) histogram plot.
hist(data_clean$G3,breaks = 10,col = "blue",main = "Distribution of Final Grades(G3)", xlab = "Final Grade", ylab = "Count",freq = TRUE, border = "Black")
dens<-density(data_clean$G3)
scaled_y <- dens$y * diff(hist(data_clean$G3, plot = FALSE)$breaks[1:2]) * length(data_clean$G3)
lines(dens$x, scaled_y, lwd = 2)



#Box plot for final grade by study time.

boxplot(split(data_clean$Final_Grade, data_clean$studytime),
        main = "Final Grade by Study Time",
        xlab = "Study Time (1 = low, 4 = high)",
        ylab = "Final Grade",
        col = c("lightblue","lightgreen","khaki","lightpink"))


# Boxplot for final grades and no of failures
boxplot(split(data_clean$Final_Grade, data_clean$failures),
        main = "Final Grade by Number of Past Failures",
        xlab = "Failures",
        ylab = "Final Grade",
        col = "lightcoral")

# barplot for Average alcohol consumption on weekdays and weekends with respect to final grade.
par(mfrow = c(1, 2))

avg_grades_walc <- tapply(data_clean$Final_Grade, data_clean$Weekend_Alcohol, mean)

barplot(avg_grades_walc,
        main = "Average Final Grade by Weekend Alcohol Level",
        xlab = "Weekend Alcohol (Walc)",
        ylab = "Average Grade",
        col = "steelblue")

avg_grades_dalc <- tapply(data_clean$Final_Grade, data_clean$Weekday_Alcohol, mean)

barplot(avg_grades_dalc,
        main = "Average Final Grade by Weekday Alcohol Level",
        xlab = "Weekday Alcohol (Dalc)",
        ylab = "Average Grade",
        col = "steelblue")
par(mfrow = c(1, 1))

# Barplot for weekend drink by going out.
avg_walc_goout <- tapply(data_clean$Weekend_Alcohol, data_clean$goout, mean)

barplot(avg_walc_goout,
        main = "Average Weekend Drinking by Going-Out Level",
        xlab = "Going Out (1–5)",
        ylab = "Average Weekend Alcohol Use",
        col = "orange")

# barplot for Average alcohol consumption on weekdays and weekends with Sex.

par(mfrow = c(1, 2))
barplot(tapply(data_clean$Weekend_Alcohol, data_clean$sex, mean),
        main = "Average Weekend Alcohol Consumption by Sex",
        xlab = "Sex",
        ylab = "Average Walc",
        col=c("pink","skyblue"))

barplot(tapply(data_clean$Weekday_Alcohol, data_clean$sex, mean),
        main = "Average Weekday Alcohol Consumption by Sex",
        xlab = "Sex",
        ylab = "Average Dalc",
        col=c("pink","skyblue"))
par(mfrow = c(1, 1))

# Count total failures per school
failures_by_school <- tapply(data_clean$failures, data_clean$school, sum)
failures_by_school
# Pie chart for total failures by school
par(mfrow = c(1, 2))
pie(failures_by_school,
    labels = paste(names(failures_by_school),
                   "(", failures_by_school, "failures)", sep=""),
    main = "Total Number of Student Failures by School",
    col = c("lightblue", "lightgreen"))

# Pie chart for total passed students % by school

zero_fail <- tapply(data_clean$failures == 0, data_clean$school, sum)
pie(zero_fail,
    labels = paste(names(zero_fail), " (", zero_fail, " zero failures)", sep=""),
    main = "Students With Zero Failures by School",
    col = c("pink","skyblue"))
par(mfrow = c(1, 1))


# Combined Scatter plot Comparing Dalc & Walc Together

# Calculate mean final grade for each alcohol level
avg_dalc <- tapply(data_clean$Final_Grade, data_clean$Weekday_Alcohol, mean)
avg_walc <- tapply(data_clean$Final_Grade, data_clean$Weekend_Alcohol, mean)

# Combine into matrix for grouped barplot
grade_matrix <- rbind(avg_dalc, avg_walc)

# Create grouped barplot
barplot(grade_matrix,
        beside = TRUE,
        col = c("skyblue", "lightgreen"),
        main = "Final Grade vs Alcohol Consumption (Weekday vs Weekend)",
        xlab = "Alcohol Consumption Level (1–5)",
        ylab = "Average Final Grade",
        legend.text = c("Weekday Alcohol (Dalc)", "Weekend Alcohol (Walc)"))


cat("Displayed: Combined Scatterplot of Alcohol Consumption vs Final Grade\n")


# 5. CORRELATION ANALYSIS

cat("\n=== 5. CORRELATION ANALYSIS ===\n")

# Prepare complete-case masks
idx_wknd <- complete.cases(data_clean$Weekend_Alcohol, data_clean$Final_Grade)
idx_wkdy <- complete.cases(data_clean$Weekday_Alcohol, data_clean$Final_Grade)

n_wknd <- sum(idx_wknd)
n_wkdy <- sum(idx_wkdy)
cat("N (WeekendAlcohol vs FinalGrade):", n_wknd, "\n")
cat("N (WeekdayAlcohol vs FinalGrade):", n_wkdy, "\n\n")

# SPEARMAN CORRELATION TEST
spearman_wknd <- cor.test(data_clean$Weekend_Alcohol[idx_wknd],
                          data_clean$Final_Grade[idx_wknd],
                          method = "spearman", exact = FALSE)

spearman_wkdy <- cor.test(data_clean$Weekday_Alcohol[idx_wkdy],
                          data_clean$Final_Grade[idx_wkdy],
                          method = "spearman", exact = FALSE)

cat("-- Spearman Results --\n")
cat("Weekend Alcohol vs Final Grade: Spearman rho =", round(spearman_wknd$estimate, 4),
    ", p =", signif(spearman_wknd$p.value, 4), "\n")
cat("Weekday Alcohol vs Final Grade: Spearman rho =", round(spearman_wkdy$estimate, 4),
    ", p =", signif(spearman_wkdy$p.value, 4), "\n\n")

# PEARSON CORRELATION TEST
pearson_wknd <- cor.test(data_clean$Weekend_Alcohol[idx_wknd],
                         data_clean$Final_Grade[idx_wknd],
                         method = "pearson")

pearson_wkdy <- cor.test(data_clean$Weekday_Alcohol[idx_wkdy],
                         data_clean$Final_Grade[idx_wkdy],
                         method = "pearson")

cat("-- Pearson Results--\n")
cat("Weekend Alcohol vs Final Grade: r =", round(pearson_wknd$estimate, 4),", p =", signif(pearson_wknd$p.value, 4), "\n")
cat("Weekday Alcohol vs Final Grade: r =", round(pearson_wkdy$estimate, 4),", p =", signif(pearson_wkdy$p.value, 4), "\n\n")

# 6. INTERPRETATION (Prioritise Spearman)

cat("\n=== 6. INTERPRETATION ===\n")
interpret_spearman <- function(s_obj, label){
  rho <- as.numeric(s_obj$estimate)
  pval <- s_obj$p.value
  if (pval < 0.05) {
    cat(label, ": Spearman rho =", round(rho,4),
        " statistically significant (p =", signif(pval,4),
        "). Interpretation: evidence of a relationship between student grades and alcohol consumption.\n")
  } else {
    cat(label, ": Spearman rho =", round(rho,4),
        " not statistically significant (p =", signif(pval,4),
        "). Interpretation: no evidence of a relationship between student grades and alcohol consumption.\n")
  }
}

interpret_spearman(spearman_wknd, "Weekend Alcohol vs Final Grade")
interpret_spearman(spearman_wkdy, "Weekday Alcohol vs Final Grade")

#Results in a table for better understanding.
summary_df <- data.frame(
  test = c("Spearman_rho_wknd","Spearman_p_wknd","Pearson_r_wknd","Pearson_p_wknd",
           "Spearman_rho_wkdy","Spearman_p_wkdy","Pearson_r_wkdy","Pearson_p_wkdy"),
  value = c(as.numeric(spearman_wknd$estimate), spearman_wknd$p.value, as.numeric(pearson_wknd$estimate), pearson_wknd$p.value,
            as.numeric(spearman_wkdy$estimate), spearman_wkdy$p.value, as.numeric(pearson_wkdy$estimate), pearson_wkdy$p.value)
)

print(summary_df)


