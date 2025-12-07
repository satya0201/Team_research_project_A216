# checking the data and measures of central tendency of the data.
View(data)
summary(data)
# checking the unique values of each columns.
unique(data$school)
unique(data$address)
unique(data$famsize)
unique(data$Dalc)
unique(data$Walc)
unique(data$G3)

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

