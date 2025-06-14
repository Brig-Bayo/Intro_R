# Module 1: Basic Use of R
# Author: Brig-Bayo
# This script demonstrates the basic concepts covered in Module 1

# ===== R as a Calculator =====
# Basic arithmetic operations
print(5 + 3)  # Addition
print(10 - 4)  # Subtraction
print(6 * 7)  # Multiplication
print(15 / 3)  # Division
print(2^3)  # Exponentiation
print(17 %% 5)  # Modulo (remainder after division)
print(17 %/% 5)  # Integer division

# ===== Variables and Assignment =====
# Assigning values to variables
x <- 10
y <- 5  # Consistent use of '<-' for assignment

# Printing the value of a variable
print(x)
x  # R also prints the value when just the variable name is typed

# Performing operations with variables
z <- x + y
print(z)  # Result: 15

# ===== Basic Functions =====
# Square root
print(sqrt(25))  # Result: 5

# Absolute value
print(abs(-7))  # Result: 7

# Rounding
print(round(3.14159, 2))  # Result: 3.14

# Natural logarithm
print(log(10))  # Result: 2.302585

# Exponential
print(exp(1))  # Result: 2.718282 (e)

# Sum
print(sum(1, 2, 3, 4, 5))  # Result: 15

# Mean
print(mean(c(1, 2, 3, 4, 5)))  # Result: 3

# ===== Data Types =====
# Numeric
x <- 10.5
print(class(x))  # Result: "numeric"

# Integer
y <- 10L  # The 'L' suffix creates an integer
print(class(y))  # Result: "integer"

# Character
name <- "John"
print(class(name))  # Result: "character"

# Logical
is_student <- TRUE
print(class(is_student))  # Result: "logical"

# Complex
z <- 3 + 2i
print(class(z))  # Result: "complex"

# ===== Data Structures =====
# Vectors
# Creating a numeric vector
numbers <- c(1, 2, 3, 4, 5)
print(numbers)

# Creating a character vector
fruits <- c("apple", "banana", "orange")
print(fruits)

# Accessing elements of a vector
print(numbers[1])  # Result: 1 (R indexing starts at 1, not 0)
print(fruits[2])  # Result: "banana"

# Vector operations
print(numbers * 2)  # Result: 2 4 6 8 10
print(numbers + c(10, 20, 30, 40, 50))  # Result: 11 22 33 44 55

# Matrices
# Creating a matrix
mat <- matrix(1:9, nrow = 3, ncol = 3)
print(mat)

# Accessing elements of a matrix
print(mat[1, 2])  # Result: 4 (element in row 1, column 2)
print(mat[2, ])   # Result: 2 5 8 (entire second row)
print(mat[, 3])   # Result: 7 8 9 (entire third column)

# Lists
# Creating a list
person <- list(name = "John", age = 30, is_student = FALSE, scores = c(85, 90, 78))
print(person)

# Accessing elements of a list
print(person$name)  # Result: "John"
print(person[[1]])  # Result: "John"
print(person$scores[2])  # Result: 90

# Data Frames
# Creating a data frame
students <- data.frame(
  name = c("John", "Alice", "Bob"),
  age = c(22, 25, 23),
  gpa = c(3.5, 3.9, 3.2),
  stringsAsFactors = FALSE
)
print(students)

# Accessing elements of a data frame
print(students$name)  # Result: "John" "Alice" "Bob"
print(students[1, ])  # Result: First row
print(students[, "age"])  # Result: 22 25 23
print(students[2, 3])  # Result: 3.9 (element in row 2, column 3)

# Factors
# Creating a factor
gender <- factor(c("male", "female", "male", "female", "male"))
print(gender)

# Getting the levels of a factor
print(levels(gender))  # Result: "female" "male"

# ===== Working Directory =====
# Get the current working directory
print(getwd())

# Set the working directory (uncomment and modify path as needed)
# setwd("/path/to/your/directory")

# ===== Importing and Exporting Data =====
# Create a sample data frame to export
sample_data <- data.frame(
  id = 1:5,
  name = c("John", "Alice", "Bob", "Carol", "David"),
  score = c(85, 92, 78, 88, 95),
  stringsAsFactors = FALSE
)

# Export to CSV
write.csv(sample_data, "sample_data.csv", row.names = FALSE)

# Import from CSV
imported_data <- read.csv("sample_data.csv", stringsAsFactors = FALSE)
print(imported_data)

# ===== Basic Data Manipulation =====
# Subsetting data
young_students <- students[students$age < 25, ]
print(young_students)

# Adding a new column
students$grade <- c("B", "A", "C")
print(students)

# Creating a column based on existing columns
students$pass <- students$gpa >= 3.5
print(students)

# Sorting data
students_sorted <- students[order(students$gpa, decreasing = TRUE), ]
print(students_sorted)

# ===== Basic Summary Statistics =====
# Summary of the entire data frame
print(summary(students))

# Summary of a specific column
print(summary(students$age))

# Mean, median, min, max of a column
print(mean(students$gpa))
print(median(students$gpa))
print(min(students$gpa))
print(max(students$gpa))
print(range(students$gpa))

# ===== Basic Data Visualization =====
# Scatter plot
plot(students$age, students$gpa, 
     main = "Age vs. GPA",
     xlab = "Age",
     ylab = "GPA",
     pch = 19,
     col = "blue")

# Bar plot
counts <- table(gender)
barplot(counts, 
        main = "Gender Distribution",
        xlab = "Gender",
        ylab = "Frequency",
        col = c("pink", "lightblue"))

# Histogram
hist(students$gpa, 
     main = "GPA Distribution",
     xlab = "GPA",
     ylab = "Frequency",
     col = "lightgreen",
     breaks = 10)

# Box plot
boxplot(students$gpa, 
        main = "GPA Distribution",
        ylab = "GPA",
        col = "lightblue")

# Saving a plot (uncomment to use)
# png("my_plot.png", width = 800, height = 600)
# plot(students$age, students$gpa, main = "Age vs. GPA", xlab = "Age", ylab = "GPA", pch = 19, col = "blue")
# dev.off()

# ===== Installing and Loading Packages =====
# Install a package (uncomment to use)
# install.packages("ggplot2")

# Check if a package is installed and install if needed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Load a package
library(ggplot2)

# Example of using ggplot2
ggplot(students, aes(x = age, y = gpa)) +
  geom_point(size = 3, color = "blue") +
  labs(title = "Age vs. GPA",
       x = "Age",
       y = "GPA") +
  theme_minimal()

# ===== Creating a More Complex Dataset =====
# Create a larger dataset for practice
set.seed(123)  # For reproducibility
n <- 50

larger_data <- data.frame(
  id = 1:n,
  age = sample(18:30, n, replace = TRUE),
  gender = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  major = factor(sample(c("Computer Science", "Mathematics", "Physics", "Biology", "Chemistry"), n, replace = TRUE)),
  gpa = round(runif(n, 2.0, 4.0), 2),
  hours_studied = sample(1:40, n, replace = TRUE)
)

# View the first few rows
print(head(larger_data))

# Summary statistics
print(summary(larger_data))

# Correlation between hours studied and GPA
print(cor(larger_data$hours_studied, larger_data$gpa))

# Visualization with ggplot2
ggplot(larger_data, aes(x = hours_studied, y = gpa, color = gender)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relationship Between Study Hours and GPA",
       x = "Hours Studied per Week",
       y = "GPA",
       color = "Gender") +
  theme_minimal()

# Box plot of GPA by major
ggplot(larger_data, aes(x = major, y = gpa, fill = major)) +
  geom_boxplot() +
  labs(title = "GPA Distribution by Major",
       x = "Major",
       y = "GPA") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the dataset for future use
write.csv(larger_data, "student_data.csv", row.names = FALSE)

# Print a message indicating the script has completed
cat("Module 1 script has completed successfully!\n")
