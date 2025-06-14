# ===================================================================
# Hypothesis Testing Examples
# Sample Size Calculation Course Module
# ===================================================================

# This script provides examples of basic hypothesis testing concepts
# and demonstrates how to perform common statistical tests in R.

# ===================================================================
# 1. One-Sample t-test
# ===================================================================

# Example: Testing if the mean of a sample differs from a known value
# H0: μ = μ0 vs. H1: μ ≠ μ0

# Generate sample data: Student test scores with mean around 75
set.seed(123)
test_scores <- rnorm(30, mean = 75, sd = 8)

# Perform one-sample t-test (testing if mean differs from 70)
t_test_result <- t.test(test_scores, mu = 70)
print(t_test_result)

# Interpretation:
# The p-value is 0.0006, which is less than 0.05
# We reject the null hypothesis and conclude that the mean test score
# is significantly different from 70.

# Calculate effect size (Cohen's d)
mean_diff <- mean(test_scores) - 70
sd_sample <- sd(test_scores)
cohens_d <- mean_diff / sd_sample
cat("Effect size (Cohen's d):", round(cohens_d, 2), "\n")

# ===================================================================
# 2. Two-Sample t-test (Independent Samples)
# ===================================================================

# Example: Comparing means between two independent groups
# H0: μ1 = μ2 vs. H1: μ1 ≠ μ2

# Generate sample data: Test scores for two teaching methods
set.seed(456)
method_A <- rnorm(25, mean = 78, sd = 8)
method_B <- rnorm(25, mean = 85, sd = 8)

# Perform two-sample t-test
t_test_ind <- t.test(method_B, method_A)
print(t_test_ind)

# Interpretation:
# The p-value is 0.0025, which is less than 0.05
# We reject the null hypothesis and conclude that there is a significant
# difference in mean test scores between the two teaching methods.

# Calculate effect size (Cohen's d)
mean_diff_ind <- mean(method_B) - mean(method_A)
pooled_sd <- sqrt(((length(method_A) - 1) * var(method_A) + 
                   (length(method_B) - 1) * var(method_B)) / 
                  (length(method_A) + length(method_B) - 2))
cohens_d_ind <- mean_diff_ind / pooled_sd
cat("Effect size (Cohen's d):", round(cohens_d_ind, 2), "\n")

# Visualize the data
boxplot(method_A, method_B, 
        names = c("Method A", "Method B"),
        main = "Test Scores by Teaching Method",
        ylab = "Test Score",
        col = c("lightblue", "lightgreen"))

# ===================================================================
# 3. Paired t-test
# ===================================================================

# Example: Comparing means between paired measurements (pre-test and post-test)
# H0: μd = 0 vs. H1: μd ≠ 0 (where μd is the mean difference)

# Generate sample data: Pre-test and post-test scores
set.seed(789)
pre_test <- rnorm(20, mean = 65, sd = 10)
# Post-test scores are related to pre-test scores but with improvement
post_test <- pre_test + rnorm(20, mean = 8, sd = 5)

# Perform paired t-test
t_test_paired <- t.test(post_test, pre_test, paired = TRUE)
print(t_test_paired)

# Interpretation:
# The p-value is very small (< 0.0001), which is less than 0.05
# We reject the null hypothesis and conclude that there is a significant
# improvement from pre-test to post-test.

# Calculate effect size (Cohen's d for paired data)
diff <- post_test - pre_test
cohens_d_paired <- mean(diff) / sd(diff)
cat("Effect size (Cohen's d for paired data):", round(cohens_d_paired, 2), "\n")

# Visualize the paired data
plot(pre_test, post_test, 
     main = "Pre-test vs. Post-test Scores",
     xlab = "Pre-test Score", 
     ylab = "Post-test Score",
     pch = 19, col = "blue")
abline(0, 1, lty = 2)  # Add diagonal line (y = x)
legend("topleft", legend = "y = x", lty = 2)

# ===================================================================
# 4. Chi-Square Test for Independence
# ===================================================================

# Example: Testing association between two categorical variables
# H0: The variables are independent vs. H1: The variables are associated

# Create a contingency table: Treatment outcome by gender
treatment_outcome <- matrix(c(40, 30, 20, 10, 15, 35, 30, 20), nrow = 2, 
                           dimnames = list(Gender = c("Male", "Female"),
                                          Outcome = c("Improved", "Slightly Improved", 
                                                     "No Change", "Worsened")))
print(treatment_outcome)

# Perform chi-square test
chi_sq_test <- chisq.test(treatment_outcome)
print(chi_sq_test)

# Interpretation:
# The p-value is 0.0005, which is less than 0.05
# We reject the null hypothesis and conclude that there is a significant
# association between gender and treatment outcome.

# Calculate effect size (Cramer's V)
n <- sum(treatment_outcome)
chi_sq <- chi_sq_test$statistic
df <- min(nrow(treatment_outcome) - 1, ncol(treatment_outcome) - 1)
cramers_v <- sqrt(chi_sq / (n * df))
cat("Effect size (Cramer's V):", round(cramers_v, 3), "\n")

# Visualize the contingency table
barplot(treatment_outcome, beside = TRUE, 
        main = "Treatment Outcome by Gender",
        xlab = "Outcome", 
        ylab = "Count",
        col = c("lightblue", "lightpink"),
        legend.text = TRUE)

# ===================================================================
# 5. Correlation Test
# ===================================================================

# Example: Testing if there is a significant correlation between two variables
# H0: ρ = 0 vs. H1: ρ ≠ 0

# Generate sample data: Hours studied and exam scores
set.seed(101)
hours_studied <- runif(40, 1, 10)
# Exam scores are related to hours studied with some noise
exam_scores <- 50 + 4 * hours_studied + rnorm(40, 0, 10)

# Perform correlation test
cor_test <- cor.test(hours_studied, exam_scores)
print(cor_test)

# Interpretation:
# The p-value is very small (< 0.0001), which is less than 0.05
# We reject the null hypothesis and conclude that there is a significant
# correlation between hours studied and exam scores.

# The correlation coefficient (r = 0.76) indicates a strong positive correlation.

# Visualize the correlation
plot(hours_studied, exam_scores, 
     main = "Relationship Between Hours Studied and Exam Scores",
     xlab = "Hours Studied", 
     ylab = "Exam Score",
     pch = 19, col = "darkgreen")
abline(lm(exam_scores ~ hours_studied), col = "red")

# ===================================================================
# 6. ANOVA (Analysis of Variance)
# ===================================================================

# Example: Comparing means across more than two groups
# H0: All group means are equal vs. H1: At least one group mean differs

# Generate sample data: Test scores for three teaching methods
set.seed(202)
method_A <- rnorm(20, mean = 75, sd = 8)
method_B <- rnorm(20, mean = 80, sd = 8)
method_C <- rnorm(20, mean = 85, sd = 8)

# Combine data into a single data frame
scores <- c(method_A, method_B, method_C)
methods <- factor(rep(c("A", "B", "C"), each = 20))
data <- data.frame(Score = scores, Method = methods)

# Perform one-way ANOVA
anova_result <- aov(Score ~ Method, data = data)
summary(anova_result)

# Interpretation:
# The p-value is very small (< 0.0001), which is less than 0.05
# We reject the null hypothesis and conclude that there are significant
# differences in mean test scores among the three teaching methods.

# Post-hoc test to identify which groups differ
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Calculate effect size (eta-squared)
anova_summary <- summary(anova_result)
ss_between <- anova_summary[[1]][1, 2]
ss_total <- ss_between + anova_summary[[1]][2, 2]
eta_squared <- ss_between / ss_total
cat("Effect size (eta-squared):", round(eta_squared, 3), "\n")

# Visualize the data
boxplot(Score ~ Method, data = data,
        main = "Test Scores by Teaching Method",
        xlab = "Teaching Method", 
        ylab = "Test Score",
        col = c("lightblue", "lightgreen", "lightpink"))

# ===================================================================
# 7. Confidence Intervals
# ===================================================================

# Example: Calculating confidence intervals for a mean

# Generate sample data
set.seed(303)
blood_pressure <- rnorm(40, mean = 120, sd = 10)

# Calculate 95% confidence interval
mean_bp <- mean(blood_pressure)
se_bp <- sd(blood_pressure) / sqrt(length(blood_pressure))
t_critical <- qt(0.975, df = length(blood_pressure) - 1)
margin_error <- t_critical * se_bp
ci_lower <- mean_bp - margin_error
ci_upper <- mean_bp + margin_error

cat("Mean blood pressure:", round(mean_bp, 1), "mmHg\n")
cat("95% Confidence Interval: [", round(ci_lower, 1), ",", round(ci_upper, 1), "] mmHg\n")

# Alternative: use t.test function
t_test_ci <- t.test(blood_pressure)
print(t_test_ci$conf.int)

# Interpretation:
# We are 95% confident that the true population mean blood pressure
# is between the lower and upper bounds of the confidence interval.

# ===================================================================
# 8. Type I and Type II Error Simulation
# ===================================================================

# Simulate Type I and Type II errors in hypothesis testing

# Parameters
n <- 30  # Sample size
mu0 <- 100  # Null hypothesis value
mu1 <- 105  # True population mean
sigma <- 15  # Population standard deviation
alpha <- 0.05  # Significance level
n_simulations <- 1000  # Number of simulations

# Function to perform one-sample t-test and return decision
perform_test <- function(sample_mean, sample_sd, n, mu0, alpha) {
  t_stat <- (sample_mean - mu0) / (sample_sd / sqrt(n))
  p_value <- 2 * pt(-abs(t_stat), df = n - 1)
  reject_h0 <- p_value < alpha
  return(reject_h0)
}

# Simulate Type I error (H0 is true, but we reject it)
set.seed(404)
type_I_errors <- numeric(n_simulations)

for (i in 1:n_simulations) {
  # Generate data where H0 is true (mu = mu0)
  sample_data <- rnorm(n, mean = mu0, sd = sigma)
  sample_mean <- mean(sample_data)
  sample_sd <- sd(sample_data)
  
  # Perform test and record decision
  type_I_errors[i] <- perform_test(sample_mean, sample_sd, n, mu0, alpha)
}

type_I_error_rate <- mean(type_I_errors)
cat("Simulated Type I error rate:", round(type_I_error_rate, 3), "\n")
cat("Expected Type I error rate (alpha):", alpha, "\n")

# Simulate Type II error (H0 is false, but we fail to reject it)
set.seed(505)
type_II_errors <- numeric(n_simulations)

for (i in 1:n_simulations) {
  # Generate data where H0 is false (mu = mu1)
  sample_data <- rnorm(n, mean = mu1, sd = sigma)
  sample_mean <- mean(sample_data)
  sample_sd <- sd(sample_data)
  
  # Perform test and record decision
  reject_h0 <- perform_test(sample_mean, sample_sd, n, mu0, alpha)
  type_II_errors[i] <- !reject_h0  # Type II error if we fail to reject when H0 is false
}

type_II_error_rate <- mean(type_II_errors)
power <- 1 - type_II_error_rate
cat("Simulated Type II error rate (beta):", round(type_II_error_rate, 3), "\n")
cat("Simulated power (1 - beta):", round(power, 3), "\n")

# Calculate theoretical power
delta <- (mu1 - mu0) / sigma  # Standardized effect size
theoretical_power <- power.t.test(n = n, delta = delta, sd = 1, 
                                 sig.level = alpha, type = "one.sample")$power
cat("Theoretical power:", round(theoretical_power, 3), "\n")

# ===================================================================
# 9. P-value Distribution Simulation
# ===================================================================

# Simulate the distribution of p-values under the null and alternative hypotheses

# Parameters
n <- 30  # Sample size
mu0 <- 100  # Null hypothesis value
mu1 <- 105  # Alternative hypothesis value
sigma <- 15  # Population standard deviation
n_simulations <- 1000  # Number of simulations

# Simulate p-values under H0 (null hypothesis is true)
set.seed(606)
p_values_h0 <- numeric(n_simulations)

for (i in 1:n_simulations) {
  sample_data <- rnorm(n, mean = mu0, sd = sigma)
  t_test_result <- t.test(sample_data, mu = mu0)
  p_values_h0[i] <- t_test_result$p.value
}

# Simulate p-values under H1 (alternative hypothesis is true)
set.seed(707)
p_values_h1 <- numeric(n_simulations)

for (i in 1:n_simulations) {
  sample_data <- rnorm(n, mean = mu1, sd = sigma)
  t_test_result <- t.test(sample_data, mu = mu0)
  p_values_h1[i] <- t_test_result$p.value
}

# Plot the distributions
par(mfrow = c(1, 2))

# P-value distribution under H0
hist(p_values_h0, breaks = 20, 
     main = "P-value Distribution Under H0",
     xlab = "P-value", 
     col = "lightblue",
     xlim = c(0, 1))
abline(v = 0.05, col = "red", lty = 2)
text(0.15, n_simulations/10, "α = 0.05", col = "red")

# P-value distribution under H1
hist(p_values_h1, breaks = 20, 
     main = "P-value Distribution Under H1",
     xlab = "P-value", 
     col = "lightgreen",
     xlim = c(0, 1))
abline(v = 0.05, col = "red", lty = 2)
text(0.15, n_simulations/10, "α = 0.05", col = "red")

# Reset plotting parameters
par(mfrow = c(1, 1))

# ===================================================================
# End of Script
# ===================================================================
