# ===================================================================
# Error Types and Power Analysis
# Sample Size Calculation Course Module
# ===================================================================

# This script demonstrates concepts related to Type I and Type II errors,
# statistical power, and how they relate to sample size determination.

# ===================================================================
# 1. Type I and Type II Errors
# ===================================================================

# Type I Error: Rejecting a true null hypothesis (false positive)
# Type II Error: Failing to reject a false null hypothesis (false negative)

# Let's simulate these errors in a hypothesis testing scenario

# Parameters
set.seed(123)
alpha <- 0.05  # Significance level (probability of Type I error)
n <- 30  # Sample size
mu0 <- 100  # Null hypothesis value
sigma <- 15  # Population standard deviation

# Simulate Type I error (when H0 is true)
n_simulations <- 10000
type_I_errors <- numeric(n_simulations)

for (i in 1:n_simulations) {
  # Generate data where H0 is true (mu = mu0)
  sample <- rnorm(n, mean = mu0, sd = sigma)
  
  # Perform one-sample t-test
  test_result <- t.test(sample, mu = mu0)
  
  # Record if we rejected H0 (Type I error since H0 is true)
  type_I_errors[i] <- test_result$p.value < alpha
}

type_I_error_rate <- mean(type_I_errors)
cat("Simulated Type I error rate:", round(type_I_error_rate, 3), "\n")
cat("Expected Type I error rate (alpha):", alpha, "\n")

# Simulate Type II error (when H0 is false)
mu1 <- 105  # True population mean (different from mu0)
type_II_errors <- numeric(n_simulations)

for (i in 1:n_simulations) {
  # Generate data where H0 is false (mu = mu1)
  sample <- rnorm(n, mean = mu1, sd = sigma)
  
  # Perform one-sample t-test
  test_result <- t.test(sample, mu = mu0)
  
  # Record if we failed to reject H0 (Type II error since H0 is false)
  type_II_errors[i] <- test_result$p.value >= alpha
}

type_II_error_rate <- mean(type_II_errors)
power <- 1 - type_II_error_rate
cat("Simulated Type II error rate (beta):", round(type_II_error_rate, 3), "\n")
cat("Simulated power (1 - beta):", round(power, 3), "\n")

# ===================================================================
# 2. Factors Affecting Power
# ===================================================================

# Let's examine how different factors affect statistical power

# 2.1 Effect of Sample Size on Power
# ----------------------------------

# Parameters
effect_size <- (mu1 - mu0) / sigma  # Standardized effect size
sample_sizes <- seq(10, 100, by = 5)
power_values <- numeric(length(sample_sizes))

for (i in 1:length(sample_sizes)) {
  n_i <- sample_sizes[i]
  power_values[i] <- power.t.test(n = n_i, delta = effect_size, sd = 1, 
                                 sig.level = alpha, type = "one.sample")$power
}

# Plot the relationship between sample size and power
plot(sample_sizes, power_values, type = "b", 
     main = "Effect of Sample Size on Statistical Power",
     xlab = "Sample Size", ylab = "Power (1 - β)",
     col = "blue", pch = 19)
abline(h = 0.8, col = "red", lty = 2)
text(max(sample_sizes) * 0.8, 0.82, "Power = 0.8", col = "red")

# Find minimum sample size for 80% power
min_n_index <- which(power_values >= 0.8)[1]
min_n <- sample_sizes[min_n_index]
points(min_n, power_values[min_n_index], col = "red", pch = 19, cex = 1.5)
text(min_n, power_values[min_n_index] - 0.05, 
     paste("n =", min_n), col = "red")

cat("Minimum sample size for 80% power:", min_n, "\n")

# 2.2 Effect of Effect Size on Power
# ----------------------------------

# Parameters
n_fixed <- 30  # Fixed sample size
effect_sizes <- seq(0.1, 1.0, by = 0.05)  # Range of effect sizes
power_values_es <- numeric(length(effect_sizes))

for (i in 1:length(effect_sizes)) {
  es_i <- effect_sizes[i]
  power_values_es[i] <- power.t.test(n = n_fixed, delta = es_i, sd = 1, 
                                    sig.level = alpha, type = "one.sample")$power
}

# Plot the relationship between effect size and power
plot(effect_sizes, power_values_es, type = "b", 
     main = "Effect of Effect Size on Statistical Power",
     xlab = "Effect Size (Cohen's d)", ylab = "Power (1 - β)",
     col = "green", pch = 19)
abline(h = 0.8, col = "red", lty = 2)
text(max(effect_sizes) * 0.8, 0.82, "Power = 0.8", col = "red")

# Find minimum effect size for 80% power
min_es_index <- which(power_values_es >= 0.8)[1]
min_es <- effect_sizes[min_es_index]
points(min_es, power_values_es[min_es_index], col = "red", pch = 19, cex = 1.5)
text(min_es, power_values_es[min_es_index] - 0.05, 
     paste("d =", round(min_es, 2)), col = "red")

cat("Minimum effect size for 80% power with n =", n_fixed, ":", round(min_es, 2), "\n")

# 2.3 Effect of Significance Level on Power
# -----------------------------------------

# Parameters
n_fixed <- 30  # Fixed sample size
es_fixed <- 0.5  # Fixed effect size
alpha_levels <- c(0.01, 0.05, 0.1)  # Different significance levels
power_values_alpha <- numeric(length(alpha_levels))

for (i in 1:length(alpha_levels)) {
  alpha_i <- alpha_levels[i]
  power_values_alpha[i] <- power.t.test(n = n_fixed, delta = es_fixed, sd = 1, 
                                       sig.level = alpha_i, type = "one.sample")$power
}

# Display the results
result_df <- data.frame(
  SignificanceLevel = alpha_levels,
  Power = round(power_values_alpha, 3)
)
print(result_df)

# ===================================================================
# 3. Power Analysis for Different Tests
# ===================================================================

# 3.1 Power Analysis for t-tests
# ------------------------------

# One-sample t-test
power_one_sample <- power.t.test(n = 30, delta = 0.5, sd = 1, 
                                sig.level = 0.05, type = "one.sample")
cat("Power for one-sample t-test:", round(power_one_sample$power, 3), "\n")

# Two-sample t-test (independent samples)
power_two_sample <- power.t.test(n = 30, delta = 0.5, sd = 1, 
                                sig.level = 0.05, type = "two.sample")
cat("Power for two-sample t-test:", round(power_two_sample$power, 3), "\n")

# Paired t-test
power_paired <- power.t.test(n = 30, delta = 0.5, sd = 1, 
                            sig.level = 0.05, type = "paired")
cat("Power for paired t-test:", round(power_paired$power, 3), "\n")

# 3.2 Power Analysis for Correlation
# ---------------------------------

# Using the pwr package
library(pwr)

# Power for correlation test
power_corr <- pwr.r.test(n = 30, r = 0.3, sig.level = 0.05)
cat("Power for correlation test (r = 0.3, n = 30):", round(power_corr$power, 3), "\n")

# 3.3 Power Analysis for ANOVA
# ---------------------------

# Power for one-way ANOVA
groups <- 3  # Number of groups
n_per_group <- 20  # Sample size per group
effect_size_f <- 0.25  # Cohen's f (medium effect)

power_anova <- pwr.anova.test(k = groups, n = n_per_group, f = effect_size_f, 
                             sig.level = 0.05)
cat("Power for one-way ANOVA:", round(power_anova$power, 3), "\n")

# 3.4 Power Analysis for Chi-square Test
# -------------------------------------

# Power for chi-square test of independence
df <- (2 - 1) * (3 - 1)  # Degrees of freedom for 2×3 contingency table
effect_size_w <- 0.3  # Cohen's w (medium effect)
n_total <- 100  # Total sample size

power_chisq <- pwr.chisq.test(w = effect_size_w, N = n_total, df = df, 
                             sig.level = 0.05)
cat("Power for chi-square test:", round(power_chisq$power, 3), "\n")

# ===================================================================
# 4. Power Curves for Different Tests
# ===================================================================

# Create power curves for different statistical tests

# Parameters
sample_sizes <- seq(10, 100, by = 5)
effect_size_d <- 0.5  # Medium effect size for t-tests
effect_size_r <- 0.3  # Medium effect size for correlation
effect_size_f <- 0.25  # Medium effect size for ANOVA
effect_size_w <- 0.3  # Medium effect size for chi-square
alpha <- 0.05  # Significance level

# Calculate power for different tests
power_one_sample <- sapply(sample_sizes, function(n) {
  power.t.test(n = n, delta = effect_size_d, sd = 1, 
              sig.level = alpha, type = "one.sample")$power
})

power_two_sample <- sapply(sample_sizes, function(n) {
  power.t.test(n = n, delta = effect_size_d, sd = 1, 
              sig.level = alpha, type = "two.sample")$power
})

power_paired <- sapply(sample_sizes, function(n) {
  power.t.test(n = n, delta = effect_size_d, sd = 1, 
              sig.level = alpha, type = "paired")$power
})

power_corr <- sapply(sample_sizes, function(n) {
  pwr.r.test(n = n, r = effect_size_r, sig.level = alpha)$power
})

power_anova <- sapply(sample_sizes, function(n) {
  pwr.anova.test(k = 3, n = n, f = effect_size_f, sig.level = alpha)$power
})

power_chisq <- sapply(sample_sizes, function(n) {
  pwr.chisq.test(w = effect_size_w, N = n, df = 2, sig.level = alpha)$power
})

# Plot power curves
plot(sample_sizes, power_one_sample, type = "l", col = "blue", lwd = 2,
     main = "Power Curves for Different Statistical Tests",
     xlab = "Sample Size", ylab = "Power (1 - β)",
     ylim = c(0, 1))
lines(sample_sizes, power_two_sample, col = "red", lwd = 2)
lines(sample_sizes, power_paired, col = "green", lwd = 2)
lines(sample_sizes, power_corr, col = "purple", lwd = 2)
lines(sample_sizes, power_anova, col = "orange", lwd = 2)
lines(sample_sizes, power_chisq, col = "brown", lwd = 2)
abline(h = 0.8, lty = 2)
legend("bottomright", 
       legend = c("One-sample t-test", "Two-sample t-test", "Paired t-test", 
                 "Correlation", "ANOVA", "Chi-square"),
       col = c("blue", "red", "green", "purple", "orange", "brown"),
       lwd = 2)

# ===================================================================
# 5. Sample Size Determination
# ===================================================================

# Calculate required sample size for different tests to achieve 80% power

# 5.1 Sample Size for t-tests
# --------------------------

# One-sample t-test
ss_one_sample <- power.t.test(power = 0.8, delta = 0.5, sd = 1, 
                             sig.level = 0.05, type = "one.sample")
cat("Required sample size for one-sample t-test:", ceiling(ss_one_sample$n), "\n")

# Two-sample t-test (independent samples)
ss_two_sample <- power.t.test(power = 0.8, delta = 0.5, sd = 1, 
                             sig.level = 0.05, type = "two.sample")
cat("Required sample size per group for two-sample t-test:", ceiling(ss_two_sample$n), "\n")

# Paired t-test
ss_paired <- power.t.test(power = 0.8, delta = 0.5, sd = 1, 
                         sig.level = 0.05, type = "paired")
cat("Required number of pairs for paired t-test:", ceiling(ss_paired$n), "\n")

# 5.2 Sample Size for Correlation
# ------------------------------

# Correlation test
ss_corr <- pwr.r.test(r = 0.3, power = 0.8, sig.level = 0.05)
cat("Required sample size for correlation test (r = 0.3):", ceiling(ss_corr$n), "\n")

# 5.3 Sample Size for ANOVA
# ------------------------

# One-way ANOVA
ss_anova <- pwr.anova.test(k = 3, f = 0.25, power = 0.8, sig.level = 0.05)
cat("Required sample size per group for one-way ANOVA:", ceiling(ss_anova$n), "\n")

# 5.4 Sample Size for Chi-square Test
# ----------------------------------

# Chi-square test of independence
ss_chisq <- pwr.chisq.test(w = 0.3, df = 2, power = 0.8, sig.level = 0.05)
cat("Required total sample size for chi-square test:", ceiling(ss_chisq$N), "\n")

# ===================================================================
# 6. Visualizing Type I and Type II Errors
# ===================================================================

# Create a visualization of Type I and Type II errors

# Parameters
mu0 <- 0  # Null hypothesis value
mu1 <- 2  # Alternative hypothesis value
sigma <- 3  # Standard deviation
n <- 30  # Sample size
alpha <- 0.05  # Significance level

# Calculate critical value
se <- sigma / sqrt(n)
z_crit <- qnorm(1 - alpha)
crit_value <- mu0 + z_crit * se

# Create sequence of x values for plotting
x <- seq(-4, 6, length.out = 1000)

# Calculate densities under null and alternative hypotheses
y0 <- dnorm(x, mean = mu0, sd = se)
y1 <- dnorm(x, mean = mu1, sd = se)

# Plot the distributions
plot(x, y0, type = "l", col = "blue", lwd = 2,
     main = "Visualization of Type I and Type II Errors",
     xlab = "Sample Mean", ylab = "Density",
     ylim = c(0, max(c(y0, y1)) * 1.1))
lines(x, y1, col = "red", lwd = 2)
abline(v = crit_value, lty = 2)
text(crit_value + 0.2, max(y0) * 0.5, "Critical Value", srt = 90)

# Shade areas representing errors
x_type1 <- x[x >= crit_value]
y_type1 <- y0[x >= crit_value]
polygon(c(x_type1, rev(x_type1)), c(rep(0, length(x_type1)), rev(y_type1)), 
        col = rgb(0, 0, 1, 0.3))

x_type2 <- x[x <= crit_value]
y_type2 <- y1[x <= crit_value]
polygon(c(x_type2, rev(x_type2)), c(rep(0, length(x_type2)), rev(y_type2)), 
        col = rgb(1, 0, 0, 0.3))

# Add legend
legend("topright", 
       legend = c("Null Distribution (H0)", "Alternative Distribution (H1)", 
                 "Type I Error Area", "Type II Error Area"),
       col = c("blue", "red", rgb(0, 0, 1, 0.3), rgb(1, 0, 0, 0.3)),
       lwd = c(2, 2, 10, 10))

# Calculate and display error rates
type1_error <- 1 - pnorm(crit_value, mean = mu0, sd = se)
type2_error <- pnorm(crit_value, mean = mu1, sd = se)
power <- 1 - type2_error

cat("Type I error rate (alpha):", round(type1_error, 3), "\n")
cat("Type II error rate (beta):", round(type2_error, 3), "\n")
cat("Power (1 - beta):", round(power, 3), "\n")

# ===================================================================
# 7. Trade-off Between Type I and Type II Errors
# ===================================================================

# Demonstrate the trade-off between Type I and Type II errors

# Parameters
mu0 <- 0  # Null hypothesis value
mu1 <- 2  # Alternative hypothesis value
sigma <- 3  # Standard deviation
n <- 30  # Sample size
alpha_levels <- seq(0.01, 0.2, by = 0.01)  # Range of alpha levels

# Calculate Type II error rates and power for different alpha levels
type2_errors <- numeric(length(alpha_levels))
power_values <- numeric(length(alpha_levels))

for (i in 1:length(alpha_levels)) {
  alpha_i <- alpha_levels[i]
  z_crit <- qnorm(1 - alpha_i)
  crit_value <- mu0 + z_crit * (sigma / sqrt(n))
  
  type2_errors[i] <- pnorm(crit_value, mean = mu1, sd = sigma / sqrt(n))
  power_values[i] <- 1 - type2_errors[i]
}

# Plot the trade-off
plot(alpha_levels, type2_errors, type = "l", col = "red", lwd = 2,
     main = "Trade-off Between Type I and Type II Errors",
     xlab = "Type I Error Rate (α)", ylab = "Error Rate",
     ylim = c(0, max(type2_errors) * 1.1))
lines(alpha_levels, power_values, col = "green", lwd = 2)
abline(v = 0.05, lty = 2)
text(0.05 + 0.01, max(type2_errors) * 0.5, "α = 0.05", srt = 90)

# Add legend
legend("topright", 
       legend = c("Type II Error Rate (β)", "Power (1 - β)"),
       col = c("red", "green"),
       lwd = 2)

# Create a data frame with the results
result_df <- data.frame(
  Alpha = alpha_levels,
  TypeII_Error = round(type2_errors, 3),
  Power = round(power_values, 3)
)

# Display selected rows
print(result_df[c(1, 5, 10, 15, 20), ])

# ===================================================================
# 8. Effect of Sample Size on Error Rates
# ===================================================================

# Demonstrate how sample size affects both types of errors

# Parameters
mu0 <- 0  # Null hypothesis value
mu1 <- 2  # Alternative hypothesis value
sigma <- 3  # Standard deviation
alpha <- 0.05  # Fixed significance level
sample_sizes <- seq(5, 100, by = 5)  # Range of sample sizes

# Calculate Type I and Type II error rates for different sample sizes
type1_errors <- rep(alpha, length(sample_sizes))  # Type I error rate is fixed at alpha
type2_errors <- numeric(length(sa
(Content truncated due to size limit. Use line ranges to read in chunks)