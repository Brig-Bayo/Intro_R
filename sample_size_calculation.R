# ===================================================================
# Sample Size Calculation for Different Study Designs
# Sample Size Calculation Course Module
# ===================================================================

# This script demonstrates sample size calculation methods for various
# study designs using R packages and functions.

# ===================================================================
# 1. Sample Size for Means Comparison
# ===================================================================

# Load required packages
library(pwr)

# 1.1 One-Sample t-test
# -------------------

# Example: Testing if a sample mean differs from a known value
# H0: μ = μ0 vs. H1: μ ≠ μ0

# Parameters
effect_size <- 0.5  # Cohen's d (medium effect)
sig_level <- 0.05   # Significance level (alpha)
power <- 0.8        # Desired power (1 - beta)
type <- "one.sample"  # One-sample test
alternative <- "two.sided"  # Two-sided test

# Calculate sample size
result_one_sample <- pwr.t.test(d = effect_size, 
                               sig.level = sig_level, 
                               power = power, 
                               type = type, 
                               alternative = alternative)
print(result_one_sample)

# Interpretation
cat("Required sample size for one-sample t-test:", ceiling(result_one_sample$n), "\n")

# 1.2 Two-Sample t-test (Independent Samples)
# ----------------------------------------

# Example: Comparing means between two independent groups
# H0: μ1 = μ2 vs. H1: μ1 ≠ μ2

# Parameters
effect_size <- 0.5  # Cohen's d (medium effect)
sig_level <- 0.05   # Significance level (alpha)
power <- 0.8        # Desired power (1 - beta)
type <- "two.sample"  # Two-sample test
alternative <- "two.sided"  # Two-sided test

# Calculate sample size
result_two_sample <- pwr.t.test(d = effect_size, 
                               sig.level = sig_level, 
                               power = power, 
                               type = type, 
                               alternative = alternative)
print(result_two_sample)

# Interpretation
cat("Required sample size per group for two-sample t-test:", ceiling(result_two_sample$n), "\n")
cat("Total sample size:", 2 * ceiling(result_two_sample$n), "\n")

# 1.3 Two-Sample t-test with Unequal Group Sizes
# -------------------------------------------

# Example: Comparing means with unequal allocation ratio
# H0: μ1 = μ2 vs. H1: μ1 ≠ μ2

# Parameters
effect_size <- 0.5  # Cohen's d (medium effect)
sig_level <- 0.05   # Significance level (alpha)
power <- 0.8        # Desired power (1 - beta)
ratio <- 2          # Allocation ratio (n2/n1 = 2)

# Calculate sample size
result_unequal <- pwr.t2n.test(d = effect_size, 
                              sig.level = sig_level, 
                              power = power, 
                              ratio = ratio)
print(result_unequal)

# Interpretation
cat("Required sample size for group 1:", ceiling(result_unequal$n1), "\n")
cat("Required sample size for group 2:", ceiling(result_unequal$n2), "\n")
cat("Total sample size:", ceiling(result_unequal$n1) + ceiling(result_unequal$n2), "\n")

# 1.4 Paired t-test
# --------------

# Example: Comparing means between paired measurements
# H0: μd = 0 vs. H1: μd ≠ 0 (where μd is the mean difference)

# Parameters
effect_size <- 0.5  # Cohen's d (medium effect)
sig_level <- 0.05   # Significance level (alpha)
power <- 0.8        # Desired power (1 - beta)
type <- "paired"    # Paired test
alternative <- "two.sided"  # Two-sided test

# Calculate sample size
result_paired <- pwr.t.test(d = effect_size, 
                           sig.level = sig_level, 
                           power = power, 
                           type = type, 
                           alternative = alternative)
print(result_paired)

# Interpretation
cat("Required number of pairs for paired t-test:", ceiling(result_paired$n), "\n")

# 1.5 Sample Size Based on Raw Values
# --------------------------------

# Example: Calculate sample size using means and standard deviations
# instead of standardized effect size

# Parameters
mean1 <- 85  # Mean of group 1
mean2 <- 80  # Mean of group 2
sd1 <- 10    # Standard deviation of group 1
sd2 <- 10    # Standard deviation of group 2
sig_level <- 0.05  # Significance level (alpha)
power <- 0.8       # Desired power (1 - beta)

# Calculate pooled standard deviation
sd_pooled <- sqrt((sd1^2 + sd2^2) / 2)

# Calculate Cohen's d
d <- abs(mean1 - mean2) / sd_pooled

# Calculate sample size
result_raw <- pwr.t.test(d = d, 
                        sig.level = sig_level, 
                        power = power, 
                        type = "two.sample", 
                        alternative = "two.sided")

# Interpretation
cat("Mean difference:", abs(mean1 - mean2), "\n")
cat("Pooled standard deviation:", sd_pooled, "\n")
cat("Calculated effect size (Cohen's d):", round(d, 2), "\n")
cat("Required sample size per group:", ceiling(result_raw$n), "\n")

# 1.6 Power Curve for Different Sample Sizes
# ---------------------------------------

# Generate power curve for different sample sizes
sample_sizes <- seq(10, 100, by = 5)
power_values <- sapply(sample_sizes, function(n) {
  pwr.t.test(n = n, 
             d = 0.5, 
             sig.level = 0.05, 
             type = "two.sample", 
             alternative = "two.sided")$power
})

# Plot power curve
plot(sample_sizes, power_values, type = "b", 
     xlab = "Sample Size per Group", 
     ylab = "Power", 
     main = "Power Curve for Two-Sample t-test (d = 0.5)",
     col = "blue", pch = 19)
abline(h = 0.8, lty = 2, col = "red")
text(max(sample_sizes) * 0.8, 0.82, "Power = 0.8", col = "red")

# ===================================================================
# 2. Sample Size for Proportions
# ===================================================================

# 2.1 One-Sample Proportion Test
# ---------------------------

# Example: Testing if a proportion differs from a known value
# H0: p = p0 vs. H1: p ≠ p0

# Parameters
p0 <- 0.5        # Null hypothesis proportion
p1 <- 0.65       # Alternative hypothesis proportion
sig_level <- 0.05  # Significance level (alpha)
power <- 0.8       # Desired power (1 - beta)
alternative <- "two.sided"  # Two-sided test

# Calculate effect size (h)
h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p0))  # Cohen's h

# Calculate sample size
result_prop <- pwr.p.test(h = h, 
                         sig.level = sig_level, 
                         power = power, 
                         alternative = alternative)
print(result_prop)

# Interpretation
cat("Effect size (Cohen's h):", round(h, 3), "\n")
cat("Required sample size for one-sample proportion test:", ceiling(result_prop$n), "\n")

# 2.2 Two-Sample Proportion Test
# ---------------------------

# Example: Comparing proportions between two independent groups
# H0: p1 = p2 vs. H1: p1 ≠ p2

# Parameters
p1 <- 0.4        # Proportion in group 1
p2 <- 0.6        # Proportion in group 2
sig_level <- 0.05  # Significance level (alpha)
power <- 0.8       # Desired power (1 - beta)
alternative <- "two.sided"  # Two-sided test

# Calculate effect size (h)
h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))  # Cohen's h
h <- abs(h)  # Take absolute value

# Calculate sample size
result_2prop <- pwr.2p.test(h = h, 
                           sig.level = sig_level, 
                           power = power, 
                           alternative = alternative)
print(result_2prop)

# Interpretation
cat("Effect size (Cohen's h):", round(h, 3), "\n")
cat("Required sample size per group:", ceiling(result_2prop$n), "\n")
cat("Total sample size:", 2 * ceiling(result_2prop$n), "\n")

# Alternative approach using power.prop.test
result_prop_alt <- power.prop.test(p1 = p1, 
                                  p2 = p2, 
                                  sig.level = sig_level, 
                                  power = power, 
                                  alternative = "two.sided")
print(result_prop_alt)

# 2.3 Sample Size for Relative Risk or Odds Ratio
# -------------------------------------------

# Example: Calculate sample size for detecting a specific relative risk

# Parameters
p1 <- 0.2        # Event rate in control group
RR <- 1.5        # Target relative risk
sig_level <- 0.05  # Significance level (alpha)
power <- 0.8       # Desired power (1 - beta)

# Calculate p2 based on relative risk
p2 <- p1 * RR

# Calculate effect size (h)
h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))  # Cohen's h
h <- abs(h)  # Take absolute value

# Calculate sample size
result_rr <- pwr.2p.test(h = h, 
                        sig.level = sig_level, 
                        power = power, 
                        alternative = "two.sided")

# Interpretation
cat("Event rate in control group:", p1, "\n")
cat("Event rate in treatment group (based on RR =", RR, "):", p2, "\n")
cat("Effect size (Cohen's h):", round(h, 3), "\n")
cat("Required sample size per group:", ceiling(result_rr$n), "\n")
cat("Total sample size:", 2 * ceiling(result_rr$n), "\n")

# Example: Calculate sample size for detecting a specific odds ratio

# Parameters
p1 <- 0.2        # Event rate in control group
OR <- 2.0        # Target odds ratio
sig_level <- 0.05  # Significance level (alpha)
power <- 0.8       # Desired power (1 - beta)

# Calculate p2 based on odds ratio
odds1 <- p1 / (1 - p1)
odds2 <- odds1 * OR
p2 <- odds2 / (1 + odds2)

# Calculate effect size (h)
h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))  # Cohen's h
h <- abs(h)  # Take absolute value

# Calculate sample size
result_or <- pwr.2p.test(h = h, 
                        sig.level = sig_level, 
                        power = power, 
                        alternative = "two.sided")

# Interpretation
cat("\nEvent rate in control group:", p1, "\n")
cat("Event rate in treatment group (based on OR =", OR, "):", round(p2, 3), "\n")
cat("Effect size (Cohen's h):", round(h, 3), "\n")
cat("Required sample size per group:", ceiling(result_or$n), "\n")
cat("Total sample size:", 2 * ceiling(result_or$n), "\n")

# 2.4 Adjusting for Expected Dropout
# -------------------------------

# Example: Adjust sample size for expected dropout rate

# Parameters
n_required <- ceiling(result_2prop$n)  # Required sample size per group
dropout_rate <- 0.15                   # Expected dropout rate

# Adjust sample size
n_adjusted <- ceiling(n_required / (1 - dropout_rate))

# Interpretation
cat("\nRequired sample size per group (without dropout):", n_required, "\n")
cat("Expected dropout rate:", dropout_rate * 100, "%\n")
cat("Adjusted sample size per group:", n_adjusted, "\n")
cat("Total adjusted sample size:", 2 * n_adjusted, "\n")

# ===================================================================
# 3. Sample Size for Correlation Studies
# ===================================================================

# 3.1 Test for Correlation
# ---------------------

# Example: Testing if a correlation differs from zero
# H0: ρ = 0 vs. H1: ρ ≠ 0

# Parameters
r <- 0.3         # Expected correlation coefficient
sig_level <- 0.05  # Significance level (alpha)
power <- 0.8       # Desired power (1 - beta)
alternative <- "two.sided"  # Two-sided test

# Calculate sample size
result_cor <- pwr.r.test(r = r, 
                        sig.level = sig_level, 
                        power = power, 
                        alternative = alternative)
print(result_cor)

# Interpretation
cat("Expected correlation:", r, "\n")
cat("Required sample size:", ceiling(result_cor$n), "\n")

# 3.2 Sample Size for Different Correlation Magnitudes
# ------------------------------------------------

# Calculate sample sizes for different correlation magnitudes
correlations <- seq(0.1, 0.9, by = 0.1)
sample_sizes_cor <- sapply(correlations, function(r) {
  ceiling(pwr.r.test(r = r, 
                    sig.level = 0.05, 
                    power = 0.8, 
                    alternative = "two.sided")$n)
})

# Create a data frame for display
correlation_table <- data.frame(
  Correlation = correlations,
  SampleSize = sample_sizes_cor
)

print(correlation_table)

# Plot the relationship
plot(correlations, sample_sizes_cor, type = "b", 
     xlab = "Correlation Coefficient (r)", 
     ylab = "Required Sample Size", 
     main = "Sample Size vs. Correlation Magnitude",
     col = "green", pch = 19)

# 3.3 Power Analysis for Correlation
# -------------------------------

# Generate power curve for different sample sizes
sample_sizes_cor_power <- seq(10, 100, by = 5)
power_values_cor <- sapply(sample_sizes_cor_power, function(n) {
  pwr.r.test(n = n, 
             r = 0.3, 
             sig.level = 0.05, 
             alternative = "two.sided")$power
})

# Plot power curve
plot(sample_sizes_cor_power, power_values_cor, type = "b", 
     xlab = "Sample Size", 
     ylab = "Power", 
     main = "Power Curve for Correlation Test (r = 0.3)",
     col = "purple", pch = 19)
abline(h = 0.8, lty = 2, col = "red")
text(max(sample_sizes_cor_power) * 0.8, 0.82, "Power = 0.8", col = "red")

# ===================================================================
# 4. Sample Size for Regression Analyses
# ===================================================================

# 4.1 Simple Linear Regression
# -------------------------

# Example: Testing if a regression coefficient differs from zero
# H0: β = 0 vs. H1: β ≠ 0

# For simple linear regression, the sample size calculation is similar to correlation
r_squared <- 0.1  # Expected R-squared (proportion of variance explained)
predictors <- 1    # Number of predictors

# Calculate f-squared (Cohen's f-squared)
f_squared <- r_squared / (1 - r_squared)

# Calculate sample size
result_reg <- pwr.f2.test(u = predictors, 
                         v = NULL, 
                         f2 = f_squared, 
                         sig.level = 0.05, 
                         power = 0.8)
n_required <- ceiling(result_reg$v + predictors + 1)

# Interpretation
cat("\nSimple Linear Regression:\n")
cat("Expected R-squared:", r_squared, "\n")
cat("Cohen's f-squared:", round(f_squared, 3), "\n")
cat("Required sample size:", n_required, "\n")

# 4.2 Multiple Linear Regression
# ---------------------------

# Example: Testing if a set of predictors explains significant variance
# H0: All βi = 0 vs. H1: At least one βi ≠ 0

# Parameters
r_squared <- 0.2    # Expected R-squared
predictors <- 5      # Number of predictors
sig_level <- 0.05    # Significance level (alpha)
power <- 0.8         # Desired power (1 - beta)

# Calculate f-squared
f_squared <- r_squared / (1 - r_squared)

# Calculate sample size
result_multi_reg <- pwr.f2.test(u = predictors, 
                               v = NULL, 
                               f2 = f_squared, 
                               sig.level = sig_level, 
                               power = power)
n_required_multi <- ceiling(result_multi_reg$v + predictors + 1)

# Interpretation
cat("\nMultiple Linear Regression:\n")
cat("Expected R-squared:", r_squared, "\n")
cat("Number of predictors:", predictors, "\n")
cat("Cohen's f-squared:", round(f_squared, 3), "\n")
cat("Required sample size:", n_required_multi, "\n")

# 4.3 Testing a Specific Predictor in Multiple Regression
# ---------------------------------------------------

# Example: Testing whether a specific predictor contributes significantly
# beyond other predictors

# Parameters
r_squared_full <- 0.3     # R-squared for full model
r_squared_reduced <- 0.2  # R-squared for model without focal predictor(s)
predictors_full <- 6      # Number of predictors in full model
predictors_reduced <- 5   # Number of predictors in reduced model
predictors_tested <- predictors_full - predictors_reduced  # Number of focal predictors

# Calculate f-squared for the change in R-squared
f_squared_change <- (r_squared_full - r_squared_reduced) / (1 - r_squared_full)

# Calculate sample size
result_pred <- pwr.f2.test(u = predictors_tested, 
                        
(Content truncated due to size limit. Use line ranges to read in chunks)