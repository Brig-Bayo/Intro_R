# ===================================================================
# Practical Applications in R
# Sample Size Calculation Course Module
# ===================================================================

# This script provides practical examples of power analysis and sample size
# calculation using various R packages, with a focus on real-world applications.

# ===================================================================
# 1. Introduction to R Packages for Power Analysis
# ===================================================================

# Install required packages (uncomment if needed)
# install.packages(c("pwr", "WebPower", "powerSurvEpi", "longpower", "simr"))

# Load the basic packages
library(pwr)  # Basic power calculations
library(WebPower)  # Web-based power analysis

# ===================================================================
# 2. Sample Size Calculation for Means Comparison
# ===================================================================

# 2.1 One-Sample t-test
# -------------------

# Example: Testing if a new teaching method improves test scores
# H0: μ = 70 vs. H1: μ > 70 (where 70 is the historical average)

# Parameters
mean_diff <- 5        # Expected improvement in test scores
sd <- 12              # Expected standard deviation
alpha <- 0.05         # Significance level
power <- 0.85         # Desired power
alternative <- "greater"  # One-sided test (we expect improvement)

# Calculate effect size
effect_size <- mean_diff / sd  # Cohen's d

# Calculate sample size
result <- pwr.t.test(d = effect_size, 
                    sig.level = alpha, 
                    power = power, 
                    type = "one.sample", 
                    alternative = alternative)
print(result)

# Interpretation
cat("Expected mean difference:", mean_diff, "\n")
cat("Standard deviation:", sd, "\n")
cat("Effect size (Cohen's d):", round(effect_size, 2), "\n")
cat("Required sample size:", ceiling(result$n), "\n")

# 2.2 Two-Sample t-test (Independent Samples)
# ----------------------------------------

# Example: Planning a study to compare two teaching methods
# H0: μ1 = μ2 vs. H1: μ1 ≠ μ2

# Parameters
mean_diff <- 8        # Expected difference in test scores
sd_pooled <- 15       # Expected pooled standard deviation
alpha <- 0.05         # Significance level
power <- 0.85         # Desired power
dropout_rate <- 0.1   # Expected 10% dropout

# Calculate effect size
effect_size <- mean_diff / sd_pooled  # Cohen's d

# Calculate sample size
result <- pwr.t.test(d = effect_size, 
                    sig.level = alpha, 
                    power = power, 
                    type = "two.sample", 
                    alternative = "two.sided")

# Calculate total sample size and adjust for potential dropout
n_per_group <- ceiling(result$n)
n_adjusted <- ceiling(n_per_group / (1 - dropout_rate))
total_n <- 2 * n_adjusted

cat("Effect size (Cohen's d):", round(effect_size, 2), "\n")
cat("Required sample size per group:", n_per_group, "\n")
cat("Adjusted for 10% dropout:", n_adjusted, "per group\n")
cat("Total sample size needed:", total_n, "\n")

# 2.3 Paired t-test
# --------------

# Example: Planning a pre-post intervention study
# H0: μd = 0 vs. H1: μd ≠ 0 (where μd is the mean difference)

# Parameters
mean_diff <- 6        # Expected mean difference (post - pre)
sd_diff <- 10         # Expected standard deviation of differences
alpha <- 0.05         # Significance level
power <- 0.9          # Desired power
dropout_rate <- 0.15  # Expected 15% dropout

# Calculate effect size
effect_size <- mean_diff / sd_diff  # Cohen's d for paired data

# Calculate sample size
result <- pwr.t.test(d = effect_size, 
                    sig.level = alpha, 
                    power = power, 
                    type = "paired", 
                    alternative = "two.sided")

# Adjust for dropout
n_required <- ceiling(result$n)
n_adjusted <- ceiling(n_required / (1 - dropout_rate))

cat("Effect size (Cohen's d for paired data):", round(effect_size, 2), "\n")
cat("Required sample size (without dropout):", n_required, "\n")
cat("Adjusted for 15% dropout:", n_adjusted, "\n")

# ===================================================================
# 3. Sample Size Calculation for Proportions
# ===================================================================

# 3.1 One-Sample Proportion Test
# ---------------------------

# Example: Testing if a new treatment achieves a success rate different from historical rate
# H0: p = 0.6 vs. H1: p ≠ 0.6 (where 0.6 is the historical success rate)

# Parameters
p0 <- 0.6             # Historical success rate
p1 <- 0.75            # Expected success rate with new treatment
alpha <- 0.05         # Significance level
power <- 0.8          # Desired power

# Calculate effect size (h)
h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p0))  # Cohen's h

# Calculate sample size
result <- pwr.p.test(h = h, 
                    sig.level = alpha, 
                    power = power, 
                    alternative = "two.sided")

cat("Historical success rate:", p0 * 100, "%\n")
cat("Expected success rate with new treatment:", p1 * 100, "%\n")
cat("Effect size (Cohen's h):", round(h, 2), "\n")
cat("Required sample size:", ceiling(result$n), "\n")

# 3.2 Two-Sample Proportion Test
# ---------------------------

# Example: Planning a clinical trial with binary outcome
# H0: p1 = p2 vs. H1: p1 ≠ p2

# Parameters
p_control <- 0.3      # Success rate in control group
p_treatment <- 0.5    # Expected success rate in treatment group
alpha <- 0.05         # Significance level
power <- 0.9          # Desired power
dropout_rate <- 0.15  # Expected 15% dropout

# Calculate sample size
result <- power.prop.test(p1 = p_control, 
                         p2 = p_treatment, 
                         sig.level = alpha, 
                         power = power, 
                         alternative = "two.sided")

# Adjust for dropout
n_per_group <- ceiling(result$n)
n_adjusted <- ceiling(n_per_group / (1 - dropout_rate))
total_n <- 2 * n_adjusted

cat("Success rate in control group:", p_control * 100, "%\n")
cat("Expected success rate in treatment group:", p_treatment * 100, "%\n")
cat("Absolute difference:", (p_treatment - p_control) * 100, "percentage points\n")
cat("Required sample size per group:", n_per_group, "\n")
cat("Adjusted for 15% dropout:", n_adjusted, "per group\n")
cat("Total sample size needed:", total_n, "\n")

# 3.3 Sample Size for Risk Ratio
# ---------------------------

# Example: Planning a study to detect a specific risk ratio
# H0: RR = 1 vs. H1: RR ≠ 1

# Parameters
p_control <- 0.2      # Event rate in control group
risk_ratio <- 1.75    # Target risk ratio
alpha <- 0.05         # Significance level
power <- 0.8          # Desired power

# Calculate event rate in treatment group based on risk ratio
p_treatment <- p_control * risk_ratio

# Calculate effect size (h)
h <- 2 * asin(sqrt(p_control)) - 2 * asin(sqrt(p_treatment))  # Cohen's h
h <- abs(h)  # Take absolute value

# Calculate sample size
result <- pwr.2p.test(h = h, 
                     sig.level = alpha, 
                     power = power, 
                     alternative = "two.sided")

cat("Event rate in control group:", p_control * 100, "%\n")
cat("Expected event rate in treatment group:", round(p_treatment * 100, 1), "%\n")
cat("Target risk ratio:", risk_ratio, "\n")
cat("Effect size (Cohen's h):", round(h, 2), "\n")
cat("Required sample size per group:", ceiling(result$n), "\n")
cat("Total sample size:", 2 * ceiling(result$n), "\n")

# ===================================================================
# 4. Sample Size Calculation for Correlation Studies
# ===================================================================

# Example: Planning a study to investigate the relationship between two variables
# H0: ρ = 0 vs. H1: ρ ≠ 0

# Parameters
expected_r <- 0.25    # Expected correlation
alpha <- 0.05         # Significance level
power <- 0.85         # Desired power
dropout_rate <- 0.1   # Expected 10% dropout

# Calculate sample size
result <- pwr.r.test(r = expected_r, 
                    sig.level = alpha, 
                    power = power, 
                    alternative = "two.sided")

# Adjust for dropout
n_required <- ceiling(result$n)
n_adjusted <- ceiling(n_required / (1 - dropout_rate))

cat("Expected correlation:", expected_r, "\n")
cat("Required sample size:", n_required, "\n")
cat("Adjusted for 10% dropout:", n_adjusted, "\n")

# ===================================================================
# 5. Sample Size Calculation for Regression Analyses
# ===================================================================

# Example: Planning a study with multiple predictors
# H0: All βi = 0 vs. H1: At least one βi ≠ 0

# Parameters
r_squared <- 0.25     # Expected R-squared
predictors <- 7       # Number of predictors
alpha <- 0.05         # Significance level
power <- 0.9          # Desired power
dropout_rate <- 0.1   # Expected 10% dropout

# Calculate f-squared
f_squared <- r_squared / (1 - r_squared)

# Calculate sample size
result <- pwr.f2.test(u = predictors, 
                     v = NULL, 
                     f2 = f_squared, 
                     sig.level = alpha, 
                     power = power)
n_required <- ceiling(result$v + predictors + 1)

# Adjust for dropout
n_adjusted <- ceiling(n_required / (1 - dropout_rate))

cat("Expected R-squared:", r_squared, "\n")
cat("Cohen's f-squared:", round(f_squared, 2), "\n")
cat("Required sample size:", n_required, "\n")
cat("Adjusted for 10% dropout:", n_adjusted, "\n")

# Rule of thumb check (10-15 observations per predictor)
rule_of_thumb <- c(10 * predictors, 15 * predictors)
cat("Rule of thumb range:", rule_of_thumb[1], "to", rule_of_thumb[2], "\n")

# ===================================================================
# 6. Sample Size Calculation for ANOVA Designs
# ===================================================================

# 6.1 One-Way ANOVA
# --------------

# Example: Comparing outcomes across four treatment groups
# H0: All μi are equal vs. H1: At least one μi differs

# Parameters
groups <- 4           # Number of groups
effect_size <- 0.25   # Cohen's f (medium effect)
alpha <- 0.05         # Significance level
power <- 0.8          # Desired power

# Calculate sample size
result <- pwr.anova.test(k = groups, 
                        f = effect_size, 
                        sig.level = alpha, 
                        power = power)

cat("Number of groups:", groups, "\n")
cat("Effect size (Cohen's f):", effect_size, "\n")
cat("Required sample size per group:", ceiling(result$n), "\n")
cat("Total sample size:", ceiling(result$n) * groups, "\n")

# 6.2 Repeated Measures ANOVA
# ------------------------

# Example: Planning a study with repeated measures
# H0: All time points have equal means vs. H1: At least one time point differs

# Parameters
groups <- 1           # One group (within-subjects design)
time_points <- 4      # Number of measurements
effect_size <- 0.25   # Expected effect size (Cohen's f)
alpha <- 0.05         # Significance level
power <- 0.85         # Desired power
correlation <- 0.6    # Expected correlation between repeated measures

# Calculate sample size
result <- wp.rmanova(n = NULL, 
                    ng = groups, 
                    nm = time_points, 
                    f = effect_size, 
                    nscor = correlation, 
                    alpha = alpha, 
                    power = power)

cat("Number of time points:", time_points, "\n")
cat("Effect size (Cohen's f):", effect_size, "\n")
cat("Correlation between repeated measures:", correlation, "\n")
cat("Required sample size:", ceiling(result$n), "\n")

# 6.3 Mixed Design ANOVA
# -------------------

# Example: Planning a study with repeated measures and between-subjects factor
# H0: No interaction between time and group vs. H1: Significant interaction

# Parameters
groups <- 3           # Number of treatment groups
time_points <- 3      # Number of time points
effect_size <- 0.25   # Expected effect size for interaction (Cohen's f)
alpha <- 0.05         # Significance level
power <- 0.85         # Desired power
correlation <- 0.6    # Expected correlation between repeated measures
dropout_rate <- 0.15  # Expected 15% dropout

# Calculate sample size
result <- wp.rmanova(n = NULL, 
                    ng = groups, 
                    nm = time_points, 
                    f = effect_size, 
                    nscor = correlation, 
                    alpha = alpha, 
                    power = power)

# Adjust for dropout
n_per_group <- ceiling(result$n)
n_adjusted <- ceiling(n_per_group / (1 - dropout_rate))
total_n <- n_adjusted * groups

cat("Number of groups:", groups, "\n")
cat("Number of time points:", time_points, "\n")
cat("Effect size (Cohen's f):", effect_size, "\n")
cat("Correlation between repeated measures:", correlation, "\n")
cat("Required sample size per group:", n_per_group, "\n")
cat("Adjusted for 15% dropout:", n_adjusted, "per group\n")
cat("Total sample size needed:", total_n, "\n")

# ===================================================================
# 7. Handling Missing Data and Dropout Rates
# ===================================================================

# 7.1 Adjusting Sample Size for Expected Dropout
# ------------------------------------------

# Function to adjust sample size for dropout
adjust_for_dropout <- function(n, dropout_rate) {
  n_adjusted <- ceiling(n / (1 - dropout_rate))
  return(n_adjusted)
}

# Example: Sample size for two-sample t-test with dropout adjustment
effect_size <- 0.5    # Cohen's d
alpha <- 0.05         # Significance level
power <- 0.8          # Desired power
dropout_rates <- c(0, 0.1, 0.2, 0.3, 0.4)  # Different dropout scenarios

# Calculate base sample size
result <- pwr.t.test(d = effect_size, 
                    sig.level = alpha, 
                    power = power, 
                    type = "two.sample", 
                    alternative = "two.sided")
n_base <- ceiling(result$n)

# Adjust for different dropout rates
n_adjusted <- sapply(dropout_rates, function(rate) adjust_for_dropout(n_base, rate))

# Create a data frame for display
dropout_table <- data.frame(
  DropoutRate = dropout_rates * 100,  # Convert to percentage
  SampleSizePerGroup = n_adjusted,
  TotalSampleSize = n_adjusted * 2
)

print(dropout_table)

# 7.2 Planning for Longitudinal Studies with Attrition
# ------------------------------------------------

# Example: Planning a longitudinal study with measurements at baseline, 6 months, and 12 months
# H0: No change over time vs. H1: Significant change over time

# Parameters
effect_size <- 0.4    # Expected effect size at 12 months
alpha <- 0.05         # Significance level
power <- 0.85         # Desired power
dropout_6m <- 0.10    # Expected dropout by 6 months
dropout_12m <- 0.15   # Additional dropout by 12 months

# Calculate total dropout by study end
total_dropout <- dropout_6m + (1 - dropout_6m) * dropout_12m

# Calculate base sample size for two-sample comparison at 12 months
result <- pwr.t.test(d = effect_size, 
                    sig.level = alpha, 
                    power = power, 
                    type = "two.sample", 
                    alternative = "two.sided")
n_base <- ceiling(result$n)

# Adjust for dropout
n_adjusted <- ceiling(n_base / (1 - total_dropout))
total_n <- 2 * n_adjusted

cat("Expected effect size:", effect_size, "\n")
cat("Base sample size per group (no dropout):", n_base, "\n")
cat("Expected dropout by 6 months:", dropout_6m * 100, "%\n")
cat("Additional dropout by 12 months:", dropout_12m * 100, "%\n")
cat("Total expected dropout by study end:", round(total_dropout * 100, 1), "%\n")
cat("Adjusted sample size per group:", n_adjusted, "\n")
cat("Total recruitment target:", total_n, "\n")

# Expected sample sizes at each time point
n_baseline <- total_n
n_6months <- round(n_bas
(Content truncated due to size limit. Use line ranges to read in chunks)