# ===================================================================
# Advanced Topics and Case Studies
# Sample Size Calculation Course Module
# ===================================================================

# This script provides examples of advanced sample size calculation topics
# and case studies for complex research designs.

# ===================================================================
# 1. Multiple Comparisons and Adjustments
# ===================================================================

# 1.1 The Multiple Comparisons Problem
# ---------------------------------

# Demonstrate how the probability of Type I error increases with multiple tests
alpha <- 0.05  # Significance level for individual tests
num_tests <- c(1, 2, 5, 10, 20, 50, 100)  # Number of independent tests

# Calculate family-wise error rate (FWER)
fwer <- 1 - (1 - alpha)^num_tests

# Create a data frame for display
fwer_table <- data.frame(
  NumTests = num_tests,
  FWER = round(fwer, 3),
  Percentage = paste0(round(fwer * 100, 1), "%")
)

print(fwer_table)

# Plot the relationship
plot(num_tests, fwer, type = "b", 
     xlab = "Number of Tests", ylab = "Family-wise Error Rate", 
     main = "Inflation of Type I Error with Multiple Tests",
     ylim = c(0, 1), col = "red", pch = 19)
abline(h = 0.05, lty = 2)
text(max(num_tests) * 0.8, 0.07, "α = 0.05", col = "blue")

# 1.2 Bonferroni Correction
# ----------------------

# Example: Sample size for multiple comparisons
# Comparing 4 treatment groups to a control group

# Parameters
groups <- 5           # Total number of groups (4 treatments + 1 control)
comparisons <- 4      # Number of comparisons (each treatment vs. control)
effect_size <- 0.5    # Expected effect size (Cohen's d)
power <- 0.8          # Desired power for each comparison
alpha <- 0.05         # Overall significance level

# Calculate adjusted alpha using Bonferroni correction
alpha_adjusted <- alpha / comparisons

# Calculate sample size without adjustment
library(pwr)
result_unadjusted <- pwr.t.test(d = effect_size, power = power, 
                               sig.level = alpha, type = "two.sample")

# Calculate sample size with Bonferroni adjustment
result_adjusted <- pwr.t.test(d = effect_size, power = power, 
                             sig.level = alpha_adjusted, type = "two.sample")

cat("Number of comparisons:", comparisons, "\n")
cat("Unadjusted alpha:", alpha, "\n")
cat("Bonferroni-adjusted alpha:", alpha_adjusted, "\n")
cat("Sample size per group (unadjusted):", ceiling(result_unadjusted$n), "\n")
cat("Sample size per group (Bonferroni-adjusted):", ceiling(result_adjusted$n), "\n")
cat("Increase in sample size:", 
    round((ceiling(result_adjusted$n) / ceiling(result_unadjusted$n) - 1) * 100), 
    "%\n")

# 1.3 Holm's Sequential Procedure
# ----------------------------

# Simulate Holm's procedure
set.seed(123)
p_values <- c(0.001, 0.008, 0.039, 0.041, 0.09)  # Example p-values from 5 tests
n_tests <- length(p_values)

# Sort p-values in ascending order
p_sorted <- sort(p_values)
indices <- order(p_values)

# Apply Holm's procedure
alpha_holm <- alpha / (n_tests:1)
reject_holm <- p_sorted <= alpha_holm

# Create a data frame for display
holm_table <- data.frame(
  Test = indices,
  P_Value = p_values,
  Rank = rank(p_values),
  Adjusted_Alpha = alpha_holm[rank(p_values)],
  Reject_H0 = p_values <= alpha_holm[rank(p_values)]
)

# Sort by test number for display
holm_table <- holm_table[order(holm_table$Test), ]
print(holm_table)

# 1.4 False Discovery Rate (FDR) Control
# ----------------------------------

# Simulate Benjamini-Hochberg procedure for FDR control
set.seed(456)
p_values <- c(0.001, 0.008, 0.039, 0.041, 0.09)  # Example p-values from 5 tests
n_tests <- length(p_values)
q <- 0.10  # Target false discovery rate

# Sort p-values in ascending order
p_sorted <- sort(p_values)
indices <- order(p_values)

# Apply Benjamini-Hochberg procedure
alpha_bh <- q * (1:n_tests) / n_tests
reject_bh <- p_sorted <= alpha_bh

# Find largest k such that p_k ≤ (k/m)q
if (any(reject_bh)) {
  k_max <- max(which(reject_bh))
  reject_indices <- indices[1:k_max]
} else {
  reject_indices <- integer(0)
}

# Create a data frame for display
bh_table <- data.frame(
  Test = 1:n_tests,
  P_Value = p_values,
  Rank = rank(p_values),
  BH_Critical_Value = alpha_bh[rank(p_values)],
  Reject_H0 = 1:n_tests %in% reject_indices
)

# Sort by test number for display
bh_table <- bh_table[order(bh_table$Test), ]
print(bh_table)

# ===================================================================
# 2. Adaptive Designs
# ===================================================================

# 2.1 Group Sequential Design
# ------------------------

# Example: Group sequential design with O'Brien-Fleming boundaries
# Using the gsDesign package
if (!requireNamespace("gsDesign", quietly = TRUE)) {
  install.packages("gsDesign")
}
library(gsDesign)

# Parameters
effect_size <- 0.5    # Expected effect size (standardized)
alpha <- 0.05         # Overall Type I error rate
beta <- 0.2           # Type II error rate (1 - power)
interim_analyses <- 2  # Number of interim analyses (plus final analysis)

# Calculate fixed design sample size
n_fixed <- ceiling(2 * ((qnorm(1 - alpha/2) + qnorm(1 - beta)) / effect_size)^2)

# Design group sequential trial with O'Brien-Fleming boundaries
design <- gsDesign(k = interim_analyses + 1,  # Total number of analyses
                  test.type = 2,  # Two-sided test
                  alpha = alpha,
                  beta = beta,
                  sfu = sfOBF)  # O'Brien-Fleming spending function

# Extract key information
n_max <- ceiling(design$n.I[interim_analyses + 1] * n_fixed)  # Maximum sample size
expected_n <- ceiling(design$en * n_fixed)  # Expected sample size under H1
stopping_boundaries <- data.frame(
  Analysis = 1:(interim_analyses + 1),
  Information_Fraction = design$timing,
  Cumulative_Sample_Size = ceiling(design$n.I * n_fixed),
  Efficacy_Z = design$upper$bound,
  Futility_Z = design$lower$bound,
  Efficacy_p = round(2 * (1 - pnorm(design$upper$bound)), 4),
  Futility_p = round(2 * pnorm(design$lower$bound), 4)
)

# Print results
cat("Fixed design sample size:", n_fixed, "\n")
cat("Group sequential design maximum sample size:", n_max, 
    "(", round((n_max/n_fixed - 1) * 100, 1), "% increase)\n", sep = "")
cat("Expected sample size under H1:", expected_n, 
    "(", round((1 - expected_n/n_fixed) * 100, 1), "% reduction)\n", sep = "")

print(stopping_boundaries)

# Plot the design
plot(design, plottype = "boundaries")

# 2.2 Sample Size Re-estimation
# -------------------------

# Example: Blinded sample size re-estimation based on observed variance
# Simulate a trial with sample size re-estimation

# Initial parameters
mean_diff <- 4        # Expected mean difference
sd_initial <- 10      # Initial estimate of standard deviation
alpha <- 0.05         # Significance level
power <- 0.9          # Desired power
max_increase <- 0.5   # Maximum allowed increase in sample size (50%)

# Initial sample size calculation
effect_size_initial <- mean_diff / sd_initial
result_initial <- pwr.t.test(d = effect_size_initial, 
                            sig.level = alpha, 
                            power = power, 
                            type = "two.sample")
n_initial <- ceiling(result_initial$n)

cat("\nSample Size Re-estimation:\n")
cat("Initial parameters:\n")
cat("  Expected mean difference:", mean_diff, "\n")
cat("  Initial SD estimate:", sd_initial, "\n")
cat("  Initial effect size (d):", round(effect_size_initial, 2), "\n")
cat("  Initial sample size per group:", n_initial, "\n")

# Simulate interim analysis (after 50% enrollment)
set.seed(789)
n_interim <- floor(n_initial / 2)
group1_interim <- rnorm(n_interim, mean = 0, sd = 12)  # True SD is 12, not 10
group2_interim <- rnorm(n_interim, mean = mean_diff, sd = 12)

# Calculate pooled SD at interim
sd_pooled_interim <- sqrt(((n_interim - 1) * var(group1_interim) + 
                          (n_interim - 1) * var(group2_interim)) / 
                         (2 * n_interim - 2))

# Recalculate sample size based on updated SD
effect_size_updated <- mean_diff / sd_pooled_interim
result_updated <- pwr.t.test(d = effect_size_updated, 
                            sig.level = alpha, 
                            power = power, 
                            type = "two.sample")
n_updated <- ceiling(result_updated$n)

# Apply maximum increase constraint
n_max_allowed <- ceiling(n_initial * (1 + max_increase))
n_final <- min(n_updated, n_max_allowed)

cat("\nInterim analysis results:\n")
cat("  Observed pooled SD:", round(sd_pooled_interim, 2), "\n")
cat("  Updated effect size (d):", round(effect_size_updated, 2), "\n")
cat("  Recalculated sample size per group:", n_updated, "\n")
cat("  Maximum allowed sample size per group:", n_max_allowed, "\n")
cat("  Final adjusted sample size per group:", n_final, "\n")
cat("  Increase from initial sample size:", 
    round((n_final/n_initial - 1) * 100, 1), "%\n", sep = "")

# ===================================================================
# 3. Sequential Analysis
# ===================================================================

# 3.1 Sequential Probability Ratio Test (SPRT)
# ----------------------------------------

# Simulate a Sequential Probability Ratio Test for comparing two proportions

# Parameters
p0 <- 0.5             # Proportion under null hypothesis
p1 <- 0.7             # Proportion under alternative hypothesis
alpha <- 0.05         # Type I error rate
beta <- 0.2           # Type II error rate
n_max <- 200          # Maximum sample size
n_sim <- 100          # Number of simulations

# Calculate SPRT boundaries
A <- beta / (1 - alpha)  # Lower boundary
B <- (1 - beta) / alpha  # Upper boundary

# Function to perform SPRT for a single trial
run_sprt <- function(true_p) {
  n <- 0
  log_LR <- 0  # Log likelihood ratio
  
  while (n < n_max) {
    # Generate new observation
    n <- n + 1
    x <- rbinom(1, 1, true_p)
    
    # Update log likelihood ratio
    if (x == 1) {
      log_LR <- log_LR + log(p1 / p0)
    } else {
      log_LR <- log_LR + log((1 - p1) / (1 - p0))
    }
    
    # Check stopping boundaries
    if (log_LR <= log(A)) {
      return(list(decision = "Accept H0", n = n))
    } else if (log_LR >= log(B)) {
      return(list(decision = "Reject H0", n = n))
    }
  }
  
  # If maximum sample size reached without decision
  if (log_LR > log(A) && log_LR < log(B)) {
    return(list(decision = "Inconclusive", n = n))
  }
}

# Simulate trials under null hypothesis (H0: p = p0)
set.seed(101)
results_h0 <- replicate(n_sim, run_sprt(p0), simplify = FALSE)
decisions_h0 <- sapply(results_h0, function(x) x$decision)
sample_sizes_h0 <- sapply(results_h0, function(x) x$n)

# Simulate trials under alternative hypothesis (H1: p = p1)
results_h1 <- replicate(n_sim, run_sprt(p1), simplify = FALSE)
decisions_h1 <- sapply(results_h1, function(x) x$decision)
sample_sizes_h1 <- sapply(results_h1, function(x) x$n)

# Calculate error rates and average sample sizes
type_I_error <- mean(decisions_h0 == "Reject H0")
power <- mean(decisions_h1 == "Reject H0")
avg_n_h0 <- mean(sample_sizes_h0)
avg_n_h1 <- mean(sample_sizes_h1)
inconclusive_h0 <- mean(decisions_h0 == "Inconclusive")
inconclusive_h1 <- mean(decisions_h1 == "Inconclusive")

# Calculate fixed design sample size
p_pooled <- (p0 + p1) / 2
fixed_n <- ceiling(2 * (qnorm(1 - alpha/2) + qnorm(1 - beta))^2 * 
                  p_pooled * (1 - p_pooled) / (p1 - p0)^2)

# Print results
cat("\nSequential Probability Ratio Test (SPRT):\n")
cat("SPRT Boundaries: A =", A, ", B =", B, "\n")
cat("Type I error rate:", round(type_I_error, 3), "\n")
cat("Power:", round(power, 3), "\n")
cat("Average sample size under H0:", round(avg_n_h0, 1), "\n")
cat("Average sample size under H1:", round(avg_n_h1, 1), "\n")
cat("Inconclusive rate under H0:", round(inconclusive_h0, 3), "\n")
cat("Inconclusive rate under H1:", round(inconclusive_h1, 3), "\n")
cat("Fixed design sample size:", fixed_n, "\n")
cat("Sample size reduction under H0:", round((1 - avg_n_h0/fixed_n) * 100, 1), "%\n")
cat("Sample size reduction under H1:", round((1 - avg_n_h1/fixed_n) * 100, 1), "%\n")

# 3.2 Triangular Test
# ---------------

# Simulate a triangular test for comparing two means
# This is a simplified implementation

# Parameters
delta <- 0.5          # Standardized effect size
alpha <- 0.05         # Type I error rate
beta <- 0.2           # Type II error rate
max_n <- 200          # Maximum sample size per group

# Calculate boundaries for triangular test
theta <- delta / 2
a <- 2 * log((1 - beta) / alpha) / theta
b <- 2 * log((1 - alpha) / beta) / theta

# Function to run a single triangular test
run_triangular_test <- function(true_delta) {
  n <- 0
  S <- 0  # Cumulative sum of differences
  V <- 0  # Information (proportional to sample size)
  
  while (n < max_n) {
    # Generate new pair of observations
    n <- n + 1
    x1 <- rnorm(1, mean = 0, sd = 1)
    x2 <- rnorm(1, mean = true_delta, sd = 1)
    diff <- x2 - x1
    
    # Update statistics
    S <- S + diff
    V <- V + 1  # Information increases by 1 for each pair
    
    # Check boundaries
    if (S >= a + b * V/2) {
      return(list(decision = "Reject H0", n = n))
    } else if (S <= -a + b * V/2) {
      return(list(decision = "Accept H0", n = n))
    }
  }
  
  # If maximum sample size reached without decision
  return(list(decision = "Inconclusive", n = n))
}

# Simulate trials under null hypothesis (H0: delta = 0)
set.seed(202)
results_tri_h0 <- replicate(n_sim, run_triangular_test(0), simplify = FALSE)
decisions_tri_h0 <- sapply(results_tri_h0, function(x) x$decision)
sample_sizes_tri_h0 <- sapply(results_tri_h0, function(x) x$n)

# Simulate trials under alternative hypothesis (H1: delta = delta)
results_tri_h1 <- replicate(n_sim, run_triangular_test(delta), simplify = FALSE)
decisions_tri_h1 <- sapply(results_tri_h1, function(x) x$decision)
sample_sizes_tri_h1 <- sapply(results_tri_h1, function(x) x$n)

# Calculate error rates and average sample sizes
type_I_error_tri <- mean(decisions_tri_h0 == "Reject H0")
power_tri <- mean(decisions_tri_h1 == "Reject H0")
avg_n_tri_h0 <- mean(sample_sizes_tri_h0)
avg_n_tri_h1 <- mean(sample_sizes_tri_h1)

# Calculate fixed design sample size
fixed_n_tri <- ceiling(2 * (qnorm(1 - alpha/2) + qnorm(1 - beta))^2 / delta^2)

# Print results
cat("\nTriangular Test:\n")
cat("Type I error rate:", round(type_I_error_tri, 3), "\n")
cat("Power:", round(power_tri, 3), "\n")
cat("Average sample size under H0:", round(avg_n_tri_h0, 1), "\n")
cat("Average sample size under H1:", round(avg_n_tri_h1, 1), "\n")
cat("Fixed design sample size:", fixed_n_tri, "\n")
cat("Sample size reduction under H0:", round((1 - avg_n_tri_h0/fixed_n_tri) * 100, 1), "%\n")
cat("Sample size reduction under H1:", round((1 - avg_n_tri_h1/fixed_n_tri) * 100, 1), "%\n")

# ===================================================================
# 4. Non-parametric Approaches
# ===================================================================

# 4.1 Sample Size for Wilcoxon Rank-Sum Test
# --------------------------------------

# Example: Sample size calculation for Wilcoxon rank-sum test
# Using the ARE method and simulation

# Parameters
effect_size <- 0.5    # Cohen's d
alpha <- 0.05         # Significance level
power <- 0.8          # Desired power
distribution <- "normal"  # Data distribution

# Calculate sample size for t-test
result_t <- pwr.t.test(d = effect_size, power = power, sig.level = alpha, 
                      type = "two.sample", alternative = "two.sided")
n_t <- ceiling(result_t$n)

# Calculate sample size for Wilcoxon test using ARE method
are <- 0.955  # Asymptotic relative efficiency under normality
n_wilcoxon_are <- ceiling(n_t / are)

cat("\nSample Size for Non-parametric Tests:\n")
cat("Sample size for t-test:", n_t, "per group\n")
cat("Sample size f
(Content truncated due to size limit. Use line ranges to read in chunks)