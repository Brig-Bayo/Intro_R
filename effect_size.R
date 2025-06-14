# ===================================================================
# Effect Size Measures
# Sample Size Calculation Course Module
# ===================================================================

# This script demonstrates various effect size measures and how to calculate
# them in R for different types of statistical analyses.

# ===================================================================
# 1. Effect Size for Mean Differences
# ===================================================================

# 1.1 Cohen's d for Independent Samples
# ------------------------------------

# Generate sample data
set.seed(123)
group1 <- rnorm(30, mean = 75, sd = 10)
group2 <- rnorm(30, mean = 85, sd = 10)

# Calculate Cohen's d
mean_diff <- mean(group2) - mean(group1)
pooled_sd <- sqrt(((length(group1) - 1) * var(group1) + 
                   (length(group2) - 1) * var(group2)) / 
                  (length(group1) + length(group2) - 2))
cohens_d <- mean_diff / pooled_sd

cat("Mean of group 1:", round(mean(group1), 2), "\n")
cat("Mean of group 2:", round(mean(group2), 2), "\n")
cat("Mean difference:", round(mean_diff, 2), "\n")
cat("Pooled standard deviation:", round(pooled_sd, 2), "\n")
cat("Cohen's d:", round(cohens_d, 2), "\n")

# Interpretation of Cohen's d
if (abs(cohens_d) < 0.2) {
  cat("Interpretation: Small effect size\n")
} else if (abs(cohens_d) < 0.8) {
  cat("Interpretation: Medium effect size\n")
} else {
  cat("Interpretation: Large effect size\n")
}

# Visualize the effect size
library(ggplot2)

# Combine data for plotting
data <- data.frame(
  Value = c(group1, group2),
  Group = factor(rep(c("Group 1", "Group 2"), each = 30))
)

# Create density plot
ggplot(data, aes(x = Value, fill = Group)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = mean(group1), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = mean(group2), linetype = "dashed", color = "red") +
  annotate("text", x = mean(group1) - 5, y = 0.04, label = paste("Mean =", round(mean(group1), 1)), color = "blue") +
  annotate("text", x = mean(group2) + 5, y = 0.04, label = paste("Mean =", round(mean(group2), 1)), color = "red") +
  annotate("text", x = mean(c(mean(group1), mean(group2))), y = 0.01, 
           label = paste("Cohen's d =", round(cohens_d, 2)), color = "black", size = 5) +
  labs(title = "Visualization of Cohen's d Effect Size",
       subtitle = "Comparing distributions of two independent groups",
       x = "Value", y = "Density") +
  theme_minimal()

# 1.2 Cohen's d for Paired Samples
# -------------------------------

# Generate paired sample data
set.seed(456)
pre_test <- rnorm(25, mean = 70, sd = 10)
post_test <- pre_test + rnorm(25, mean = 8, sd = 5)  # Post-test scores are related to pre-test

# Calculate Cohen's d for paired data
diff <- post_test - pre_test
mean_diff_paired <- mean(diff)
sd_diff <- sd(diff)
cohens_d_paired <- mean_diff_paired / sd_diff

cat("\nPaired Samples:\n")
cat("Mean pre-test:", round(mean(pre_test), 2), "\n")
cat("Mean post-test:", round(mean(post_test), 2), "\n")
cat("Mean difference:", round(mean_diff_paired, 2), "\n")
cat("Standard deviation of differences:", round(sd_diff, 2), "\n")
cat("Cohen's d for paired data:", round(cohens_d_paired, 2), "\n")

# 1.3 Glass's Delta
# ---------------

# Used when variances are unequal and one group is considered a control group
set.seed(789)
control_group <- rnorm(30, mean = 50, sd = 8)
treatment_group <- rnorm(30, mean = 60, sd = 12)  # Different variance

# Calculate Glass's delta
mean_diff_glass <- mean(treatment_group) - mean(control_group)
glass_delta <- mean_diff_glass / sd(control_group)  # Using SD of control group only

cat("\nGlass's Delta:\n")
cat("Mean of control group:", round(mean(control_group), 2), "\n")
cat("Mean of treatment group:", round(mean(treatment_group), 2), "\n")
cat("Mean difference:", round(mean_diff_glass, 2), "\n")
cat("Standard deviation of control group:", round(sd(control_group), 2), "\n")
cat("Glass's delta:", round(glass_delta, 2), "\n")

# 1.4 Hedges' g
# -----------

# Correction for small sample sizes
n1 <- length(group1)
n2 <- length(group2)
correction_factor <- 1 - (3 / (4 * (n1 + n2 - 2) - 1))
hedges_g <- cohens_d * correction_factor

cat("\nHedges' g (Corrected Cohen's d):\n")
cat("Cohen's d:", round(cohens_d, 2), "\n")
cat("Correction factor:", round(correction_factor, 4), "\n")
cat("Hedges' g:", round(hedges_g, 2), "\n")

# ===================================================================
# 2. Effect Size for ANOVA
# ===================================================================

# 2.1 Eta-squared (η²)
# ------------------

# Generate data for one-way ANOVA
set.seed(101)
group_A <- rnorm(20, mean = 75, sd = 8)
group_B <- rnorm(20, mean = 80, sd = 8)
group_C <- rnorm(20, mean = 85, sd = 8)

# Combine data
scores <- c(group_A, group_B, group_C)
groups <- factor(rep(c("A", "B", "C"), each = 20))
data_anova <- data.frame(Score = scores, Group = groups)

# Perform ANOVA
anova_result <- aov(Score ~ Group, data = data_anova)
anova_summary <- summary(anova_result)
print(anova_summary)

# Calculate eta-squared
ss_between <- anova_summary[[1]][1, 2]  # Sum of squares between groups
ss_total <- ss_between + anova_summary[[1]][2, 2]  # Total sum of squares
eta_squared <- ss_between / ss_total

cat("\nEta-squared (η²):", round(eta_squared, 3), "\n")

# Interpretation of eta-squared
if (eta_squared < 0.06) {
  cat("Interpretation: Small effect size\n")
} else if (eta_squared < 0.14) {
  cat("Interpretation: Medium effect size\n")
} else {
  cat("Interpretation: Large effect size\n")
}

# 2.2 Partial Eta-squared (η²p)
# ---------------------------

# For one-way ANOVA, partial eta-squared equals eta-squared
# For more complex designs, they differ
partial_eta_squared <- eta_squared
cat("Partial eta-squared (η²p):", round(partial_eta_squared, 3), "\n")

# 2.3 Omega-squared (ω²)
# --------------------

# Less biased estimate of effect size for ANOVA
df_between <- anova_summary[[1]][1, 1]  # Degrees of freedom between groups
df_within <- anova_summary[[1]][2, 1]  # Degrees of freedom within groups
ms_within <- anova_summary[[1]][2, 3]  # Mean square within groups
n_total <- length(scores)  # Total sample size

omega_squared <- (ss_between - (df_between * ms_within)) / 
                (ss_total + ms_within)

cat("Omega-squared (ω²):", round(omega_squared, 3), "\n")

# 2.4 Cohen's f
# -----------

# Convert eta-squared to Cohen's f
cohens_f <- sqrt(eta_squared / (1 - eta_squared))
cat("Cohen's f:", round(cohens_f, 3), "\n")

# Interpretation of Cohen's f
if (cohens_f < 0.1) {
  cat("Interpretation: Small effect size\n")
} else if (cohens_f < 0.4) {
  cat("Interpretation: Medium effect size\n")
} else {
  cat("Interpretation: Large effect size\n")
}

# ===================================================================
# 3. Effect Size for Categorical Data
# ===================================================================

# 3.1 Odds Ratio
# ------------

# Create a 2x2 contingency table
treatment <- c("Treatment", "Treatment", "Control", "Control")
outcome <- c("Success", "Failure", "Success", "Failure")
count <- c(40, 10, 20, 30)
contingency_table <- matrix(count, nrow = 2, 
                           dimnames = list(Group = c("Treatment", "Control"),
                                          Outcome = c("Success", "Failure")))
print(contingency_table)

# Calculate odds ratio
a <- contingency_table[1, 1]  # Treatment success
b <- contingency_table[1, 2]  # Treatment failure
c <- contingency_table[2, 1]  # Control success
d <- contingency_table[2, 2]  # Control failure

odds_ratio <- (a * d) / (b * c)
cat("\nOdds Ratio:", round(odds_ratio, 2), "\n")

# Calculate 95% confidence interval for odds ratio
log_odds_ratio <- log(odds_ratio)
se_log_odds_ratio <- sqrt(1/a + 1/b + 1/c + 1/d)
ci_lower <- exp(log_odds_ratio - 1.96 * se_log_odds_ratio)
ci_upper <- exp(log_odds_ratio + 1.96 * se_log_odds_ratio)

cat("95% Confidence Interval for Odds Ratio: [", 
    round(ci_lower, 2), ", ", round(ci_upper, 2), "]\n", sep = "")

# Interpretation of odds ratio
if (odds_ratio > 1) {
  cat("Interpretation: The odds of success are", round(odds_ratio, 2), 
      "times higher in the treatment group compared to the control group.\n")
} else if (odds_ratio < 1) {
  cat("Interpretation: The odds of success are", round(1/odds_ratio, 2), 
      "times lower in the treatment group compared to the control group.\n")
} else {
  cat("Interpretation: No difference in odds between groups.\n")
}

# 3.2 Risk Ratio (Relative Risk)
# ----------------------------

# Calculate risk ratio
risk_treatment <- a / (a + b)  # Proportion of success in treatment group
risk_control <- c / (c + d)    # Proportion of success in control group
risk_ratio <- risk_treatment / risk_control

cat("\nRisk in treatment group:", round(risk_treatment, 3), "\n")
cat("Risk in control group:", round(risk_control, 3), "\n")
cat("Risk Ratio:", round(risk_ratio, 2), "\n")

# Calculate 95% confidence interval for risk ratio
log_risk_ratio <- log(risk_ratio)
se_log_risk_ratio <- sqrt((b/(a*(a+b))) + (d/(c*(c+d))))
ci_lower_rr <- exp(log_risk_ratio - 1.96 * se_log_risk_ratio)
ci_upper_rr <- exp(log_risk_ratio + 1.96 * se_log_risk_ratio)

cat("95% Confidence Interval for Risk Ratio: [", 
    round(ci_lower_rr, 2), ", ", round(ci_upper_rr, 2), "]\n", sep = "")

# Interpretation of risk ratio
if (risk_ratio > 1) {
  cat("Interpretation: The risk of success is", round(risk_ratio, 2), 
      "times higher in the treatment group compared to the control group.\n")
} else if (risk_ratio < 1) {
  cat("Interpretation: The risk of success is", round(1/risk_ratio, 2), 
      "times lower in the treatment group compared to the control group.\n")
} else {
  cat("Interpretation: No difference in risk between groups.\n")
}

# 3.3 Risk Difference (Absolute Risk Reduction)
# ------------------------------------------

# Calculate risk difference
risk_difference <- risk_treatment - risk_control
cat("\nRisk Difference:", round(risk_difference, 3), "\n")

# Calculate 95% confidence interval for risk difference
se_risk_diff <- sqrt((risk_treatment * (1 - risk_treatment) / (a + b)) + 
                     (risk_control * (1 - risk_control) / (c + d)))
ci_lower_rd <- risk_difference - 1.96 * se_risk_diff
ci_upper_rd <- risk_difference + 1.96 * se_risk_diff

cat("95% Confidence Interval for Risk Difference: [", 
    round(ci_lower_rd, 3), ", ", round(ci_upper_rd, 3), "]\n", sep = "")

# Calculate Number Needed to Treat (NNT)
if (risk_difference > 0) {
  nnt <- 1 / risk_difference
  cat("Number Needed to Treat (NNT):", ceiling(nnt), "\n")
  cat("Interpretation: Approximately", ceiling(nnt), 
      "patients need to be treated to prevent one additional bad outcome.\n")
} else if (risk_difference < 0) {
  nnt <- 1 / abs(risk_difference)
  cat("Number Needed to Harm (NNH):", ceiling(nnt), "\n")
  cat("Interpretation: Approximately", ceiling(nnt), 
      "patients need to be treated to cause one additional bad outcome.\n")
} else {
  cat("No difference in risk between groups.\n")
}

# 3.4 Phi Coefficient and Cramer's V
# --------------------------------

# For 2x2 contingency table
chi_sq_test <- chisq.test(contingency_table)
chi_sq <- chi_sq_test$statistic
n <- sum(contingency_table)

# Calculate phi coefficient (for 2x2 tables)
phi_coef <- sqrt(chi_sq / n)
cat("\nPhi Coefficient:", round(phi_coef, 3), "\n")

# Interpretation of phi coefficient
if (phi_coef < 0.1) {
  cat("Interpretation: Negligible association\n")
} else if (phi_coef < 0.3) {
  cat("Interpretation: Weak association\n")
} else if (phi_coef < 0.5) {
  cat("Interpretation: Moderate association\n")
} else {
  cat("Interpretation: Strong association\n")
}

# Create a larger contingency table for Cramer's V
treatment_larger <- c("Drug A", "Drug A", "Drug A", "Drug B", "Drug B", "Drug B", "Placebo", "Placebo", "Placebo")
outcome_larger <- c("Improved", "No Change", "Worsened", "Improved", "No Change", "Worsened", "Improved", "No Change", "Worsened")
count_larger <- c(45, 30, 25, 60, 25, 15, 30, 40, 30)
contingency_larger <- matrix(count_larger, nrow = 3, 
                            dimnames = list(Treatment = c("Drug A", "Drug B", "Placebo"),
                                           Outcome = c("Improved", "No Change", "Worsened")))
print(contingency_larger)

# Calculate Cramer's V
chi_sq_larger <- chisq.test(contingency_larger)$statistic
n_larger <- sum(contingency_larger)
r <- nrow(contingency_larger)
c <- ncol(contingency_larger)
min_dim <- min(r - 1, c - 1)

cramers_v <- sqrt(chi_sq_larger / (n_larger * min_dim))
cat("\nCramer's V:", round(cramers_v, 3), "\n")

# Interpretation of Cramer's V
if (cramers_v < 0.1) {
  cat("Interpretation: Negligible association\n")
} else if (cramers_v < 0.3) {
  cat("Interpretation: Weak association\n")
} else if (cramers_v < 0.5) {
  cat("Interpretation: Moderate association\n")
} else {
  cat("Interpretation: Strong association\n")
}

# ===================================================================
# 4. Effect Size for Correlation
# ===================================================================

# 4.1 Pearson's r
# -------------

# Generate correlated data
set.seed(202)
x <- rnorm(50, mean = 0, sd = 1)
y <- 0.6 * x + rnorm(50, mean = 0, sd = 0.8)  # Moderate correlation

# Calculate Pearson's correlation coefficient
cor_result <- cor.test(x, y)
r <- cor_result$estimate
cat("\nPearson's r:", round(r, 3), "\n")
cat("95% Confidence Interval: [", 
    round(cor_result$conf.int[1], 3), ", ", 
    round(cor_result$conf.int[2], 3), "]\n", sep = "")

# Interpretation of Pearson's r
if (abs(r) < 0.1) {
  cat("Interpretation: Negligible correlation\n")
} else if (abs(r) < 0.3) {
  cat("Interpretation: Weak correlation\n")
} else if (abs(r) < 0.5) {
  cat("Interpretation: Moderate correlation\n")
} else if (abs(r) < 0.7) {
  cat("Interpretation: Strong correlation\n")
} else {
  cat("Interpretation: Very strong correlation\n")
}

# Visualize the correlation
plot(x, y, pch = 19, col = "blue",
     main = paste("Correlation: r =", round(r, 2)),
     xlab = "X Variable", ylab = "Y Variable")
abline(lm(y ~ x), col = "red", lwd = 2)

# 4.2 Coefficient of Determination (r²)
# -----------------------------------

# Calculate r-squared
r_squared <- r^2
cat("\nCoefficient of Determination (r²):", round(r_squared, 3), "\n")
cat("Interpretation:", round(r_squared * 100, 1), 
    "% of the variance in Y is explained by X.\n")

# 4.3 Spearman's Rank Correlation
# -----------------------------

# Generate data with non-linear relationship
set.seed(303)
x_nonlinear <- rnorm(50, mean = 0, sd = 1)
y_nonlinear <- x_nonlinear^2 + rnorm(50, mean = 0, sd = 0.5)

# Calculate Pearson's and Spearman's correlations
pearson_nonlinear <- cor(x_nonlinear, y_nonlinear, method = "pearson")
spearman_nonlinear <- cor(x_nonlinear, y_nonlinear, method = "spearman")

cat("\nNon-linear Relationship:\n")
cat("Pearson's r:", round(pearson_nonlinear, 3), "\n")
cat("Spearman's rho:", round(spearman_nonlinear, 3), "\n")

# Visualize the non-linear relationship
plot(x_nonlinear, y_nonlinear, pch = 19, col = "purple",
     main = paste("Non-linear Relationship\nPearson's r =", round(pearson_nonlinear, 2),
                 ", Spearman's rho =", round(spearman_nonlinear, 2)),
     xlab = "X Variable", ylab = "Y Variable")
curve(x^2, add = TRUE, col = "red", lwd = 2)

# ===================================================================
# 5. Effect Size for Regression
# ===================================================================

# 5.1 R-squared and Adjusted R-squared
# ----------------------------------

# Generate data for multiple regression
set.seed(404)
n <- 100
x1 <- rnorm(n, mean = 0, sd = 1)
x2 <- rnorm(n, mean = 0, sd = 1)
x3 <- rnorm(n, mean = 0, sd = 1)
y <- 2 + 0.5*x1 + 0.3*x2 + 0.1*x3 + rnorm(n, mean = 0, sd = 1)

# Create data frame
data_reg <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

#
(Content truncated due to size limit. Use line ranges to read in chunks)