# -------------------------------------------------------- Code Information ----
# Project:              Project 3 - Limited Dependent Variable Models
# 
# Author:               Patrick Cataldo
# Date:                 12/18/2023
# Last Modified:        11/9/2024
#
# Purpose:              The purpose of this code is to analyze the loan dataset
#                       and extract insights, as well as create both linear
#                       and probit probability models to estimate the denial of 
#                       a loan.
#
# ----------------------------------------------------------------- Outline ----
# Part 1:               Summary of Munnell                                   N/A
# Part 2:               Analyze the data                                Line  35
# Part 3:               Estimate a Linear Probability Model             Line 102
# Part 4:               Review Lim and Ky Paper                              N/A
# Part 5:               Estimate the Probit Model                       Line 117
# Part 6:               Marginal Effects                                Line 132
# Part 7:               Deliverable Tables and Graphs                   Line 261
#
# ---------------------------------------------- Install necessary packages ----
# Install packages if not already installed
# install.packages("stargazer")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("gridExtra")

# Load Libraries
library(stargazer)
library(dplyr)
library(ggplot2)
library(gridExtra)

# ------------------------------------------------ Part 2: Analyze the data ----

# Load in data file
hdmaFile <- "C:/Users/School Account/Desktop/JMU Classes/2023-24/Fall Semester 2023/ECON485 - Advanced Econometrics/Projects/Project 3 Overhaul/hmda_subset_F23.csv"
myData <- read.csv(hdmaFile)

# Percentage of apps denied
totalDenied <- sum(myData$denied == 1)
percentDenied <- (totalDenied / nrow(myData)) * 100

# Percent of apps accepted
totalAccepted <- sum(myData$denied == 0)
percentAccepted <- (totalAccepted / nrow(myData)) * 100

# Part 2b: Separate data by RACE
# Minority applicants (race == 1)
minorityData <- subset(myData, race == 1)
minorityTotalDenied <- sum(minorityData$denied == 1)
minorityPercentDenied <- minorityTotalDenied / nrow(minorityData) * 100
minorityTotalAccepted <- sum(minorityData$denied == 0)
minorityPercentAccepted <- minorityTotalAccepted / nrow(minorityData) * 100

# White applicants (race == 0)
whiteData <- subset(myData, race == 0)
whiteTotalDenied <- sum(whiteData$denied == 1)
whitePercentDenied <- whiteTotalDenied / nrow(whiteData) * 100
whiteTotalAccepted <- sum(whiteData$denied == 0)
whitePercentAccepted <- whiteTotalAccepted / nrow(whiteData) * 100

# Create a table of applications denied and accepted by race
denied_by_race <- data.frame(
  Race = c("White", "Minority"),
  Denied = c(whiteTotalDenied, minorityTotalDenied),
  Accepted = c(whiteTotalAccepted, minorityTotalAccepted),
  Percent_Denied = c(whitePercentDenied, minorityPercentDenied),
  Percent_Accepted = c(whitePercentAccepted, minorityPercentAccepted)
)

# Part 2c: Separate data by MALE
# Male applicants (male == 1)
maleData <- subset(myData, male == 1)
maleTotalDenied <- sum(maleData$denied == 1)
malePercentDenied <- maleTotalDenied / nrow(maleData) * 100
maleTotalAccepted <- sum(maleData$denied == 0)
malePercentAccepted <- maleTotalAccepted / nrow(maleData) * 100

# Female applicants (male == 0)
femaleData <- subset(myData, male == 0)
femaleTotalDenied <- sum(femaleData$denied == 1)
femalePercentDenied <- femaleTotalDenied / nrow(femaleData) * 100
femaleTotalAccepted <- sum(femaleData$denied == 0)
femalePercentAccepted <- femaleTotalAccepted / nrow(femaleData) * 100

# Create a table of applications denied and accepted by gender
denied_by_gender <- data.frame(
  Gender = c("Male", "Female"),
  Denied = c(maleTotalDenied, femaleTotalDenied),
  Accepted = c(maleTotalAccepted, femaleTotalAccepted),
  Percent_Denied = c(malePercentDenied, femalePercentDenied),
  Percent_Accepted = c(malePercentAccepted, femalePercentAccepted)
)

# Get descriptive statistics to view sample averages
descriptiveStatsTable <- stargazer(myData, type = "html", 
                                   title = "Descriptive Statistics", 
                                   summary = TRUE, digits = 2)

# ----------------------------- Part 3: Estimate a Linear Probability Model ----
olsModel <- lm(denied ~ pi + race + ccs + lv + pbcr + uria + se + condo + sfam + 
                 male + iraj, data = myData)

# Add predicted values to the dataset
myData$denied_LPM_hat <- fitted(olsModel)

# Identify the individuals with the lowest and highest predicted probabilities
min_prob <- min(myData$denied_LPM_hat)
max_prob <- max(myData$denied_LPM_hat)

# Get the observations with min and max predicted probabilities
min_prob_obs <- myData[which.min(myData$denied_LPM_hat), ]
max_prob_obs <- myData[which.max(myData$denied_LPM_hat), ]

# --------------------------------------- Part 5: Estimate the Probit Model ----
probitModel <- glm(denied ~ pi + race + ccs + lv + pbcr + uria + se + condo + 
                     sfam + male + iraj, family = binomial(link = "probit"), 
                   data = myData)

# Add predicted probabilities to the dataset
myData$denied_Probit_hat <- predict(probitModel, type = "response")

# Construct Denied_hat values based on a cutoff of 0.5
estimatedDenied <- ifelse(myData$denied_Probit_hat > 0.5, 1, 0)
myData$denied_HAT <- estimatedDenied

# Measure accuracy of the model
accuracy <- mean(myData$denied == myData$denied_HAT) * 100

# ------------------------------------------------ Part 6: Marginal Effects ----

## A: Continuous X Variables

# Calculate the linear predictor (z) for each observation
linearPredictor <- predict(probitModel, type = "link")

# Compute the standard normal density function phi(z) for each observation
phi_z <- dnorm(linearPredictor)

# Extract coefficients from the probit model
coefficients <- coef(probitModel)

# a.1 Marginal Effect of a 10 Percentage Point Increase in PI
delta_PI <- 10
marginalEffects_PI <- phi_z * coefficients["pi"] * delta_PI
averageMarginalEffect_PI <- mean(marginalEffects_PI)

# a.2 Effect of One More "Slow Pay" on Credit Account (CCS)
delta_CCS <- 1
marginalEffects_CCS <- phi_z * coefficients["ccs"] * delta_CCS
averageMarginalEffect_CCS <- mean(marginalEffects_CCS)

# a.3 Marginal Effect of a 10 Percentage Point Increase in Loan-to-Value Ratio
delta_LV <- 10
marginalEffects_LV <- phi_z * coefficients["lv"] * delta_LV
averageMarginalEffect_LV <- mean(marginalEffects_LV)

## B: Dichotomous X Variables (Dummy Variables)

# Calculate the mean of each variable (excluding the dependent variable)
meanValues <- myData %>%
  select(pi, race, ccs, lv, pbcr, uria, se, condo, sfam, male, iraj) %>%
  summarise_all(mean)
meanValues <- as.list(meanValues)

# b.1 Marginal Effect for RACE
z_race1 <- coefficients["(Intercept)"] +
  coefficients["race"] * 1 +
  coefficients["pi"]   * meanValues$pi +
  coefficients["ccs"]  * meanValues$ccs +
  coefficients["lv"]   * meanValues$lv +
  coefficients["pbcr"] * meanValues$pbcr +
  coefficients["uria"] * meanValues$uria +
  coefficients["se"]   * meanValues$se +
  coefficients["condo"]* meanValues$condo +
  coefficients["sfam"] * meanValues$sfam +
  coefficients["male"] * meanValues$male +
  coefficients["iraj"] * meanValues$iraj

z_race0 <- coefficients["(Intercept)"] +
  coefficients["race"] * 0 +
  coefficients["pi"]   * meanValues$pi +
  coefficients["ccs"]  * meanValues$ccs +
  coefficients["lv"]   * meanValues$lv +
  coefficients["pbcr"] * meanValues$pbcr +
  coefficients["uria"] * meanValues$uria +
  coefficients["se"]   * meanValues$se +
  coefficients["condo"]* meanValues$condo +
  coefficients["sfam"] * meanValues$sfam +
  coefficients["male"] * meanValues$male +
  coefficients["iraj"] * meanValues$iraj

P_race1 <- pnorm(z_race1)
P_race0 <- pnorm(z_race0)
marginalEffect_RACE <- P_race1 - P_race0

# b.2 Marginal Effect for IRAJ
z_iraj1 <- coefficients["(Intercept)"] +
  coefficients["iraj"] * 1 +
  coefficients["pi"]   * meanValues$pi +
  coefficients["race"] * meanValues$race +
  coefficients["ccs"]  * meanValues$ccs +
  coefficients["lv"]   * meanValues$lv +
  coefficients["pbcr"] * meanValues$pbcr +
  coefficients["uria"] * meanValues$uria +
  coefficients["se"]   * meanValues$se +
  coefficients["condo"]* meanValues$condo +
  coefficients["sfam"] * meanValues$sfam +
  coefficients["male"] * meanValues$male

z_iraj0 <- coefficients["(Intercept)"] +
  coefficients["iraj"] * 0 +
  coefficients["pi"]   * meanValues$pi +
  coefficients["race"] * meanValues$race +
  coefficients["ccs"]  * meanValues$ccs +
  coefficients["lv"]   * meanValues$lv +
  coefficients["pbcr"] * meanValues$pbcr +
  coefficients["uria"] * meanValues$uria +
  coefficients["se"]   * meanValues$se +
  coefficients["condo"]* meanValues$condo +
  coefficients["sfam"] * meanValues$sfam +
  coefficients["male"] * meanValues$male

P_iraj1 <- pnorm(z_iraj1)
P_iraj0 <- pnorm(z_iraj0)
marginalEffect_IRAJ <- P_iraj1 - P_iraj0

# b.3 Marginal Effect for SE
z_se1 <- coefficients["(Intercept)"] +
  coefficients["se"]   * 1 +
  coefficients["pi"]   * meanValues$pi +
  coefficients["race"] * meanValues$race +
  coefficients["ccs"]  * meanValues$ccs +
  coefficients["lv"]   * meanValues$lv +
  coefficients["pbcr"] * meanValues$pbcr +
  coefficients["uria"] * meanValues$uria +
  coefficients["condo"]* meanValues$condo +
  coefficients["sfam"] * meanValues$sfam +
  coefficients["male"] * meanValues$male +
  coefficients["iraj"] * meanValues$iraj

z_se0 <- coefficients["(Intercept)"] +
  coefficients["se"]   * 0 +
  coefficients["pi"]   * meanValues$pi +
  coefficients["race"] * meanValues$race +
  coefficients["ccs"]  * meanValues$ccs +
  coefficients["lv"]   * meanValues$lv +
  coefficients["pbcr"] * meanValues$pbcr +
  coefficients["uria"] * meanValues$uria +
  coefficients["condo"]* meanValues$condo +
  coefficients["sfam"] * meanValues$sfam +
  coefficients["male"] * meanValues$male +
  coefficients["iraj"] * meanValues$iraj

P_se1 <- pnorm(z_se1)
P_se0 <- pnorm(z_se0)
marginalEffect_SE <- P_se1 - P_se0

# ------------------------------------------------------ Deliverable Tables ----

# Set the output HTML file path to your specified directory
htmlFile <- "C:/Users/School Account/Desktop/JMU Classes/2023-24/Fall Semester 2023/ECON485 - Advanced Econometrics/Projects/Project 3 Overhaul/Project3_Outputs.html"

# Create the HTML file and write outputs
sink(htmlFile, type = "output", split = TRUE)

cat("<html><head><title>Project 3 Outputs</title></head><body>")
cat("<h1>Project 3 - Limited Dependent Variable Models</h1>")

# Part 2 Outputs
cat("<h2>Part 2: Analyze the Data</h2>")

cat("<h3>Overall Loan Applications</h3>")
cat(sprintf("<p>Total Applications Denied: %d (%.2f%%)</p>", totalDenied, 
            percentDenied))
cat(sprintf("<p>Total Applications Accepted: %d (%.2f%%)</p>", totalAccepted, 
            percentAccepted))

cat("<h3>Applications Denied and Accepted by Race</h3>")
stargazer(denied_by_race, type = "html", summary = FALSE, rownames = FALSE)

cat("<h3>Applications Denied and Accepted by Gender</h3>")
stargazer(denied_by_gender, type = "html", summary = FALSE, rownames = FALSE)

cat("<h3>Descriptive Statistics</h3>")
descriptiveStatsTable

# Part 3 Outputs
cat("<h2>Part 3: Estimate a Linear Probability Model</h2>")
stargazer(olsModel, type = "html",
          title = "Linear Probability Model Results",
          dep.var.labels = "Denied",
          covariate.labels = c("B0", "Debt-to-Income Ratio", "Race", 
                               "Credit History", "Loan-to-Value Ratio", 
                               "Credit Status", 
                               "Unemployment Probability", "Self Employed", 
                               "Condo Unit", "Single Family Unit", 
                               "Gender", "Adjustable Rate"),
          intercept.bottom = FALSE, intercept.top = TRUE,
          digits = 3)

cat("<h3>Individuals with Extreme Predicted Probabilities</h3>")
cat("<p><strong>Lowest Predicted Probability:</strong></p>")
stargazer(min_prob_obs, type = "html", summary = FALSE, rownames = FALSE)
cat(sprintf("<p>Predicted Probability: %.4f</p>", min_prob))

cat("<p><strong>Highest Predicted Probability:</strong></p>")
stargazer(max_prob_obs, type = "html", summary = FALSE, rownames = FALSE)
cat(sprintf("<p>Predicted Probability: %.4f</p>", max_prob))

# Part 5 Outputs
cat("<h2>Part 5: Estimate the Probit Model</h2>")
stargazer(olsModel, probitModel, type = "html",
          title = "Regression Results - OLS and Probit Models",
          dep.var.labels = c("Denied (OLS)", "Denied (Probit)"),
          covariate.labels = c("B0", "Debt-to-Income Ratio", "Race", 
                               "Credit History", "Loan-to-Value Ratio", 
                               "Credit Status", 
                               "Unemployment Probability", "Self Employed", 
                               "Condo Unit", "Single Family Unit", 
                               "Gender", "Adjustable Rate"),
          model.names = FALSE, intercept.bottom = FALSE, intercept.top = TRUE,
          digits = 3)

cat(sprintf("<p>Model Accuracy: %.2f%%</p>", accuracy))

# Part 6 Outputs
cat("<h2>Part 6: Marginal Effects</h2>")

# Output Marginal Effects for Continuous Variables
cat("<h3>A: Continuous Variables</h3>")
cat("<table border='1'>")
cat("<tr><th>Variable</th><th>Marginal Effect</th></tr>")
cat(sprintf("<tr><td>PI (10 pt increase)</td><td>%.4f</td></tr>", 
            averageMarginalEffect_PI))
cat(sprintf("<tr><td>CCS (+1 slow pay)</td><td>%.4f</td></tr>", 
            averageMarginalEffect_CCS))
cat(sprintf("<tr><td>LV (10 pt increase)</td><td>%.4f</td></tr>", 
            averageMarginalEffect_LV))
cat("</table>")

# Output Marginal Effects for Dummy Variables
cat("<h3>B: Dummy Variables</h3>")
cat("<table border='1'>")
cat("<tr><th>Variable</th><th>Marginal Effect</th></tr>")
cat(sprintf("<tr><td>Race (Minority to White)</td><td>%.4f</td></tr>", 
            marginalEffect_RACE))
cat(sprintf("<tr><td>Adjustable Rate Mortgage (0 to 1)</td><td>%.4f</td></tr>", 
            marginalEffect_IRAJ))
cat(sprintf("<tr><td>Self-Employed (0 to 1)</td><td>%.4f</td></tr>", 
            marginalEffect_SE))
cat("</table>")

cat("</body></html>")

# Close the HTML file
sink()

# ----------------------------------------------------------- End of Script ----