##################
library(maxLik)

# Load data
datas14 <- c(45, 37, 14, 64, 67, 58, 67, 55, 64, 62, 9, 65, 65,43, 13, 8, 31, 30, 66, 9, 10, 31, 31, 31, 46, 37, 46, 44,45, 30, 26, 28, 45, 40, 47, 53, 47, 41, 39, 33, 38, 26, 22, 31, 46, 47, 66, 61, 54, 28, 9, 63, 56, 9, 49, 52, 58, 49, 53, 63, 16, 67, 61, 67, 28, 17, 31, 46, 52, 50, 30, 33, 13, 63, 54, 63, 56, 32, 33, 37, 7, 56, 1, 67, 38, 33, 22, 25, 30, 34, 53, 53, 41, 45, 59, 59, 60, 62, 14, 57, 56, 57, 40, 44, 63)

# Define MLE functions for each distribution
llf_CpD <- function(param) {
  eta <- param[1]
  phi <- param[2]
  term1 <- n * (2 * log(eta) - log(phi + eta))
  term2 <- sum(log(1 + (phi * eta^2 * datas14^3)/6))
  term3 <- -eta * sum(datas14)
  return(term1 + term2 + term3)
}

llf_RD <- function(param) {
  eta <- param[1]
  phi <- param[2]
  term1 <- n * (2 * log(eta) - log(eta + 2*phi + 6))
  term2 <- sum(log(1 + phi*eta*datas14^2 + eta^2*datas14^3))
  term3 <- -eta * sum(datas14)
  return(term1 + term2 + term3)
}
# TPLD (Equation 70)
llf_TPLD <- function(param) {
  eta <- param[1]
  phi <- param[2]
  term1 <- n * (2 * log(eta) - log(eta * phi + 1))
  term2 <- sum(log(phi + datas14))
  term3 <- -eta * sum(datas14)
  return(term1 + term2 + term3)
}

# TPAD (Equation 71)
llf_TPAD <- function(param) {
  eta <- param[1]
  phi <- param[2]
  term1 <- n * (3 * log(eta) - log(phi * eta^2 + 2))
  term2 <- sum(log(phi + datas14^2))
  term3 <- -eta * sum(datas14)
  return(term1 + term2 + term3)
}

# TPRD (Equation 72)
llf_TPRD <- function(param) {
  eta <- param[1]
  phi <- param[2]
  term1 <- n * (4 * log(eta) - log(phi * eta^3 + 6))
  term2 <- sum(log(phi + datas14^3))
  term3 <- -eta * sum(datas14)
  return(term1 + term2 + term3)
}

# TPSD (Equation 73)
llf_TPSD <- function(param) {
  eta <- param[1]
  phi <- param[2]
  term1 <- n * (3 * log(eta) - log(eta^2 + phi * eta + 2))
  term2 <- sum(log(1 + phi * datas14 + datas14^2))
  term3 <- -eta * sum(datas14)
  return(term1 + term2 + term3)
}

# SD (Equation 74)
llf_SD <- function(param) {
  eta <- param[1]
  phi <- param[2]
  term1 <- n * (4 * log(eta) - log(eta^4 + 6 * phi))
  term2 <- sum(log(eta + phi * datas14^3))
  term3 <- -eta * sum(datas14)
  return(term1 + term2 + term3)
}

# Fit all models (adjust starting values if needed)
start_vals <- c(eta = 0.1, phi = 0.1)
fits <- list(
  CpD = maxLik(llf_CpD, start = start_vals, method = "BFGS"),
  RD = maxLik(llf_RD, start = start_vals, method = "BFGS"),
  TPLD = maxLik(llf_TPLD, start = start_vals, method = "BFGS"),
  TPAD = maxLik(llf_TPAD, start = start_vals, method = "BFGS"),
  TPRD = maxLik(llf_TPRD, start = start_vals, method = "BFGS"),
  TPSD = maxLik(llf_TPSD, start = start_vals, method = "BFGS"),
  SD = maxLik(llf_SD, start = start_vals, method = "BFGS")
)


# Plot histogram
hist(datas14, freq = FALSE, breaks = "FD", col = "lightblue",
     main = "Histogram with Fitted Distributions", xlab = "Value", ylim = c(0, 0.04))

# Define PDFs using estimated parameters

pdf_CpD <- function(x, eta, phi) {
  (eta^2 / (phi + eta)) * (1 + (phi * eta^2 * x^3)/6) * exp(-eta * x)
}

pdf_RD <- function(x, eta, phi) {
  (eta^2 / (eta + 2*phi + 6)) * (1 + phi*eta*x^2 + eta^2*x^3) * exp(-eta * x)
}

pdf_TPLD <- function(x, eta, phi) {
  (eta^2 / (eta * phi + 1)) * (phi + x) * exp(-eta * x)
}

pdf_TPAD <- function(x, eta, phi) {
  (eta^3 / (phi * eta^2 + 2)) * (phi + x^2) * exp(-eta * x)
}

pdf_TPRD <- function(x, eta, phi) {
  (eta^4 / (phi * eta^3 + 6)) * (phi + x^3) * exp(-eta * x)
}

pdf_TPSD <- function(x, eta, phi) {
  (eta^3 / (eta^2 + phi * eta + 2)) * (1 + phi * x + x^2) * exp(-eta * x)
}

pdf_SD <- function(x, eta, phi) {
  (eta^4 / (eta^4 + 6 * phi)) * (eta + phi * x^3) * exp(-eta * x)
}

# Overlay all PDFs
colors <- c("red", "blue", "green", "purple", "orange", "violet", "darkred")
dist_names <- names(fits)

for (i in seq_along(fits)) {
  params <- coef(fits[[i]])
  curve(get(paste0("pdf_", dist_names[i]))(x, params["eta"], params["phi"]),
        from = 0, to = max(datas14), add = TRUE, col = colors[i], lwd = 2)
}

# Add legend
legend("topright", legend = dist_names, col = colors, lwd = 2, cex = 0.8)