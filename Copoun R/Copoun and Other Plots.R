library(maxLik)

# Data
datas14 <- c(45,37,14,64,67,58,67,55,64,62,9,65,65,43,13,8,31,30,66,9,10,31,31,31,46,37,46,44,45,30,26,28,45,40,47,53,47,41,39,33,38,26,22,31,46,47,66,61,54,28,9,63,56,9,49,52,58,49,53,63,16,67,61,67,28,17,31,46,52,50,30,33,13,63,54,63,56,32,33,37,7,56,1,67,38,33,22,25,30,34,53,53,41,45,59,59,60,62,14,57,56,57,40,44,63)
datas14 <- na.omit(datas14)
n <- length(datas14)
x <- datas14

# Initial parameters
mean_x <- mean(datas14)
sd_x <- sd(datas14)
starts <- c(eta = 0.1 * sd_x, phi = mean_x / 10)

# Define all log-likelihood functions
llf_CpD <- function(param) {
  eta <- param[1]; phi <- param[2]
  2*n*log(eta) - n*log(phi + eta) + sum(log(1 + (phi*eta^2*x^3)/6)) - eta*sum(x)
}

llf_RD <- function(param) {
  eta <- param[1]; phi <- param[2]
  2*n*log(eta) - n*log(eta + 2*phi + 6) + sum(log(1 + phi*eta*x^2 + eta^2*x^3)) - eta*sum(x)
}

llf_TPLD <- function(param) {
  eta <- param[1]; phi <- param[2]
  2*n*log(eta) - n*log(eta*phi + 1) + sum(log(phi + x)) - eta*sum(x)
}

llf_TPAD <- function(param) {
  eta <- param[1]; phi <- param[2]
  3*n*log(eta) - n*log(phi*eta^2 + 2) + sum(log(phi + x^2)) - eta*sum(x)
}

llf_TPSD <- function(param) {
  eta <- param[1]; phi <- param[2]
  3*n*log(eta) - n*log(eta^2 + phi*eta + 2) + sum(log(1 + phi*x + x^2)) - eta*sum(x)
}

llf_SD <- function(param) {
  eta <- param[1]; phi <- param[2]
  4*n*log(eta) - n*log(eta^4 + 6*phi) + sum(log(eta + phi*x^3)) - eta*sum(x)
}

# Perform MLE for all distributions
fit_dist <- function(llf, name) {
  tryCatch(
    expr = {
      ml <- maxLik(llf, start = starts, method = "BFGS")
      cat("\n---", name, "---\n")
      print(summary(ml))
      return(ml)
    },
    error = function(e) {
      cat("Error fitting", name, ":", e$message, "\n")
      return(NULL)
    }
  )
}

fits <- list(
  CpD = fit_dist(llf_CpD, "CpD"),
  RD = fit_dist(llf_RD, "RD"),
  TPLD = fit_dist(llf_TPLD, "TPLD"),
  TPAD = fit_dist(llf_TPAD, "TPAD"),
  TPSD = fit_dist(llf_TPSD, "TPSD"),
  SD = fit_dist(llf_SD, "SD")
)

# Define PDF functions
f_CpD <- function(x, eta, phi) (eta^2/(phi + eta)) * (1 + (phi*eta^2*x^3)/6) * exp(-eta*x)
f_RD <- function(x, eta, phi) (eta^2/(eta + 2*phi + 6)) * (1 + phi*eta*x^2 + eta^2*x^3) * exp(-eta*x)
f_TPLD <- function(x, eta, phi) (eta^2/(eta*phi + 1)) * (phi + x) * exp(-eta*x)
f_TPAD <- function(x, eta, phi) (eta^3/(phi*eta^2 + 2)) * (phi + x^2) * exp(-eta*x)
f_TPSD <- function(x, eta, phi) (eta^3/(eta^2 + phi*eta + 2)) * (1 + phi*x + x^2) * exp(-eta*x)
f_SD <- function(x, eta, phi) (eta^4/(eta^4 + 6*phi)) * (eta + phi*x^3) * exp(-eta*x)

# Plot histogram and curves
hist(datas14, freq = FALSE, breaks = 20, col = "lightblue",
     main = "Distribution Comparison", xlab = "x", ylim = c(0, 0.06))

colors <- c("red", "blue", "darkgreen", "purple", "orange", "brown")
dist_names <- names(fits)
xseq <- seq(0, max(datas14), length.out = 200)

for(i in seq_along(fits)) {
  if(!is.null(fits[[i]])) {
    params <- coef(fits[[i]])
    curve(get(paste0("f_", dist_names[i]))(x, params["eta"], params["phi"]),
          col = colors[i], lwd = 2, add = TRUE)
  }
}

legend("topright", legend = dist_names, col = colors, lwd = 2, cex = 0.8)