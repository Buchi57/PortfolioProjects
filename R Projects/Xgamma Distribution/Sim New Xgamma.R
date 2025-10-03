set.seed(1234)

# Function to simulate from g(x; Phi) using the acceptance rejection algorithm
sim_g <- function(n, theta){
  x <- rexp(n) # Simulate from exponential distribution
  k <- theta^2/(1 + theta)
  y <- runif(n, 0, k) 
  accept <- y < (theta^2/((1+theta))*(1 + (theta^2*x^3)/6)*exp(-theta*x))
  x[accept] 
}

# Parameters
n_vals <- c(50, 100, 150, 200)
theta_vals <- c(0.5, 1, 1.5, 2)
N <- 10000  

# Run simulation and calculate bias and MSE for each scenario
for(theta in theta_vals){
  for(n in n_vals){  
    bias <- rep(0, N)
    for(i in 1:N){
      sim <- sim_g(n, theta)
      mle <- mean(sim)
      bias[i] <- mle - theta 
    }    
    mse <- mean(bias^2)
    print(c(n, theta, mean(bias), mse))
 	
  }
}

