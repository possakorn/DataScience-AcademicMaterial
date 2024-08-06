# load libraries 
pacman::p_load(
  tidyverse, 
  tidymodels,
  discrim,
  matlib
  )

# function
get_LDA2 <- function(x1, x2, y, new1, new2) { 
  # YOUR CODE HERE
  
  ## 02 Correct inputs
  
  # check that x1 is numeric vector
  if (!is.numeric(x1)) {
    stop("x1 must be numeric.")
  }
  
  # check that x2 is numeric vector
  if (!is.numeric(x2)) {
    stop("x2 must be numeric.")
  }
  
  # check that new1 is numeric vector
  if (!is.numeric(new1)) {
    stop("new1 must be numeric.")
  }
  
  # check that new2 is numeric vector
  if (!is.numeric(new2)) {
    stop("new2 must be numeric.")
  }
  
  # check that x1, x2 and y are the same length
  if (length(x1) != length(x2) | length(x1) != length(y)) { 
    stop("x1, x2, and y must be the same length.")
  }
  
  # check that new1 and new2 are the same length
  if (length(new1) != length(new2)) { 
    stop("new1 and new2 must be the same length.")
  }
  
  ## 03 Edge cases
  
  # check data frame for checking
 
  df_prep <- tibble(y = as.factor(y)) %>% 
    group_by(y) %>% 
    summarise(num = n())
  
  # Check: y has at least 2 levels;
  if (nrow(df_prep) < 2) {
    stop("y must be at least 2 levels.")
  }
  
  # Check: each level has at least 2 observations; 
  if (nrow(df_prep %>% filter(num < 2)) != 0) {
    stop("each level of y has at least 2 observations")
  }
  
  # Check: each level has non-zero variance for x1 and x2
  if (var(x1) == 0 | var(x2) == 0) { 
    stop("each level has non-zero variance for x1 and x2")
  }
  
  ## 04 Function implementation
  
  # Step 0: Initial the input matrix for training and testing sets
  train_vec <- cbind(x1, x2) # training set
  test_vec <- cbind(new1, new2) # testing set
  
  # Step 1: Get number of observations (N) and classes (K)
  N <- nrow(train_vec)
  K <- length(unique(y))


  # Step 2: Initialize the matrix
  mu_vec <- matrix(NA, nrow = K, ncol = 2) # mean vector ( hard code 2 for x1 and x2 )
  pi_vec <- numeric(K) # pi vector
  pool_cov_vec <- matrix(0, nrow = 2, ncol = 2) # Covariance matrix for each class ( hard code 2 for x1 and x2 )
  
  # Step 3: Compute means, priors, and covariance matrices for each class
  for (k in 1:K) {
    x_sub <- train_vec[y == unique(y)[k], ]
    mu_vec[k, ] <- colMeans(x_sub)
    pi_vec[k] <- nrow(x_sub) / N
    cov_vec <- cov(x_sub)
  }
  
  # Step 4: Compute pooled covariance matrix
  for (k in 1:K) {
    pool_cov_vec <- pool_cov_vec + (nrow(x_sub) - 1) * cov_vec  # Add weighted covariance matrix to pooled_cov
  }
  pool_cov_vec <- pool_cov_vec / (N - K)  # Divide by (N - K) to finalize pooled covariance matrix
  pool_cov_vec.inv <- inv(pool_cov_vec)
  
  
  # Step 5: Compute discriminant scores for new data points
  delta <- matrix(NA, nrow = nrow(test_vec), ncol = K)  # Initialize matrix for discriminant scores
  
  for (i in 1:nrow(test_vec)) {
    for (k in 1:K) {
      delta[i, k] <- 
        t(as.matrix(test_vec[i, ])) %*% pool_cov_vec.inv %*% mu_vec[k, ] - 
        0.5 * t(mu_vec[k, ]) %*% pool_cov_vec.inv %*% mu_vec[k, ] + 
        log(pi_vec[k])
      # print(paste("i ", i))
      # print(paste("k ", k))
      # print(delta)
    }
  }
  
  # Step 6 Return the class with the highest discriminant score for each new data point
  output <- unique(y)[apply(delta, 1, which.max)]
  return(output)
}