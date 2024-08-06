# load libraries 
pacman::p_load(
  tidyverse
)

# function
get_kmeans <- function(x, k) { 
  # YOUR CODE HERE
  
  ## 02 Correct inputs
  
  # check that x is numeric vector
  if (!is.numeric(x)) {stop("x1 must be numeric.")}
  
  # check that k is number of groups - whole number
  if ( !is.numeric(k) || k %% 1 != 0) {stop("k must be number of groups.")}
  
  ## 03 Edge cases
  
  if (length(x) < k) {stop("x should be at least k observations")}
  
  if (k <= 1) {stop("k should have more than one group")}
  
  ## 04 Function implementation
  
  # step01: random location centriod
  set.seed(0)
  df <- tibble(
    x = x,
    cluster = factor(sample(1:k, length(x),replace = TRUE))
  )
 
  # step02-a: get centriod
  get_centroids <- function(df) { 
    df %>%
      group_by(cluster) %>% 
      summarise(
        x = mean(x)
        ) %>% 
      mutate(rank = rank(x),
             cluster = as.factor(rank)) %>% 
      select(-rank)
  }
  
  # step02-b: recalculate the cluster
  reassign_points <- function(df, centroids) {
    for (i in 1: length(df$x)){
      dist <- list()
      for (j in 1:k) {
        dist[[j]] <- (df$x[i] -  centroids$x[j])^2
        }
      df$cluster[i] <- centroids$cluster[which.min(unlist(dist))]
    }
    return(df)
  }
  
  # step03: loop 
  old_cluster <- NULL
  while (!identical(df$cluster, old_cluster)) { 
    old_cluster <- df$cluster 
    centroids <- get_centroids(df)
    df <- reassign_points(df, centroids)
  }
  # step04: prepare the output results
  output <- df$cluster %>% 
    as.numeric()
  
  return(output)
}
# x <- 10*rep(10:1, each = 3)
# get_kmeans(x = x, 3)
