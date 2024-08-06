get_ROC <- function(obs, pred){
  # YOUR CODE HERE
  ### Check Correct inputs
  if( !(is.numeric(pred) ) ){
    return("input vectors aren't numeric")
  }
  if( !(length(obs) == length(pred)) ){
    return("input vectors aren't same length")
  }
  ### Check Correct input values
  for(i in seq_along(obs)){    
    if( !(obs[i] %in% c(0,1)) ){
      return("values aren't within obs conform to specifications")
    }
  }
  for(i in seq_along(pred)){    
    if(!between(pred[i], 0, 1)){
      return("values aren't within pred conform to specifications")
    }
  }
  
  # Setting the thresholds step following predicted value
  thresholds <- sort(unique(pred))
  
  # Initialize vectors to store sensitivity and specificity
  sens <- rep(0, length(thresholds))
  spec <- rep(0, length(thresholds))
  
  # Initialize the for loop to create the sensitivity and specificity for all thresholds points
  for(i in seq_along(thresholds)){
    threshold <- thresholds[i]
    
    # Apply criteria to the prediction based on the threshold
    pred_binary <- ifelse(pred >= threshold, 1, 0)
    
    # Calculate Confusion Matrix: True Positives, True Negatives, False Positives and False Negatives
    TP <- sum(pred_binary == 1 & obs == 1)
    TN <- sum(pred_binary == 0 & obs == 0)
    FP <- sum(pred_binary == 1 & obs == 0)
    FN <- sum(pred_binary == 0 & obs == 1)
    
    # Calculate specificity and sensitivity 
    spec[i] <- TN / (TN + FP)
    sens[i] <- TP / (TP + FN)
  }
  
  # Generate A tibble with 3 columns: threshold, specificity, and sensitivity
  output <- tibble(threshold = thresholds,
                   specificity = spec,
                   sensitivity = sens
  )
  return(output)
}