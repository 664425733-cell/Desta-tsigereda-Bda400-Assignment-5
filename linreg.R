linreg <- function(regressionSource, regressionLength, regressionOffset) {
  
  # Total number of elements
  n <- length(regressionSource)
  
  # Check if regressionLength is valid
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  
  # Check if regressionOffset is valid
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  
  # Calculate start and end indices
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  
  # Extract subset
  source_subset <- regressionSource[start_index:end_index]
  
  # Index values for regression points
  index_values <- 1:length(source_subset)
  
  # Sums
  sum_index <- sum(index_values)
  sum_source <- sum(source_subset)
  
  # Means
  mean_index <- mean(index_values)
  mean_source <- mean(source_subset)
  
  # Numerator and denominator
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  
  # Slope and intercept
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  
  # Predicted values
  predicted_values <- slope * index_values + intercept
  
  # Return result
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}
