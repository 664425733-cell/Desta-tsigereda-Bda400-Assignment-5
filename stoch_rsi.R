# Simple Moving Average function (needed for %K and %D)
sma <- function(x, n) {
  result <- rep(NA, length(x))
  for (i in n:length(x)) {
    result[i] <- mean(x[(i - n + 1):i])
  }
  return(result)
}

# Stochastic RSI (StochRSI)
stoch_rsi <- function(data, period, k_period, d_period) {
  
  # Step 1: Calculate RSI
  rsi_values <- rsi(data, period)
  
  # Step 2: Normalize RSI (StochRSI raw values)
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  
  # Step 3: %K line (SMA of k_values)
  k_line <- sma(k_values, k_period)
  
  # Step 4: %D line (SMA of %K)
  d_line <- sma(k_line, d_period)
  
  # Step 5: Return both lines
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
