# Test visualizations

# Test data flow visualization
test_data_flow <- function() {
  func_name <- "filter"
  metadata <- list(
    package = "dplyr",
    args = c(".data", "...", ".by", ".preserve"),
    signature = "filter(.data, ..., .by = NULL, .preserve = FALSE)"
  )
  
  # Try to generate a visualization
  viz <- generate_data_flow_diagram(func_name, metadata)
  
  # Print result
  print(viz)
  
  # Return success
  return(TRUE)
}

# Test function network visualization
test_function_network <- function() {
  func_name <- "lm"
  metadata <- list(
    package = "stats",
    args = c("formula", "data", "subset", "weights", "na.action", "method", "model", "x", "y", "qr", "singular.ok", "contrasts", "offset", "..."),
    signature = "lm(formula, data, subset, weights, na.action, method = \"qr\", model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, contrasts = NULL, offset, ...)"
  )
  
  # Try to generate a visualization
  viz <- generate_function_network(func_name, metadata)
  
  # Print result
  print(viz)
  
  # Return success
  return(TRUE)
}

# Test code highlight visualization
test_code_highlight <- function() {
  func_name <- "lapply"
  metadata <- list(
    package = "base",
    args = c("X", "FUN", "..."),
    signature = "lapply(X, FUN, ...)"
  )
  
  # Try to generate a visualization
  viz <- generate_code_highlight(func_name, metadata)
  
  # Print result
  print(viz)
  
  # Return success
  return(TRUE)
}

# Run all tests if this file is sourced directly
if (interactive()) {
  cat("Testing data flow visualization...\n")
  test_data_flow()
  
  cat("\nTesting function network visualization...\n")
  test_function_network()
  
  cat("\nTesting code highlight visualization...\n")
  test_code_highlight()
}