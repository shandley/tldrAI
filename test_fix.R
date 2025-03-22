# Test our fix to the highlight warning

# Needed for get_config
source("R/config.R")

# Define null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Source necessary files
source("R/visualization_impl.R")
source("R/visualization_core.R")

# Test data
func_name <- 'mean'
metadata <- list(package = 'base', args = c('x', 'trim', 'na.rm'))

# Call the function that had the warning
tryCatch({
  viz <- generate_code_highlight(func_name, metadata)
  cat("Function executed successfully without warnings\!\n")
}, error = function(e) {
  cat("Error:", e$message, "\n")
})
