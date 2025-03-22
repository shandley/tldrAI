# Debug script to identify source of warning
library(highlight)

# Override sprintf to capture the warning
orig_sprintf <- base::sprintf
debug_sprintf <- function(...) {
  tryCatch({
    result <- orig_sprintf(...)
    return(result)
  }, warning = function(w) {
    print("WARNING CAUGHT in sprintf:")
    print(list(...))
    cat("Warning message:", w$message, "\n")
    result <- orig_sprintf(...)
    return(result)
  })
}

# Install debug function
assignInNamespace("sprintf", debug_sprintf, ns = "base")

# Test highlight function with a simple file
tempfile <- tempfile(fileext = ".R")
cat("function(x) { return(x + 1) }", file = tempfile)
result <- highlight::highlight(file = tempfile, renderer = highlight::renderer_html())

# Cleanup
unlink(tempfile)
