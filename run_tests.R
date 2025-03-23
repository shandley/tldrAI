#!/usr/bin/env Rscript

# Load required libraries
library(testthat)

# Load the package (assuming it's in the current directory)
package_path <- getwd()
source_files <- list.files(file.path(package_path, "R"), full.names = TRUE, pattern = "\\.R$")
for (file in source_files) {
  source(file)
}

# Run the tests
test_file("tests/testthat/test-package.R")