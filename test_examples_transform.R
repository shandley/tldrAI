# Load the necessary files
source("R/character.R")

# Create a test response with examples
test_response <- '```
# glm

## Purpose
Fits generalized linear models for regression and classification tasks.

## Usage
glm(formula, data, family)

## Key Arguments
- formula: Model formula specifying predictors and response variable
- data: Data frame containing the variables
- family: Error distribution and link function (e.g. gaussian, binomial, poisson)

## Examples
```r
# Logistic regression
glm(am ~ hp + mpg, data = mtcars, family = binomial)

# Poisson regression
glm(counts ~ treatment, data = poissonDF, family = poisson)
```
```'

# Transform with time traveler voice
factory <- CharacterVoiceFactory$new()
time_traveler <- factory$get_voice("time_traveler")

# Print original and transformed responses
cat("Original response:\n")
cat(test_response)

transformed <- apply_character_voice(test_response, "time_traveler")
cat("\n\nTransformed response:\n")
cat(transformed)

# Check if examples still exist
cat("\n\nChecking for Examples section:\n")
if (grepl("## Examples", transformed)) {
  cat("Examples section exists.\n")
  
  # Check if code blocks still exist
  if (grepl("```r", transformed)) {
    cat("R code blocks exist.\n")
  } else {
    cat("R code blocks missing.\n")
  }
} else {
  cat("Examples section missing.\n")
}