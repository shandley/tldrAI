# Load all necessary files
source("R/character.R")

# Create test response with examples
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

# Apply character voice transformation
cat("Original response:\n")
cat(test_response, "\n\n")

# Test transform with time traveler voice
transformed_response <- apply_character_voice(test_response, "time_traveler")
cat("Transformed response (time_traveler):\n")
cat(transformed_response, "\n\n")

# Test transform with theatrical villain voice
transformed_response2 <- apply_character_voice(test_response, "theatrical_villain")
cat("Transformed response (theatrical_villain):\n")
cat(transformed_response2, "\n\n")

# Check if Examples section is preserved
cat("Checking if Examples section is preserved:\n")
if (grepl("## Examples", transformed_response) && 
    grepl("```r", transformed_response) && 
    grepl("glm\\(am ~ hp", transformed_response)) {
  cat("Examples section and code blocks are correctly preserved.\n")
} else {
  cat("Problem with Examples section preservation.\n")
}