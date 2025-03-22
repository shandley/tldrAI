# Load all necessary files
source("R/character.R")
source("R/output.R")

# Sample API response with examples
api_response <- '```r
# glm

## Purpose
Fits generalized linear models for regression and classification tasks.

## Usage
glm(formula, data, family = gaussian)

## Key Arguments
- formula: Model formula specifying predictors and response variable
- data: Data frame containing the variables 
- family: Error distribution and link function (e.g., gaussian, binomial, poisson)

## Examples
```r
# Logistic regression for binary outcome
glm(am ~ hp + mpg, data = mtcars, family = binomial)

# Poisson regression for count data
glm(counts ~ treatment, data = poissonDF, family = poisson)
```
```'

# Apply character voice transformation
cat("Testing with a realistic API response...\n\n")

transformed <- apply_character_voice(api_response, "time_traveler")
cat("Transformed with Time Traveler voice:\n")
cat(transformed, "\n\n")

# Extract the content and format it like the tldr output
cat("Extracting and formatting content (simulating output display):\n")
content <- extract_code_block(transformed)
cat(content, "\n\n")

# Check if examples are preserved in the extracted content
cat("Checking if Examples section is correctly preserved:\n")
if (grepl("## Examples", content) && 
    grepl("# Logistic regression", content) && 
    grepl("glm\\(am ~ hp", content)) {
  cat("Examples section and code are correctly preserved in the extracted content.\n")
} else {
  cat("Problem with Examples section in extracted content.\n")
}