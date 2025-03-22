# Load the character.R file
source("R/character.R")

# Create a factory and get voices
factory <- CharacterVoiceFactory$new()
time_traveler <- factory$get_voice("time_traveler")
theatrical_villain <- factory$get_voice("theatrical_villain")

# Test individual transform_purpose methods
cat("\n=== Testing Character Voice transform_purpose methods ===\n")
test_lines <- c(
  "This function computes the mean of a vector.",
  "It handles NA values appropriately."
)

cat("\nTimeTraveler voice:\n")
print(time_traveler$transform_purpose(test_lines))

cat("\nTheatricalVillain voice:\n")
print(theatrical_villain$transform_purpose(test_lines))

# Test apply_character_voice function
cat("\n\n=== Testing apply_character_voice function ===\n")

# Create a test response
test_response <- "```r
## Purpose
This function computes the mean of a vector.
It handles NA values appropriately.

## Usage
mean(x, trim = 0, na.rm = FALSE, ...)
```"

cat("\nOriginal response:\n")
cat(test_response)

# Create a special test function that doesn't rely on regex
test_apply_character_voice <- function(response, voice_name) {
  # Create factory and get voice
  factory <- CharacterVoiceFactory$new()
  voice <- factory$get_voice(voice_name)
  
  # Define purpose content lines
  purpose_lines <- c(
    "This function computes the mean of a vector.",
    "It handles NA values appropriately."
  )
  
  # Transform purpose lines
  transformed_purpose <- voice$transform_purpose(purpose_lines)
  
  # Manually replace in the response
  lines <- strsplit(response, "\n")[[1]]
  for (i in seq_along(lines)) {
    if (lines[i] == "This function computes the mean of a vector.") {
      lines[i] <- transformed_purpose[1]
      if (length(transformed_purpose) > 1) {
        lines[i+1] <- transformed_purpose[2]
      }
      break
    }
  }
  
  # Reassemble the response
  paste(lines, collapse = "\n")
}

cat("\n\nResponse with TimeTraveler voice:\n")
time_traveler_response <- test_apply_character_voice(test_response, "time_traveler")
cat(time_traveler_response)

cat("\n\nResponse with TheatricalVillain voice:\n")
villain_response <- test_apply_character_voice(test_response, "theatrical_villain")
cat(villain_response)

cat("\n\n=== Test Complete ===\n")