# Ensure character.R is loaded
source(system.file("R/character.R", package = "tldrAI", mustWork = FALSE))
if (!exists("CharacterVoiceFactory")) {
  # Try alternative path for development environment
  source(file.path(getwd(), "R/character.R"))
}

test_that("apply_character_voice transforms only purpose section", {
  # Create a simple mock response with a Purpose section
  mock_response <- "```r
## Purpose
This function computes the mean of a vector.
It handles NA values appropriately.

## Usage
mean(x, trim = 0, na.rm = FALSE, ...)
```"

  # Test TimeTraveler voice
  transformed_time <- apply_character_voice(mock_response, "time_traveler")
  expect_true(grepl("## Purpose", transformed_time))
  expect_true(grepl("ancient|historical|primitive|future|quantum", transformed_time, ignore.case = TRUE))
  expect_false(grepl("MWAHAHAHA|TREMBLE|DIABOLICAL", transformed_time))
  
  # Usage section should remain unchanged
  expect_true(grepl("## Usage\nmean\\(x, trim = 0, na.rm = FALSE, \\.\\.\\.\\)", transformed_time))
  
  # Test TheatricalVillain voice
  transformed_villain <- apply_character_voice(mock_response, "theatrical_villain")
  expect_true(grepl("## Purpose", transformed_villain))
  expect_true(grepl("MWAHAHAHA|TREMBLE|DIABOLICAL|BEHOLD", transformed_villain, ignore.case = TRUE))
  expect_false(grepl("ancient|historical|primitive|future", transformed_villain, ignore.case = TRUE))
  
  # Usage section should remain unchanged
  expect_true(grepl("## Usage\nmean\\(x, trim = 0, na.rm = FALSE, \\.\\.\\.\\)", transformed_villain))
})

test_that("all character voices have transform_purpose methods", {
  factory <- CharacterVoiceFactory$new()
  
  # Check all character voices
  for (voice_name in names(factory$voices)) {
    if (voice_name == "none") next  # Skip the "none" voice as it's special
    
    voice <- factory$get_voice(voice_name)
    test_lines <- c("This function computes the mean of a vector.", 
                   "It handles NA values appropriately.")
    
    # Should transform the purpose lines
    transformed <- voice$transform_purpose(test_lines)
    expect_type(transformed, "character")
    expect_length(transformed, length(test_lines))
    
    # Transformed should be different from original (except in rare cases)
    different_lines <- any(transformed != test_lines)
    expect_true(different_lines, 
               info = paste0("Voice '", voice_name, "' should transform purpose lines"))
  }
})