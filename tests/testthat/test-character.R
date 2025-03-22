test_that("character voice factory returns all expected voices", {
  factory <- CharacterVoiceFactory$new()
  voices <- factory$list_voices()
  
  # Check that we have the required voices
  expected_voices <- c("none", "enthusiastic_explorer", "cynical_detective", 
                      "wise_mentor", "eccentric_scientist")
  expect_true(all(expected_voices %in% voices$name))
  
  # Check that each voice has a description
  expect_true(all(nchar(voices$description) > 0))
})

test_that("each character voice has a working transform method", {
  factory <- CharacterVoiceFactory$new()
  test_response <- "```\n# mean\n\n## Purpose\nCalculate the arithmetic mean of values.\n\n## Usage\nmean(x, na.rm = FALSE)\n\n## Examples\n```r\nmean(c(1, 2, 3, 4, 5))\n```\n```"
  
  # Test each voice transformation
  for (voice_name in names(factory$voices)) {
    voice <- factory$get_voice(voice_name)
    transformed <- voice$transform(test_response)
    
    # Should return a character string
    expect_type(transformed, "character")
    
    # Transformation shouldn't be empty
    expect_true(nchar(transformed) > 0)
    
    # Code blocks should be preserved
    expect_match(transformed, "```r", fixed = TRUE)
    
    if (voice_name != "none") {
      # For non-default voices, the transformation should change the text
      expect_false(identical(transformed, test_response))
    } else {
      # The "none" voice should return the original text unchanged
      expect_identical(transformed, test_response)
    }
  }
})

test_that("apply_character_voice applies the correct transformation", {
  test_response <- "```\n# mean\n\n## Purpose\nCalculate the arithmetic mean of values.\n\n## Usage\nmean(x, na.rm = FALSE)\n```"
  
  # No transformation for "none" voice
  none_result <- apply_character_voice(test_response, "none")
  expect_identical(none_result, test_response)
  
  # Enthusiastic Explorer voice should modify section headers
  explorer_result <- apply_character_voice(test_response, "enthusiastic_explorer")
  expect_match(explorer_result, "Awesome Purpose!", fixed = TRUE)
  expect_match(explorer_result, "Let's Dive In!", fixed = TRUE)
  
  # Cynical Detective voice should modify section headers
  detective_result <- apply_character_voice(test_response, "cynical_detective")
  expect_match(detective_result, "The Real Deal", fixed = TRUE)
  expect_match(detective_result, "How to Use It", fixed = TRUE)
  
  # Invalid voice name should default to no transformation
  invalid_result <- apply_character_voice(test_response, "nonexistent_voice")
  expect_identical(invalid_result, test_response)
})

test_that("tldr_list_voices returns a data frame with expected columns", {
  # Capture output to avoid printing during tests
  output <- utils::capture.output(voices <- tldr_list_voices())
  
  expect_s3_class(voices, "data.frame")
  expect_true("name" %in% names(voices))
  expect_true("description" %in% names(voices))
  expect_true(nrow(voices) >= 5)  # At least our 5 default voices
})