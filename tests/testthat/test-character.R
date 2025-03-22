test_that("character voice factory returns all expected voices", {
  factory <- CharacterVoiceFactory$new()
  voices <- factory$list_voices()
  
  # Check that we have the required voices
  expected_voices <- c("none", "enthusiastic_explorer", "cynical_detective", 
                      "wise_mentor", "eccentric_scientist", "conspiracy_theorist",
                      "exaggerator", "reluctant_helper", "time_traveler", 
                      "theatrical_villain")
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
  
  # Conspiracy Theorist voice should modify section headers
  conspiracy_result <- apply_character_voice(test_response, "conspiracy_theorist")
  expect_match(conspiracy_result, "REAL Purpose (That THEY Don't Want You To Know)", fixed = TRUE)
  expect_match(conspiracy_result, "How to Use It (While Staying Off the Grid)", fixed = TRUE)
  
  # Exaggerator voice should modify section headers
  exaggerator_result <- apply_character_voice(test_response, "exaggerator")
  expect_match(exaggerator_result, "ABSOLUTELY REVOLUTIONARY Purpose", fixed = TRUE)
  expect_match(exaggerator_result, "LIFE-CHANGING Usage", fixed = TRUE)
  
  # Reluctant Helper voice should modify section headers
  reluctant_result <- apply_character_voice(test_response, "reluctant_helper")
  expect_match(reluctant_result, "*sigh* Fine, here's the Purpose", fixed = TRUE)
  expect_match(reluctant_result, "Usage (since you can't figure it out yourself)", fixed = TRUE)
  
  # Time Traveler voice should modify section headers
  time_traveler_result <- apply_character_voice(test_response, "time_traveler")
  expect_match(time_traveler_result, "Ancient Purpose (circa 2023)", fixed = TRUE)
  expect_match(time_traveler_result, "Primitive Usage Methods", fixed = TRUE)
  
  # Theatrical Villain voice should modify section headers
  villain_result <- apply_character_voice(test_response, "theatrical_villain")
  expect_match(villain_result, "DIABOLICAL Purpose", fixed = TRUE)
  expect_match(villain_result, "Execution of My EVIL PLAN", fixed = TRUE)
  
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
  expect_true(nrow(voices) >= 10)  # At least our 10 voices
})

test_that("conspiracy theorist voice adds conspiracy elements", {
  test_response <- "```\n# mean\n\n## Purpose\nCalculate the arithmetic mean of values.\n\n## Usage\nmean(x, na.rm = FALSE)\n\n## Key Arguments\n- x: A numeric vector\n- na.rm: Whether to remove NA values\n```"
  
  result <- apply_character_voice(test_response, "conspiracy_theorist")
  
  # Should add conspiracy language
  expect_match(result, "THEY", fixed = TRUE)
  
  # Should modify bullet points
  expect_match(result, "*lowers voice*", fixed = TRUE)
  
  # Should preserve code blocks
  expect_match(result, "mean(x, na.rm = FALSE)", fixed = TRUE)
  
  # Should add conspiracy theories at paragraph breaks
  theories <- c(
    "This is all connected", 
    "hiding this function", 
    "reports your usage", 
    "track your computational patterns",
    "cover story"
  )
  has_theory <- any(sapply(theories, function(t) grepl(t, result, fixed = FALSE)))
  expect_true(has_theory)
})

test_that("exaggerator voice adds hyperbole", {
  test_response <- "```\n# mean\n\n## Purpose\nCalculate the arithmetic mean of values.\n\n## Usage\nmean(x, na.rm = FALSE)\n\n## Key Arguments\n- x: A numeric vector\n- na.rm: Whether to remove NA values\n```"
  
  result <- apply_character_voice(test_response, "exaggerator")
  
  # Should add exaggerated language
  expect_match(result, "ABSOLUTELY REVOLUTIONARY", fixed = TRUE)
  expect_match(result, "LIFE-CHANGING", fixed = TRUE)
  
  # Should modify bullet points
  expect_match(result, "THE INCREDIBLE", fixed = TRUE)
  expect_match(result, "TRANSFORM", fixed = TRUE)
  
  # Should add ALL CAPS words
  caps_words <- c("CRUCIAL", "ESSENTIAL", "REVOLUTIONARY", "EXTRAORDINARY", 
                 "INCREDIBLE", "UNBELIEVABLE", "SPECTACULAR", "PHENOMENAL")
  has_caps_word <- any(sapply(caps_words, function(w) grepl(w, result, fixed = TRUE)))
  expect_true(has_caps_word)
})

test_that("reluctant helper voice adds reluctant elements", {
  test_response <- "```\n# mean\n\n## Purpose\nCalculate the arithmetic mean of values.\n\n## Usage\nmean(x, na.rm = FALSE)\n```"
  
  result <- apply_character_voice(test_response, "reluctant_helper")
  
  # Should add reluctant language
  expect_match(result, "*sigh*", fixed = TRUE)
  
  # Should add intro and outro
  expect_match(result, "*reluctantly starts typing*", fixed = TRUE)
  expect_match(result, "Happy now?", fixed = TRUE)
  
  # Should add sarcastic phrases
  expect_true(
    grepl("*heavy sigh*", result, fixed = TRUE) ||
    grepl("if you're still reading this", result, fixed = TRUE) ||
    grepl("eye roll", result, fixed = TRUE) ||
    grepl("everything spelled out", result, fixed = TRUE)
  )
})

test_that("time traveler voice adds futuristic elements", {
  test_response <- "```\n# mean\n\n## Purpose\nCalculate the arithmetic mean of values.\n\n## Usage\nmean(x, na.rm = FALSE)\n```"
  
  result <- apply_character_voice(test_response, "time_traveler")
  
  # Should add time travel language
  expect_match(result, "Ancient Purpose", fixed = TRUE)
  expect_match(result, "Primitive Usage Methods", fixed = TRUE)
  
  # Should add future intro and outro
  expect_match(result, "*materializes from the future*", fixed = TRUE)
  expect_match(result, "Greetings from the year", fixed = TRUE)
  expect_match(result, "primitive tools", fixed = TRUE)
  
  # Should mention future technologies
  future_techs <- c(
    "quantum-neural", 
    "thought-code", 
    "multi-dimensional", 
    "consciousness-based",
    "temporal logic",
    "hyper-dimensional"
  )
  has_future_tech <- any(sapply(future_techs, function(t) grepl(t, result, fixed = FALSE)))
  expect_true(has_future_tech)
})

test_that("theatrical villain voice adds villainous elements", {
  test_response <- "```\n# mean\n\n## Purpose\nCalculate the arithmetic mean of values.\n\n## Usage\nmean(x, na.rm = FALSE)\n```"
  
  result <- apply_character_voice(test_response, "theatrical_villain")
  
  # Should add villainous language
  expect_match(result, "DIABOLICAL Purpose", fixed = TRUE)
  expect_match(result, "EVIL PLAN", fixed = TRUE)
  
  # Should add villainous intro and outro
  expect_match(result, "*descends dramatically", fixed = TRUE)
  expect_match(result, "MWAHAHAHA", fixed = TRUE)
  
  # Should add villainous phrases
  villain_phrases <- c(
    "twirls mustache", 
    "WITNESS", 
    "BEHOLD", 
    "You fool",
    "MY WILL"
  )
  has_villain_phrase <- any(sapply(villain_phrases, function(p) grepl(p, result, fixed = FALSE)))
  expect_true(has_villain_phrase)
})