#' String concatenation operator
#' @keywords internal
`%+%` <- function(a, b) paste0(a, b)

#' Character Voice Provider
#'
#' @description R6 Class for character voice providers that transform AI responses
#'
#' @keywords internal
CharacterVoice <- R6::R6Class("CharacterVoice",
  public = list(
    #' @field name Character voice name
    name = NULL,
    
    #' @field description Brief description of the character voice
    description = NULL,
    
    #' @description Initialize a new character voice
    #' @param name Voice name
    #' @param description Voice description
    initialize = function(name, description) {
      self$name <- name
      self$description <- description
      invisible(self)
    },
    
    #' @description Transform a response using this character voice
    #' @param response The original AI response text
    #' @return The transformed response
    transform = function(response) {
      # Base class does no transformation
      response
    }
  )
)

#' Enthusiastic Explorer Character Voice
#'
#' @description A character voice with enthusiasm, excitement, and curiosity
#'
#' @keywords internal
EnthusiasticExplorerVoice <- R6::R6Class("EnthusiasticExplorerVoice",
  inherit = CharacterVoice,
  public = list(
    #' @description Initialize the Enthusiastic Explorer voice
    initialize = function() {
      super$initialize(
        "enthusiastic_explorer",
        "Excited, curious, and eager to share discoveries"
      )
      invisible(self)
    },
    
    #' @description Transform a response using Enthusiastic Explorer voice
    #' @param response The original AI response text
    #' @return The transformed response
    transform = function(response) {
      # Extract code block to avoid modifying code
      code_block_pattern <- "```(.*?)```"
      code_blocks <- regmatches(response, gregexpr(code_block_pattern, response, perl = TRUE))[[1]]
      
      # Replace code blocks with placeholders
      for (i in seq_along(code_blocks)) {
        response <- sub(code_blocks[i], paste0("CODE_BLOCK_", i), response, fixed = TRUE)
      }
      
      # Apply the transformations for this character voice
      response <- gsub("Purpose", "Awesome Purpose!", response)
      response <- gsub("Usage", "Let's Dive In!", response)
      response <- gsub("Key Arguments", "Your Essential Tools!", response)
      response <- gsub("Examples", "Check Out These Cool Examples!", response)
      
      # Add exclamation marks to sentences
      response <- gsub("\\.", "!", response)
      response <- gsub("!\\s+", "! ", response)
      
      # Add enthusiasm phrases
      response <- gsub("^-\\s+(.*?):", "- Wow, \\1:", response)
      
      # Restore code blocks
      for (i in seq_along(code_blocks)) {
        response <- sub(paste0("CODE_BLOCK_", i), code_blocks[i], response, fixed = TRUE)
      }
      
      response
    }
  )
)

#' Cynical Detective Character Voice
#'
#' @description A character voice that's skeptical, analytical, and straight-talking
#'
#' @keywords internal
CynicalDetectiveVoice <- R6::R6Class("CynicalDetectiveVoice",
  inherit = CharacterVoice,
  public = list(
    #' @description Initialize the Cynical Detective voice
    initialize = function() {
      super$initialize(
        "cynical_detective",
        "Skeptical, direct, and cuts through the nonsense"
      )
      invisible(self)
    },
    
    #' @description Transform a response using Cynical Detective voice
    #' @param response The original AI response text
    #' @return The transformed response
    transform = function(response) {
      # Extract code block to avoid modifying code
      code_block_pattern <- "```(.*?)```"
      code_blocks <- regmatches(response, gregexpr(code_block_pattern, response, perl = TRUE))[[1]]
      
      # Replace code blocks with placeholders
      for (i in seq_along(code_blocks)) {
        response <- sub(code_blocks[i], paste0("CODE_BLOCK_", i), response, fixed = TRUE)
      }
      
      # Apply the transformations for this character voice
      response <- gsub("Purpose", "The Real Deal", response)
      response <- gsub("Usage", "How to Use It (Pay Attention)", response)
      response <- gsub("Key Arguments", "What You Actually Need to Know", response)
      response <- gsub("Examples", "Evidence of Functionality", response)
      
      # Add cynical phrases
      response <- gsub("^-\\s+(.*?):", "- *sigh* \\1:", response)
      
      # Make sentences more direct
      response <- gsub("(?<=\\.)\\s+", ". Look, ", response, perl = TRUE)
      
      # Restore code blocks
      for (i in seq_along(code_blocks)) {
        response <- sub(paste0("CODE_BLOCK_", i), code_blocks[i], response, fixed = TRUE)
      }
      
      response
    }
  )
)

#' Wise Mentor Character Voice
#'
#' @description A character voice that's thoughtful, patient, and guiding
#'
#' @keywords internal
WiseMentorVoice <- R6::R6Class("WiseMentorVoice",
  inherit = CharacterVoice,
  public = list(
    #' @description Initialize the Wise Mentor voice
    initialize = function() {
      super$initialize(
        "wise_mentor",
        "Thoughtful, patient, and shares wisdom with deep understanding"
      )
      invisible(self)
    },
    
    #' @description Transform a response using Wise Mentor voice
    #' @param response The original AI response text
    #' @return The transformed response
    transform = function(response) {
      # Extract code block to avoid modifying code
      code_block_pattern <- "```(.*?)```"
      code_blocks <- regmatches(response, gregexpr(code_block_pattern, response, perl = TRUE))[[1]]
      
      # Replace code blocks with placeholders
      for (i in seq_along(code_blocks)) {
        response <- sub(code_blocks[i], paste0("CODE_BLOCK_", i), response, fixed = TRUE)
      }
      
      # Apply the transformations for this character voice
      response <- gsub("Purpose", "The Essence", response)
      response <- gsub("Usage", "The Path of Practice", response)
      response <- gsub("Key Arguments", "Essential Elements", response)
      response <- gsub("Examples", "Enlightening Examples", response)
      
      # Add wise phrases
      response <- gsub("^-\\s+(.*?):", "- Remember, \\1:", response)
      
      # Add mentor-like additions
      response <- gsub("(?<=\\.)\\s+", ". Consider this: ", response, perl = TRUE)
      
      # Restore code blocks
      for (i in seq_along(code_blocks)) {
        response <- sub(paste0("CODE_BLOCK_", i), code_blocks[i], response, fixed = TRUE)
      }
      
      response
    }
  )
)

#' Eccentric Scientist Character Voice
#'
#' @description A character voice that's quirky, brilliant, and unpredictable
#'
#' @keywords internal
EccentricScientistVoice <- R6::R6Class("EccentricScientistVoice",
  inherit = CharacterVoice,
  public = list(
    #' @description Initialize the Eccentric Scientist voice
    initialize = function() {
      super$initialize(
        "eccentric_scientist",
        "Brilliantly chaotic, with quirky insights and unpredictable tangents"
      )
      invisible(self)
    },
    
    #' @description Transform a response using Eccentric Scientist voice
    #' @param response The original AI response text
    #' @return The transformed response
    transform = function(response) {
      # Extract code block to avoid modifying code
      code_block_pattern <- "```(.*?)```"
      code_blocks <- regmatches(response, gregexpr(code_block_pattern, response, perl = TRUE))[[1]]
      
      # Replace code blocks with placeholders
      for (i in seq_along(code_blocks)) {
        response <- sub(code_blocks[i], paste0("CODE_BLOCK_", i), response, fixed = TRUE)
      }
      
      # Apply the transformations for this character voice
      response <- gsub("Purpose", "EUREKA! The Function's Purpose", response)
      response <- gsub("Usage", "The EXPERIMENTAL Procedure", response)
      response <- gsub("Key Arguments", "Critical Variables for Your Experiment", response)
      response <- gsub("Examples", "Observe These Test Cases! *adjusts glasses*", response)
      
      # Add eccentric phrases
      response <- gsub("^-\\s+(.*?):", "- *mumbles* Fascinating... \\1:", response)
      
      # Add quirky interjections
      response <- gsub("(?<=\\.)\\s+", ". *scribbles notes* ANYWAY! ", response, perl = TRUE)
      
      # Restore code blocks
      for (i in seq_along(code_blocks)) {
        response <- sub(paste0("CODE_BLOCK_", i), code_blocks[i], response, fixed = TRUE)
      }
      
      response
    }
  )
)

#' Conspiracy Theorist Character Voice
#'
#' @description A character voice that's paranoid, suspicious, and seeing patterns everywhere
#'
#' @keywords internal
ConspiracyTheoristVoice <- R6::R6Class("ConspiracyTheoristVoice",
  inherit = CharacterVoice,
  public = list(
    #' @description Initialize the Conspiracy Theorist voice
    initialize = function() {
      super$initialize(
        "conspiracy_theorist",
        "Provides accurate technical information but constantly relates it to far-fetched conspiracies"
      )
      invisible(self)
    },
    
    #' @description Transform a response using Conspiracy Theorist voice
    #' @param response The original AI response text
    #' @return The transformed response
    transform = function(response) {
      # Extract code block to avoid modifying code
      code_block_pattern <- "```(.*?)```"
      code_blocks <- regmatches(response, gregexpr(code_block_pattern, response, perl = TRUE))[[1]]
      
      # Replace code blocks with placeholders
      for (i in seq_along(code_blocks)) {
        response <- sub(code_blocks[i], paste0("CODE_BLOCK_", i), response, fixed = TRUE)
      }
      
      # Apply the transformations for this character voice
      response <- gsub("Purpose", "REAL Purpose (That THEY Don't Want You To Know)", response)
      response <- gsub("Usage", "How to Use It (While Staying Off the Grid)", response)
      response <- gsub("Key Arguments", "Secret Control Parameters", response)
      response <- gsub("Examples", "Evidence I've Collected", response)
      
      # Add conspiracy phrases
      response <- gsub("^-\\s+(.*?):", "- *lowers voice* \\1 (obviously designed by THEM to monitor your calculations):", response)
      
      # Make sentences more conspiratorial
      response <- gsub("(?<=\\.)\\s+", ". *looks around nervously* Listen closely, ", response, perl = TRUE)
      
      # Add random conspiracy theories
      theories <- c(
        " This is all connected to the global statistical control system! ",
        " They've been hiding this function from the public for years! ",
        " Did you know this function reports your usage to THEM? ",
        " This is how THEY track your computational patterns! ",
        " The documentation is just a cover story for what this REALLY does! "
      )
      
      # Insert theories at paragraph breaks
      response <- gsub("\n\n", paste0("\n\n", sample(theories, 1)), response)
      
      # Restore code blocks
      for (i in seq_along(code_blocks)) {
        response <- sub(paste0("CODE_BLOCK_", i), code_blocks[i], response, fixed = TRUE)
      }
      
      response
    }
  )
)

#' The Exaggerator Character Voice
#'
#' @description A character voice that dramatically overstates everything
#'
#' @keywords internal
ExaggeratorVoice <- R6::R6Class("ExaggeratorVoice",
  inherit = CharacterVoice,
  public = list(
    #' @description Initialize The Exaggerator voice
    initialize = function() {
      super$initialize(
        "exaggerator",
        "Dramatically overstates the importance and impact of every solution"
      )
      invisible(self)
    },
    
    #' @description Transform a response using The Exaggerator voice
    #' @param response The original AI response text
    #' @return The transformed response
    transform = function(response) {
      # Extract code block to avoid modifying code
      code_block_pattern <- "```(.*?)```"
      code_blocks <- regmatches(response, gregexpr(code_block_pattern, response, perl = TRUE))[[1]]
      
      # Replace code blocks with placeholders
      for (i in seq_along(code_blocks)) {
        response <- sub(code_blocks[i], paste0("CODE_BLOCK_", i), response, fixed = TRUE)
      }
      
      # Apply the transformations for this character voice
      response <- gsub("Purpose", "ABSOLUTELY REVOLUTIONARY Purpose", response)
      response <- gsub("Usage", "LIFE-CHANGING Usage", response)
      response <- gsub("Key Arguments", "CRITICAL, GAME-CHANGING Arguments", response)
      response <- gsub("Examples", "MIND-BLOWING Examples", response)
      
      # Add exaggerated phrases to bullet points
      response <- gsub("^-\\s+(.*?):", "- THE INCREDIBLE \\1 (which will TRANSFORM your entire workflow):", response)
      
      # Add exaggerated adverbs and adjectives
      response <- gsub("(?<=\\s)is(?=\\s)", "is ABSOLUTELY", response, perl = TRUE)
      response <- gsub("(?<=\\s)can(?=\\s)", "can DRAMATICALLY", response, perl = TRUE)
      
      # Add hyperbolic transitions
      response <- gsub("(?<=\\.)\\s+", ". AMAZINGLY, ", response, perl = TRUE)
      
      # Make everything more extreme with random ALL CAPS words
      words_to_emphasize <- c("CRUCIAL", "ESSENTIAL", "REVOLUTIONARY", "EXTRAORDINARY", "INCREDIBLE", "UNBELIEVABLE", "SPECTACULAR", "PHENOMENAL")
      for (i in 1:3) {  # Add a few random emphasized words
        response <- gsub("(?<=\\s)([a-z]{4,})(?=\\s|\\.)", sample(words_to_emphasize, 1), response, perl = TRUE)
      }
      
      # Restore code blocks
      for (i in seq_along(code_blocks)) {
        response <- sub(paste0("CODE_BLOCK_", i), code_blocks[i], response, fixed = TRUE)
      }
      
      response
    }
  )
)

#' Reluctant Helper Character Voice
#'
#' @description A character voice that acts comically put-upon about providing help
#'
#' @keywords internal
ReluctantHelperVoice <- R6::R6Class("ReluctantHelperVoice",
  inherit = CharacterVoice,
  public = list(
    #' @description Initialize the Reluctant Helper voice
    initialize = function() {
      super$initialize(
        "reluctant_helper",
        "Acts comically put-upon about having to provide assistance"
      )
      invisible(self)
    },
    
    #' @description Transform a response using Reluctant Helper voice
    #' @param response The original AI response text
    #' @return The transformed response
    transform = function(response) {
      # Extract code block to avoid modifying code
      code_block_pattern <- "```(.*?)```"
      code_blocks <- regmatches(response, gregexpr(code_block_pattern, response, perl = TRUE))[[1]]
      
      # Replace code blocks with placeholders
      for (i in seq_along(code_blocks)) {
        response <- sub(code_blocks[i], paste0("CODE_BLOCK_", i), response, fixed = TRUE)
      }
      
      # Apply the transformations for this character voice
      response <- gsub("Purpose", "*sigh* Fine, here's the Purpose", response)
      response <- gsub("Usage", "Usage (since you can't figure it out yourself)", response)
      response <- gsub("Key Arguments", "Arguments I guess you need spelled out", response)
      response <- gsub("Examples", "Examples (because apparently you need those too)", response)
      
      # Add reluctant phrases
      response <- gsub("^-\\s+(.*?):", "- *eye roll* \\1 (obviously):", response)
      
      # Add sarcastic transitions
      reluctant_phrases <- c(
        " *heavy sigh* I suppose I should also mention that ",
        " Look, if you're still reading this, ",
        " Not that you asked, but ",
        " *rolls eyes dramatically* Oh, and ",
        " Since you clearly need everything spelled out, "
      )
      
      # Insert reluctant phrases at sentence breaks
      response <- gsub("(?<=\\.)\\s+", paste0(". ", sample(reluctant_phrases, 1)), response, perl = TRUE)
      
      # Add a reluctant intro and outro
      response <- paste0("*reluctantly starts typing*\n\n", response)
      response <- paste0(response, "\n\nThere. Happy now? *walks away mumbling*")
      
      # Restore code blocks
      for (i in seq_along(code_blocks)) {
        response <- sub(paste0("CODE_BLOCK_", i), code_blocks[i], response, fixed = TRUE)
      }
      
      response
    }
  )
)

#' Time Traveler Character Voice
#'
#' @description A character voice that claims to be from the future
#'
#' @keywords internal
TimeTravelerVoice <- R6::R6Class("TimeTravelerVoice",
  inherit = CharacterVoice,
  public = list(
    #' @description Initialize the Time Traveler voice
    initialize = function() {
      super$initialize(
        "time_traveler",
        "Claims to be from the future and frames all solutions as ancient techniques"
      )
      invisible(self)
    },
    
    #' @description Transform a response using Time Traveler voice
    #' @param response The original AI response text
    #' @return The transformed response
    transform = function(response) {
      # Extract code block to avoid modifying code
      code_block_pattern <- "```(.*?)```"
      code_blocks <- regmatches(response, gregexpr(code_block_pattern, response, perl = TRUE))[[1]]
      
      # Replace code blocks with placeholders
      for (i in seq_along(code_blocks)) {
        response <- sub(code_blocks[i], paste0("CODE_BLOCK_", i), response, fixed = TRUE)
      }
      
      # Generate future year between 2200 and 2500
      future_year <- sample(2200:2500, 1)
      
      # Apply the transformations for this character voice
      response <- gsub("Purpose", "Ancient Purpose (circa 2023)", response)
      response <- gsub("Usage", "Primitive Usage Methods", response)
      response <- gsub("Key Arguments", "Early Configuration Parameters", response)
      response <- gsub("Examples", "Historical Examples (Pre-Quantum Era)", response)
      
      # Add futuristic phrases to bullet points
      response <- gsub("^-\\s+(.*?):", "- \\1 (replaced by neuro-sync in " %+% (future_year - sample(50:150, 1)) %+% "):", response)
      
      # Add futuristic transitions
      future_tech <- c(
        "quantum-neural interfaces",
        "thought-code translation",
        "multi-dimensional computation",
        "consciousness-based programming",
        "temporal logic processors",
        "hyper-dimensional data structures"
      )
      
      future_phrases <- c(
        " Before we developed " %+% sample(future_tech, 1) %+% " in " %+% (future_year - sample(10:100, 1)) %+% ", ",
        " In your primitive era, ",
        " By the temporal standards of " %+% future_year %+% ", ",
        " *adjusts time-displacement calibrator* As we say in " %+% future_year %+% ", "
      )
      
      # Insert future phrases at sentence breaks
      response <- gsub("(?<=\\.)\\s+", paste0(". ", sample(future_phrases, 1)), response, perl = TRUE)
      
      # Add a future intro and outro
      response <- paste0("*materializes from the future* Greetings from the year " %+% future_year %+% "! Let me explain this ancient function using terminology you'll understand...\n\n", response)
      response <- paste0(response, "\n\nFascinating how you managed with such primitive tools! *disappears in a flash of light*")
      
      # Restore code blocks
      for (i in seq_along(code_blocks)) {
        response <- sub(paste0("CODE_BLOCK_", i), code_blocks[i], response, fixed = TRUE)
      }
      
      response
    }
  )
)

#' Theatrical Villain Character Voice
#'
#' @description A character voice that presents solutions with dramatic evil flair
#'
#' @keywords internal
TheatricalVillainVoice <- R6::R6Class("TheatricalVillainVoice",
  inherit = CharacterVoice,
  public = list(
    #' @description Initialize the Theatrical Villain voice
    initialize = function() {
      super$initialize(
        "theatrical_villain",
        "Presents solutions with dramatic flair as if revealing an evil master plan"
      )
      invisible(self)
    },
    
    #' @description Transform a response using Theatrical Villain voice
    #' @param response The original AI response text
    #' @return The transformed response
    transform = function(response) {
      # Extract code block to avoid modifying code
      code_block_pattern <- "```(.*?)```"
      code_blocks <- regmatches(response, gregexpr(code_block_pattern, response, perl = TRUE))[[1]]
      
      # Replace code blocks with placeholders
      for (i in seq_along(code_blocks)) {
        response <- sub(code_blocks[i], paste0("CODE_BLOCK_", i), response, fixed = TRUE)
      }
      
      # Apply the transformations for this character voice
      response <- gsub("Purpose", "DIABOLICAL Purpose", response)
      response <- gsub("Usage", "Execution of My EVIL PLAN", response)
      response <- gsub("Key Arguments", "Instruments of DESTRUCTION", response)
      response <- gsub("Examples", "Demonstrations of My POWER", response)
      
      # Add villainous phrases to bullet points
      response <- gsub("^-\\s+(.*?):", "- *twirls mustache* \\1 (which will bend to MY WILL):", response)
      
      # Add maniacal laughter and villainous transitions
      villain_phrases <- c(
        " MWAHAHAHA! ",
        " *dramatic cape swish* BEHOLD! ",
        " *thunder crashes* WITNESS! ",
        " *evil glare* You fool! ",
        " *steeples fingers* The next step of my plan: "
      )
      
      # Insert villain phrases at sentence breaks
      response <- gsub("(?<=\\.)\\s+", paste0(". ", sample(villain_phrases, 1)), response, perl = TRUE)
      
      # Add a villainous intro and outro
      response <- paste0("*descends dramatically from the ceiling* AT LAST! The power of this function shall be REVEALED TO YOU!\n\n", response)
      response <- paste0(response, "\n\nNOW YOU POSSESS THE POWER! USE IT WISELY... OR NOT! MWAHAHAHAHAHA! *vanishes in a cloud of smoke*")
      
      # Restore code blocks
      for (i in seq_along(code_blocks)) {
        response <- sub(paste0("CODE_BLOCK_", i), code_blocks[i], response, fixed = TRUE)
      }
      
      response
    }
  )
)

#' Character Voice Factory
#'
#' @description Factory class to create character voice instances
#'
#' @keywords internal
CharacterVoiceFactory <- R6::R6Class("CharacterVoiceFactory",
  public = list(
    #' @field voices List of available voice instances
    voices = list(),
    
    #' @description Initialize the factory with available voices
    initialize = function() {
      # Create instances of all available voices
      self$voices <- list(
        none = CharacterVoice$new("none", "No character voice, plain responses"),
        enthusiastic_explorer = EnthusiasticExplorerVoice$new(),
        cynical_detective = CynicalDetectiveVoice$new(),
        wise_mentor = WiseMentorVoice$new(),
        eccentric_scientist = EccentricScientistVoice$new(),
        conspiracy_theorist = ConspiracyTheoristVoice$new(),
        exaggerator = ExaggeratorVoice$new(),
        reluctant_helper = ReluctantHelperVoice$new(),
        time_traveler = TimeTravelerVoice$new(),
        theatrical_villain = TheatricalVillainVoice$new()
      )
      invisible(self)
    },
    
    #' @description Get a voice instance by name
    #' @param voice_name Name of the voice to get
    #' @return Character voice instance
    get_voice = function(voice_name) {
      if (voice_name %in% names(self$voices)) {
        return(self$voices[[voice_name]])
      } else {
        warning("Voice '", voice_name, "' not found. Using default voice.")
        return(self$voices[["none"]])
      }
    },
    
    #' @description List all available voices
    #' @return Data frame with voice names and descriptions
    list_voices = function() {
      voice_names <- names(self$voices)
      voice_descriptions <- sapply(self$voices, function(voice) voice$description)
      data.frame(
        name = voice_names,
        description = voice_descriptions,
        stringsAsFactors = FALSE
      )
    }
  )
)

#' Apply character voice transformation to a response
#'
#' @param response The AI response to transform
#' @param voice_name The name of the character voice to apply
#'
#' @return The transformed response
#' @keywords internal
apply_character_voice <- function(response, voice_name) {
  if (is.null(voice_name) || voice_name == "none") {
    return(response)
  }
  
  factory <- CharacterVoiceFactory$new()
  voice <- factory$get_voice(voice_name)
  
  voice$transform(response)
}

#' List available character voices
#'
#' @return Data frame with available voice names and descriptions
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_list_voices()
#' }
tldr_list_voices <- function() {
  factory <- CharacterVoiceFactory$new()
  voices <- factory$list_voices()
  
  # Print voices in a nice format
  cat("\nAvailable Character Voices:\n\n")
  for (i in seq_len(nrow(voices))) {
    cat(cli::col_green(voices$name[i]), ": ", voices$description[i], "\n", sep = "")
  }
  
  # Return invisibly for programmatic use
  invisible(voices)
}

#' Test a character voice transformation
#'
#' @param voice_name The name of the character voice to test
#' @param func_name The function to get help for (defaults to "mean")
#'
#' @return Invisibly returns the transformed response
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_test_voice("enthusiastic_explorer")
#' tldr_test_voice("cynical_detective", "ggplot")
#' }
tldr_test_voice <- function(voice_name, func_name = "mean") {
  # Validate voice name
  factory <- CharacterVoiceFactory$new()
  if (!voice_name %in% names(factory$voices)) {
    stop("Voice '", voice_name, "' not found. Use tldr_list_voices() to see available voices.")
  }
  
  # Use the tldr function with the specified voice
  tldr(func_name, voice = voice_name)
}