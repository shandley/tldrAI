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
        eccentric_scientist = EccentricScientistVoice$new()
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