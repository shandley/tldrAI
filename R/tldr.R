#' Get AI-powered quick reference for R functions
#'
#' The main function for retrieving AI-generated help for R functions. It provides concise, 
#' practical explanations with examples, focusing on real-world usage rather than theoretical 
#' details. The function connects to LLM providers (Claude or OpenAI) to generate customized 
#' help text that prioritizes clarity and usefulness.
#'
#' @param func_name Character string specifying the name of an R function. Can be in the form 
#'        "function" or "package::function" (recommended for functions in packages like dplyr or ggplot2).
#' @param verbose Logical indicating whether to include more detailed information. When TRUE, 
#'        returns more comprehensive explanations and additional usage details. Default is FALSE.
#' @param examples Integer indicating the number of examples to include. Higher values provide 
#'        more diverse usage examples. Default is 2.
#' @param refresh Logical indicating whether to ignore cached results and generate a fresh response. 
#'        Useful when checking for updated information. Default is FALSE.
#' @param provider Character string specifying the LLM provider to use. Options are "claude" (default) 
#'        or "openai". Requires API key to be configured for the selected provider.
#' @param voice Character string specifying the character voice to use. Options include "enthusiastic_explorer", 
#'        "cynical_detective", "wise_mentor", "eccentric_scientist", etc. Use tldr_list_voices() to see all available options.
#' @param async Logical indicating whether to make the API call asynchronously. When TRUE, allows you to continue 
#'        working while waiting for the response. Use tldr_check_async() to retrieve the result. Default is FALSE.
#' @param context Logical indicating whether to use contextual awareness. When TRUE, analyzes your R environment 
#'        to provide examples relevant to your data and loaded packages. Default is FALSE.
#' @param visualize Logical indicating whether to include visualizations. When TRUE, generates visualizations to 
#'        help understand the function. Default is FALSE.
#' @param vis_type Character string specifying the visualization type:
#'        \itemize{
#'          \item "diagram": Simple function diagram showing inputs/outputs (default)
#'          \item "flowchart": Logical flow diagram showing conditionals and loops
#'          \item "data_flow": Data transformation diagram (good for dplyr, ggplot2)
#'          \item "function_network": Network diagram showing related functions
#'          \item "code_highlight": Syntax highlighted code with interactive elements
#'        }
#' @param prompt_install Logical indicating whether to prompt for installation of required packages 
#'        for visualizations. When FALSE, falls back to ASCII visualization if packages are missing. Default is TRUE.
#' @param export_visualization Logical indicating whether to export the visualization to a file. Default is FALSE.
#' @param export_path Character string specifying the path to save exported visualization. If NULL, auto-generates 
#'        a path based on function name and visualization type.
#' @param export_format Character string specifying the format for exported visualization: "svg" (vector), 
#'        "png" (bitmap), or "txt" (ASCII). Default is "svg".
#' @param multimodal Logical indicating whether to use the multimodal visualization interface showing 
#'        text documentation and visualizations side by side. Requires htmlwidgets and htmltools 
#'        packages. Default is FALSE.
#' @param theme Character string specifying the color theme for the multimodal interface: "light" (default) 
#'        or "dark".
#'
#' @return Prints formatted help to the console and invisibly returns the raw response as a character string.
#'         The returned object has attributes that can be accessed with attr():
#'         \itemize{
#'           \item "provider": The LLM provider used ("claude" or "openai")
#'           \item "voice": The character voice used (if any)
#'           \item "context_aware": TRUE if contextual awareness was used
#'         }
#'
#' @seealso
#' \code{\link{tldr_config}} for configuration options
#' \code{\link{tldr_context_config}} for contextual awareness settings
#' \code{\link{tldr_list_voices}} for available character voices
#' \code{\link{tldr_check_async}} for retrieving asynchronous responses
#' \code{\link{tldr_visualization_config}} for visualization settings
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' tldr("mean")                     # Basic help for mean function
#' tldr("ggplot2::ggplot")          # Always use package prefix for clarity
#' 
#' # Detailed information
#' tldr("sd", verbose = TRUE)       # More detailed explanation
#' tldr("aggregate", examples = 4)  # More usage examples
#' 
#' # Provider selection
#' tldr("lm", provider = "claude")  # Use Claude API (default)
#' tldr("glm", provider = "openai") # Use OpenAI API
#' 
#' # Character voices
#' tldr("median", voice = "enthusiastic_explorer") # Excited and energetic
#' tldr("sd", voice = "cynical_detective")         # Skeptical and direct
#' tldr("var", voice = "wise_mentor")              # Thoughtful and patient
#' tldr("sum", voice = "eccentric_scientist")      # Quirky and unpredictable
#' 
#' # Contextual awareness
#' tldr("filter", context = TRUE)   # Examples using your actual data
#' tldr("ggplot", context = TRUE)   # Tailored to your environment
#' 
#' # Asynchronous usage
#' tldr("plot", async = TRUE)       # Non-blocking API call
#' result <- tldr_check_async()     # Retrieve the result when ready
#' 
#' # Visualizations
#' tldr("mean", visualize = TRUE)                   # Basic diagram (default)
#' tldr("if", vis_type = "flowchart")               # Logical flow diagram
#' tldr("dplyr::filter", vis_type = "data_flow")    # Data transformation flow
#' tldr("lm", vis_type = "function_network")        # Related functions network
#' tldr("mean", vis_type = "code_highlight")        # Syntax highlighted code
#' 
#' # Export visualizations
#' tldr("mean", visualize = TRUE, 
#'      export_visualization = TRUE, 
#'      export_path = "mean_diagram.svg")           # Export as SVG
#'      
#' tldr("if", vis_type = "flowchart", 
#'      export_visualization = TRUE, 
#'      export_path = "if_flowchart.png", 
#'      export_format = "png")                      # Export as PNG
#' 
#' # Multimodal interface (text and visualization combined)
#' tldr("dplyr::filter", multimodal = TRUE)         # Light theme (default)
#' tldr("ggplot2::ggplot", multimodal = TRUE, theme = "dark") # Dark theme
#' 
#' # Resolving ambiguous function names
#' library(ggplot2)                 # Load the package first
#' tldr("ggplot")                   # Now refers to ggplot2::ggplot
#' }
#' 
#' @note API keys must be configured before using this function. 
#' Use tldr_config(api_key = "your_claude_api_key") or 
#' tldr_config(openai_api_key = "your_openai_api_key") to set up API access.
tldr <- function(func_name, verbose = NULL, examples = NULL, refresh = FALSE, 
                provider = NULL, voice = NULL, async = NULL, context = NULL,
                visualize = NULL, vis_type = NULL, prompt_install = TRUE,
                export_visualization = FALSE, export_path = NULL, export_format = NULL,
                multimodal = FALSE, theme = "light") {
  # Validate input
  if (!is.character(func_name) || length(func_name) != 1) {
    stop("func_name must be a single character string")
  }
  
  # Use defaults if NULL
  if (is.null(verbose)) verbose <- get_config("verbose_default", default = FALSE)
  if (is.null(examples)) examples <- get_config("examples_default", default = 2)
  if (is.null(voice)) voice <- get_config("character_voice", default = "none")
  if (is.null(async)) async <- get_config("async_mode", default = FALSE)
  if (is.null(context)) {
    # Get default context setting from config
    config <- get_config_all()
    if (!is.null(config$context_settings) && !is.null(config$context_settings$enable_context_awareness)) {
      context <- config$context_settings$enable_context_awareness
    } else {
      context <- FALSE  # Default to false if not configured
    }
  }
  
  # Handle visualization settings
  if (is.null(visualize)) {
    # Get default visualization setting from config
    config <- get_config_all()
    if (!is.null(config$visualization_settings) && !is.null(config$visualization_settings$enable_visualization)) {
      visualize <- config$visualization_settings$enable_visualization
    } else {
      visualize <- FALSE  # Default to false if not configured
    }
  }
  
  if (is.null(vis_type)) {
    # Get default visualization type from config
    config <- get_config_all()
    if (!is.null(config$visualization_settings) && !is.null(config$visualization_settings$default_type)) {
      vis_type <- config$visualization_settings$default_type
    } else {
      vis_type <- "diagram"  # Default to diagram if not configured
    }
  }
  
  # Set refresh mode temporarily in the config to be used by get_ai_response
  if (refresh) {
    old_refresh_mode <- get_config("refresh_mode", default = FALSE)
    tldr_config(refresh_mode = TRUE)
    on.exit(tldr_config(refresh_mode = old_refresh_mode))
  }
  
  # Handle package::function format
  pkg <- NULL
  if (grepl("::", func_name, fixed = TRUE)) {
    parts <- strsplit(func_name, "::", fixed = TRUE)[[1]]
    pkg <- parts[1]
    func_name <- parts[2]
  }
  
  # Determine which provider to use
  selected_provider <- provider %||% get_config("provider", default = "claude")
  
  # Check for cached response
  cache_path <- get_cache_path(func_name, selected_provider)
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: Function name: ", func_name)
    message("DEBUG: Provider: ", selected_provider)
    message("DEBUG: Cache path: ", cache_path)
    message("DEBUG: Async mode: ", async)
    message("DEBUG: Context mode: ", context)
  }
  
  # ALWAYS generate a fresh response in context mode, since data frames and environment can change
  # This prevents stale contextual examples when data frames are deleted or modified
  if (!refresh && !context && file.exists(cache_path)) {
    # Check if cache is expired
    cache_ttl <- get_config("cache_ttl", default = 30)
    file_time <- file.info(cache_path)$mtime
    now <- Sys.time()
    
    if (difftime(now, file_time, units = "days") <= cache_ttl) {
      response <- readRDS(cache_path)
      
      # Apply character voice transformation if selected
      if (voice != "none") {
        response <- apply_character_voice(response, voice)
        attr(response, "voice") <- voice
      }
      
      # Create visualization if requested
      visualization <- NULL
      if (visualize) {
        if (get_config("debug_mode", default = FALSE)) {
          message("DEBUG: Creating visualization of type: ", vis_type)
        }
        
        # Get function metadata
        metadata <- get_function_metadata(func_name, pkg)
        
        # Create the visualization
        visualization <- create_visualization(func_name, metadata, vis_type)
      }
      
      print_tldr_response(response, func_name, verbose, examples, 
                         selected_provider, voice, visualization)
      return(invisible(response))
    } else if (get_config("offline_mode", default = FALSE)) {
      # In offline mode, use expired cache anyway
      response <- readRDS(cache_path)
      message("Using expired cached response (offline mode)")
      
      # Apply character voice transformation if selected
      if (voice != "none") {
        response <- apply_character_voice(response, voice)
        attr(response, "voice") <- voice
      }
      
      # Create visualization if requested
      visualization <- NULL
      if (visualize) {
        if (get_config("debug_mode", default = FALSE)) {
          message("DEBUG: Creating visualization of type: ", vis_type)
        }
        
        # Get function metadata
        metadata <- get_function_metadata(func_name, pkg)
        
        # Create the visualization with prompt_install parameter
        visualization <- create_visualization(func_name, metadata, vis_type, prompt_install = prompt_install)
      }
      
      print_tldr_response(response, func_name, verbose, examples, 
                         selected_provider, voice, visualization)
      return(invisible(response))
    }
    # Otherwise continue to get a fresh response
  } else if ((refresh || context) && get_config("offline_mode", default = FALSE)) {
    if (context) {
      stop("Cannot use context awareness in offline mode. Disable offline mode first with tldr_offline(FALSE).")
    } else {
      stop("Cannot refresh in offline mode. Disable offline mode first with tldr_offline(FALSE).")
    }
  }
  
  # Get function metadata
  func_metadata <- tryCatch({
    metadata <- get_function_metadata(func_name, pkg)
    
    # Debug information if debug mode is enabled
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Function metadata:")
      message("DEBUG:   - Package: ", metadata$package)
      message("DEBUG:   - Signature: ", metadata$signature)
      if (!is.null(metadata$description)) {
        message("DEBUG:   - Description: ", substr(metadata$description, 1, 50), "...")
      } else if (!is.null(metadata$missing_package) && metadata$missing_package) {
        message("DEBUG:   - Missing package: ", metadata$package)
      } else if (!is.null(metadata$missing_function) && metadata$missing_function) {
        message("DEBUG:   - Missing function: ", metadata$name, " in package ", metadata$package)
      }
    }
    
    metadata
  }, error = function(e) {
    if (get_config("offline_mode", default = FALSE) && file.exists(cache_path)) {
      message("Function not found, using cached response (offline mode)")
      response <- readRDS(cache_path)
      
      # Apply character voice transformation if selected
      if (voice != "none") {
        response <- apply_character_voice(response, voice)
        attr(response, "voice") <- voice
      }
      
      print_tldr_response(response, func_name, verbose, examples, selected_provider, voice)
      return(invisible(response))
    } else {
      stop(e)
    }
  })
  
  # Handle contextual awareness
  context_data <- NULL
  if (context) {
    # Check if the function has a missing package flag
    if (!is.null(func_metadata$missing_package) && func_metadata$missing_package) {
      # If the package is missing, we'll still provide context data but note the missing package
      context_data <- paste0("CONTEXT NOTE: While analyzing your environment, I notice that you're interested in the '", 
                           func_name, "' function from the '", func_metadata$package, 
                           "' package, which is not installed. I've provided general information about this function",
                           " and installation instructions above.")
      
      # Add the basic installation command from the metadata
      if (!is.null(func_metadata$install_command)) {
        context_data <- paste0(context_data, "\n\nINSTALLATION: ", func_metadata$install_command)
      }
    } else {
      # Regular context processing for installed packages or functions
      # Get context settings
      config <- get_config_all()
      context_settings <- config$context_settings %||% list(
        enable_context_awareness = TRUE,
        analyze_data_frames = TRUE,
        analyze_packages = TRUE,
        analyze_history = TRUE,
        anonymize_data = TRUE,
        max_rows_sample = 5,
        max_cols_sample = 5,
        include_row_count = TRUE,
        include_class_info = TRUE,
        include_column_types = TRUE,
        max_history_commands = 10
      )
      
      # Create context analyzer and analyze environment
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Analyzing user environment context")
      }
      
      # Initialize context analyzer with current settings
      context_analyzer <- ContextAnalyzer$new(context_settings)
      
      # Analyze the environment
      context_analyzer$analyze_environment()
      
      # Format context data for prompt
      context_data <- context_analyzer$format_context_for_prompt(func_name, func_metadata)
      
      # Generate contextual examples if possible
      contextual_examples <- context_analyzer$generate_contextual_examples(func_name, func_metadata)
      
      # Add suggested next steps if available
      next_steps <- context_analyzer$suggest_next_steps(func_name, func_metadata)
      
      if (length(next_steps) > 0) {
        context_data <- paste0(context_data, "\n\nWORKFLOW SUGGESTIONS:\n", paste(next_steps, collapse = "\n"))
      }
      
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Context data generated successfully")
      }
    }
  }
  
  # Build prompt with or without context
  prompt <- build_prompt(func_name, func_metadata, verbose, examples, context_data)
  
  # Get API response in offline mode?
  if (get_config("offline_mode", default = FALSE) && !file.exists(cache_path)) {
    stop("Function information not cached and offline mode is enabled. Disable offline mode to fetch response.")
  }
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    # Create a clean prompt with function name for debug output
    debug_prompt <- gsub("{{FUNCTION_NAME}}", func_name, prompt, fixed = TRUE)
    debug_prompt <- gsub("{{FUNCTION_SIGNATURE}}", func_metadata$signature, debug_prompt, fixed = TRUE)
    debug_prompt <- gsub("{{PACKAGE_NAME}}", func_metadata$package, debug_prompt, fixed = TRUE)
    
    message("DEBUG: Generated prompt:")
    message(debug_prompt)
  }
  
  # Handle async API calls
  if (async) {
    # Check if required packages are available
    if (!requireNamespace("future", quietly = TRUE) || !requireNamespace("promises", quietly = TRUE)) {
      warning("Async mode requires the 'future' and 'promises' packages. Install with: install.packages(c(\"future\", \"promises\"))")
      async <- FALSE
    } else {
      # Initialize future if needed
      if (!getOption("future.initialized", FALSE)) {
        future::plan(future::multisession)
        options(future.initialized = TRUE)
      }
    }
  }
  
  # Get API response (with async support)
  response_obj <- get_ai_response(prompt, provider_override = selected_provider, async = async)
  
  # Handle async response
  if (async && inherits(response_obj, "FutureClass")) {
    message("Async request initiated. Use tldr_check_async() to retrieve the result.")
    
    # Register the response future for later retrieval
    assign("tldr_last_async_request", list(
      future = response_obj,
      func_name = func_name,
      verbose = verbose,
      examples = examples,
      provider = selected_provider,
      voice = voice,
      context = context,
      visualize = visualize,
      vis_type = vis_type,
      prompt_install = prompt_install,
      export_visualization = export_visualization,
      export_path = export_path,
      export_format = export_format,
      multimodal = multimodal,
      theme = theme,
      timestamp = Sys.time()
    ), envir = .GlobalEnv)
    
    return(invisible(response_obj))
  }
  
  # Debug information if debug mode is enabled
  if (get_config("debug_mode", default = FALSE)) {
    message("DEBUG: API Response (first 100 chars): ", substr(response_obj, 1, 100))
  }
  
  # Store provider info for output formatting
  attr(response_obj, "provider") <- selected_provider
  
  # Apply character voice transformation if selected
  if (voice != "none") {
    response_obj <- apply_character_voice(response_obj, voice)
    attr(response_obj, "voice") <- voice
  }
  
  # Add context-aware flag if used
  if (context) {
    attr(response_obj, "context_aware") <- TRUE
  }
  
  # Create visualization if requested
  visualization <- NULL
  if (visualize) {
    if (get_config("debug_mode", default = FALSE)) {
      message("DEBUG: Creating visualization of type: ", vis_type)
    }
    
    # Get function metadata
    metadata <- get_function_metadata(func_name, pkg)
    
    # Create the visualization with prompt_install parameter
    visualization <- create_visualization(func_name, metadata, vis_type, prompt_install = prompt_install)
  }
  
  # Print formatted response
  print_tldr_response(response_obj, func_name, verbose, examples, 
                     selected_provider, voice, visualization,
                     multimodal, theme)
  
  # Handle export if requested explicitly or via auto-export config
  vis_settings <- get_config("visualization_settings", 
                            default = list(auto_export = FALSE, 
                                          export_format = "svg",
                                          export_dir = getwd()))
  
  auto_export <- vis_settings$auto_export %||% FALSE
  
  if ((export_visualization || auto_export) && !is.null(visualization)) {
    if (is.null(export_path)) {
      # Get export directory from config or use working directory
      export_dir <- vis_settings$export_dir %||% getwd()
      if (!dir.exists(export_dir)) {
        export_dir <- getwd()
      }
      
      # Create a default export path if none provided
      export_path <- file.path(export_dir, paste0(func_name, "_", vis_type))
      
      # Add appropriate extension based on format
      export_fmt <- export_format %||% vis_settings$export_format %||% "svg"
      export_path <- paste0(export_path, ".", export_fmt)
    }
    
    # Perform the export
    export_visualization(visualization, export_path, format = export_format)
  }
  
  invisible(response_obj)
}

#' Check and retrieve results from asynchronous tldr calls
#'
#' @param wait Logical indicating whether to wait for the result if not ready
#' @param timeout Numeric value specifying how long to wait for the result (in seconds)
#'
#' @return The result of the asynchronous tldr call
#' @export
#'
#' @examples
#' \dontrun{
#' tldr("mean", async = TRUE)
#' # Do other work...
#' result <- tldr_check_async()
#' }
tldr_check_async <- function(wait = TRUE, timeout = 30) {
  # Check if there is a registered async request
  if (!exists("tldr_last_async_request", envir = .GlobalEnv)) {
    stop("No asynchronous request found. Make a call with tldr(..., async = TRUE) first.")
  }
  
  # Get the async request object
  async_req <- get("tldr_last_async_request", envir = .GlobalEnv)
  
  # Check if ready
  is_ready <- future::resolved(async_req$future)
  
  if (!is_ready && !wait) {
    message("Async request is still processing. Set wait=TRUE to wait for completion.")
    return(invisible(NULL))
  } else if (!is_ready && wait) {
    message("Waiting for async request to complete (timeout: ", timeout, " seconds)...")
    
    start_time <- Sys.time()
    while (!future::resolved(async_req$future)) {
      Sys.sleep(0.5)
      if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
        stop("Async request timed out after ", timeout, " seconds. Try again later with tldr_check_async().")
      }
    }
  }
  
  # Get the result
  tryCatch({
    response <- future::value(async_req$future)
    
    # Store provider info for output formatting
    attr(response, "provider") <- async_req$provider
    
    # Apply character voice transformation if selected
    if (async_req$voice != "none") {
      response <- apply_character_voice(response, async_req$voice)
      attr(response, "voice") <- async_req$voice
    }
    
    # Store context awareness info for output
    if (!is.null(async_req$context) && async_req$context) {
      attr(response, "context_aware") <- TRUE
    }
    
    # Create visualization if requested
    visualization <- NULL
    if (!is.null(async_req$visualize) && async_req$visualize) {
      if (get_config("debug_mode", default = FALSE)) {
        message("DEBUG: Creating visualization of type: ", async_req$vis_type)
      }
      
      # Get function metadata for the function
      func_parts <- strsplit(async_req$func_name, "::", fixed = TRUE)[[1]]
      if (length(func_parts) > 1) {
        pkg <- func_parts[1]
        func_name <- func_parts[2]
      } else {
        pkg <- NULL
        func_name <- async_req$func_name
      }
      
      metadata <- get_function_metadata(func_name, pkg)
      
      # Create the visualization with prompt_install parameter
      # For async calls, we need to store the prompt_install parameter in the async_req object
      visualization <- create_visualization(func_name, metadata, async_req$vis_type, prompt_install = async_req$prompt_install %||% TRUE)
    }
    
    # Print formatted response
    print_tldr_response(response, async_req$func_name, 
                      async_req$verbose, async_req$examples, 
                      async_req$provider, async_req$voice,
                      visualization,
                      async_req$multimodal %||% FALSE, 
                      async_req$theme %||% "light")
    
    # Handle export if requested explicitly or via auto-export config
    vis_settings <- get_config("visualization_settings", 
                               default = list(auto_export = FALSE, 
                                             export_format = "svg",
                                             export_dir = getwd()))
    
    auto_export <- vis_settings$auto_export %||% FALSE
    
    if (((!is.null(async_req$export_visualization) && async_req$export_visualization) || 
        auto_export) && !is.null(visualization)) {
      
      export_path <- async_req$export_path
      if (is.null(export_path)) {
        # Get export directory from config or use working directory
        export_dir <- vis_settings$export_dir %||% getwd()
        if (!dir.exists(export_dir)) {
          export_dir <- getwd()
        }
        
        # Create a default export path if none provided
        export_path <- file.path(export_dir, paste0(async_req$func_name, "_", async_req$vis_type))
        
        # Add appropriate extension based on format
        export_fmt <- async_req$export_format %||% vis_settings$export_format %||% "svg"
        export_path <- paste0(export_path, ".", export_fmt)
      }
      
      # Perform the export
      export_visualization(visualization, export_path, format = async_req$export_format)
    }
    
    # Clean up the async request
    rm("tldr_last_async_request", envir = .GlobalEnv)
    
    invisible(response)
  }, error = function(e) {
    # Clean up the async request on error
    rm("tldr_last_async_request", envir = .GlobalEnv)
    stop("Error in async request: ", e$message)
  })
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}