#' Context Analyzer Class
#'
#' @description R6 Class for analyzing R environment context
#'
#' @importFrom R6 R6Class
#' @importFrom utils head object.size
#' @keywords internal
ContextAnalyzer <- R6::R6Class("ContextAnalyzer",
  public = list(
    #' @field context_data A list containing analyzed context data
    context_data = NULL,
    
    #' @field privacy_settings A list containing privacy configuration
    privacy_settings = NULL,
    
    #' @description Initialize a new ContextAnalyzer
    #' @param privacy_settings List of privacy settings
    initialize = function(privacy_settings = NULL) {
      self$privacy_settings <- privacy_settings %||% list(
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
      
      self$context_data <- list(
        data_frames = list(),
        active_packages = character(),
        recent_functions = character(),
        command_history = character(),
        environment_info = list(),
        relevance_scores = list()
      )
      
      invisible(self)
    },
    
    #' @description Analyze the current R environment
    #' @return Self (for method chaining)
    analyze_environment = function() {
      # Respect privacy settings
      if (self$privacy_settings$analyze_data_frames) {
        self$analyze_data_frames()
      }
      
      if (self$privacy_settings$analyze_packages) {
        self$analyze_packages()
      }
      
      if (self$privacy_settings$analyze_history) {
        self$analyze_command_history()
      }
      
      # Analyze general environment info regardless of settings
      self$analyze_environment_info()
      
      invisible(self)
    },
    
    #' @description Analyze data frames in the global environment
    #' @return Self (for method chaining)
    analyze_data_frames = function() {
      # Get all objects in global environment
      all_objects <- ls(envir = .GlobalEnv)
      
      # Filter to keep only data frames
      data_frames <- list()
      
      for (obj_name in all_objects) {
        obj <- get(obj_name, envir = .GlobalEnv)
        
        # Check if object is a data frame or similar structure
        if (is.data.frame(obj) || 
            (requireNamespace("tibble", quietly = TRUE) && tibble::is_tibble(obj)) ||
            is.matrix(obj)) {
          
          # Basic info about the object
          df_info <- list(
            name = obj_name,
            rows = nrow(obj),
            cols = ncol(obj),
            size_bytes = utils::object.size(obj),
            column_names = names(obj)
          )
          
          # Add column types if requested
          if (self$privacy_settings$include_column_types) {
            if (is.data.frame(obj) || (requireNamespace("tibble", quietly = TRUE) && tibble::is_tibble(obj))) {
              df_info$column_types <- vapply(obj, function(x) class(x)[1], character(1))
            } else if (is.matrix(obj)) {
              df_info$column_types <- rep(typeof(obj), ncol(obj))
              names(df_info$column_types) <- colnames(obj)
            }
          }
          
          # Include class information if requested
          if (self$privacy_settings$include_class_info) {
            df_info$class <- class(obj)
          }
          
          # Add data sample if anonymization is disabled
          if (!self$privacy_settings$anonymize_data) {
            # Get a small sample of the data
            max_rows <- min(self$privacy_settings$max_rows_sample, nrow(obj))
            max_cols <- min(self$privacy_settings$max_cols_sample, ncol(obj))
            
            if (max_rows > 0 && max_cols > 0) {
              sample_rows <- seq_len(max_rows)
              sample_cols <- seq_len(max_cols)
              
              if (is.data.frame(obj) || (requireNamespace("tibble", quietly = TRUE) && tibble::is_tibble(obj))) {
                df_info$data_sample <- obj[sample_rows, sample_cols, drop = FALSE]
              } else if (is.matrix(obj)) {
                df_info$data_sample <- obj[sample_rows, sample_cols, drop = FALSE]
              }
            }
          }
          
          # Store the information
          data_frames[[obj_name]] <- df_info
        }
      }
      
      self$context_data$data_frames <- data_frames
      invisible(self)
    },
    
    #' @description Analyze packages and recently used functions
    #' @return Self (for method chaining)
    analyze_packages = function() {
      # Get currently attached packages
      pkg_list <- search()
      pkg_list <- pkg_list[grepl("^package:", pkg_list)]
      pkg_list <- sub("^package:", "", pkg_list)
      
      # Store package list
      self$context_data$active_packages <- pkg_list
      
      # Track recently used functions from command history if allowed
      if (self$privacy_settings$analyze_history) {
        # This will be populated by analyze_command_history
        # Just preparing the structure here
      }
      
      invisible(self)
    },
    
    #' @description Analyze command history
    #' @return Self (for method chaining)
    analyze_command_history = function() {
      # Try to get command history
      history <- tryCatch({
        # Get command history, filtering out tldrAI calls to avoid noise
        hist_file <- tempfile()
        utils::savehistory(hist_file)
        history_lines <- readLines(hist_file, warn = FALSE)
        unlink(hist_file)
        
        # Remove tldrAI function calls to avoid noise
        history_lines <- history_lines[!grepl("^tldr(\\(|_)", history_lines)]
        
        # Get most recent commands, respecting the max limit
        max_count <- min(self$privacy_settings$max_history_commands, length(history_lines))
        recent_commands <- tail(history_lines, max_count)
        
        recent_commands
      }, error = function(e) {
        character(0)  # Return empty if there's an error
      })
      
      # Store command history
      self$context_data$command_history <- history
      
      # Extract function calls from history
      if (length(history) > 0) {
        # Simple regex to extract function calls like function_name(...)
        func_calls <- regmatches(history, regexpr("[[:alnum:]_.]+\\(", history))
        func_calls <- sub("\\($", "", func_calls)
        
        # Store the function calls with duplicates removed
        self$context_data$recent_functions <- unique(func_calls)
      }
      
      invisible(self)
    },
    
    #' @description Analyze general environment information
    #' @return Self (for method chaining)
    analyze_environment_info = function() {
      # Get R version and platform
      self$context_data$environment_info$r_version <- getRversion()
      self$context_data$environment_info$platform <- R.version$platform
      
      # Check for common data analysis frameworks
      self$context_data$environment_info$has_tidyverse <- 
        requireNamespace("tidyverse", quietly = TRUE) || 
        (requireNamespace("dplyr", quietly = TRUE) && 
         requireNamespace("ggplot2", quietly = TRUE))
      
      self$context_data$environment_info$has_data_table <- 
        requireNamespace("data.table", quietly = TRUE)
      
      invisible(self)
    },
    
    #' @description Score data frames for relevance to a given function
    #' @param func_name Name of the function
    #' @param func_metadata Metadata about the function
    #' @return Named vector of relevance scores
    score_data_frame_relevance = function(func_name, func_metadata) {
      if (length(self$context_data$data_frames) == 0) {
        return(numeric(0))
      }
      
      # Initialize scores
      scores <- numeric(length(self$context_data$data_frames))
      names(scores) <- names(self$context_data$data_frames)
      
      # Extract package info
      func_package <- func_metadata$package
      
      # Get function arguments
      func_args <- func_metadata$args
      
      # Score each data frame based on various relevance signals
      for (df_name in names(self$context_data$data_frames)) {
        df_info <- self$context_data$data_frames[[df_name]]
        score <- 0
        
        # Score based on column types for data manipulation functions
        if (func_package %in% c("dplyr", "tidyr", "data.table", "base", "stats")) {
          score <- score + 2
          
          # Score even higher for data frames with reasonable size
          if (df_info$rows > 0 && df_info$rows < 1000 && df_info$cols < 50) {
            score <- score + 1
          }
        }
        
        # Score for machine learning functions
        if (func_package %in% c("caret", "randomForest", "rpart", "glmnet") || 
            func_name %in% c("train", "createDataPartition", "randomForest", "rpart", "glmnet")) {
          score <- score + 3
          
          # Score higher for data frames with more rows (better for ML)
          if (df_info$rows > 50) {
            score <- score + 1
          }
          
          # Score higher for data frames with multiple column types (good for ML examples)
          if (!is.null(df_info$column_types)) {
            n_numeric <- sum(df_info$column_types %in% c("numeric", "integer", "double"))
            n_factor <- sum(df_info$column_types %in% c("character", "factor"))
            n_logical <- sum(df_info$column_types == "logical")
            
            if (n_numeric > 0 && (n_factor > 0 || n_logical > 0)) {
              score <- score + 2  # Has both predictors and potential target variables
            }
          }
        }
        
        # Score higher for plotting functions if data frame has numeric columns
        if (func_package %in% c("ggplot2", "graphics", "lattice")) {
          # Check if data frame has numeric columns for plotting
          if (is.data.frame(df_info$column_types)) {
            has_numeric <- any(sapply(df_info$column_types, function(x) x %in% c("numeric", "integer")))
            if (has_numeric) {
              score <- score + 3
            }
          }
        }
        
        # Score based on recent usage
        if (df_name %in% self$extract_objects_from_history()) {
          score <- score + 2
        }
        
        scores[df_name] <- score
      }
      
      # Store the scores
      self$context_data$relevance_scores[[func_name]] <- scores
      
      # Return in descending order of relevance
      sort(scores, decreasing = TRUE)
    },
    
    #' @description Extract objects used in recent commands
    #' @return Character vector of object names
    extract_objects_from_history = function() {
      if (length(self$context_data$command_history) == 0) {
        return(character(0))
      }
      
      # Get all objects in global environment
      all_objects <- ls(envir = .GlobalEnv)
      
      # Find which objects appear in command history
      used_objects <- character(0)
      
      for (obj in all_objects) {
        # Create a pattern that matches the object as a distinct word/symbol
        pattern <- paste0("\\b", obj, "\\b")
        if (any(grepl(pattern, self$context_data$command_history))) {
          used_objects <- c(used_objects, obj)
        }
      }
      
      used_objects
    },
    
    #' @description Generate function examples using actual data
    #' @param func_name Name of the function
    #' @param func_metadata Metadata about the function
    #' @param top_n Number of top data frames to use
    #' @return List of example strings
    generate_contextual_examples = function(func_name, func_metadata, top_n = 2) {
      # Score data frames for relevance
      relevance_scores <- self$score_data_frame_relevance(func_name, func_metadata)
      
      if (length(relevance_scores) == 0) {
        return(character(0))
      }
      
      # Get top data frames
      top_dfs <- names(head(relevance_scores, top_n))
      
      # Verify all data frames still exist (they could have been deleted since analysis)
      if (length(top_dfs) > 0) {
        # Filter to only include data frames that still exist
        existing_dfs <- top_dfs[sapply(top_dfs, function(df_name) {
          exists(df_name, envir = .GlobalEnv) && is.data.frame(get(df_name, envir = .GlobalEnv))
        })]
        
        if (length(existing_dfs) == 0) {
          # If all the top data frames were deleted, we need to re-analyze the environment
          if (get_config("debug_mode", default = FALSE)) {
            message("DEBUG: Data frames changed, re-analyzing environment")
          }
          self$analyze_environment()
          # Recalculate relevance with current environment
          relevance_scores <- self$score_data_frame_relevance(func_name, func_metadata)
          if (length(relevance_scores) == 0) {
            return(character(0))
          }
          top_dfs <- names(head(relevance_scores, top_n))
        } else {
          # Use only existing data frames
          top_dfs <- existing_dfs
        }
      }
      
      # Generate examples based on function and data frames
      examples <- character(0)
      
      # Extract package and function name
      func_package <- func_metadata$package
      
      # Generate examples based on package and available data
      for (df_name in top_dfs) {
        # Double-check the data frame exists and its info is available
        if (!exists(df_name, envir = .GlobalEnv) || !is.data.frame(get(df_name, envir = .GlobalEnv)) || 
            is.null(self$context_data$data_frames[[df_name]])) {
          next  # Skip to the next data frame
        }
        
        df_info <- self$context_data$data_frames[[df_name]]
        
        # Try to generate examples with proper error handling for missing packages
        
        # Tidyverse/dplyr functions
        if (func_package == "dplyr" && func_name %in% c("filter", "select", "mutate", "summarise", "group_by", 
                                                     "arrange", "distinct", "left_join", "right_join", "inner_join", "full_join")) {
          if (requireNamespace("dplyr", quietly = TRUE)) {
            examples <- c(examples, self$generate_dplyr_example(func_name, df_name, df_info))
          } else {
            # Add a placeholder with installation suggestion
            examples <- c(examples, sprintf("# To use dplyr::%s with your '%s' data frame:\n# First install the package: install.packages(\"dplyr\")", 
                                           func_name, df_name))
          }
        } 
        # tidyr functions
        else if (func_package == "tidyr" && func_name %in% c("pivot_longer", "pivot_wider", "separate", "unite", "drop_na", "fill")) {
          if (requireNamespace("tidyr", quietly = TRUE)) {
            examples <- c(examples, self$generate_tidyr_example(func_name, df_name, df_info))
          } else {
            examples <- c(examples, sprintf("# To use tidyr::%s with your '%s' data frame:\n# First install the package: install.packages(\"tidyr\")", 
                                           func_name, df_name))
          }
        } 
        # ggplot2 functions
        else if (func_package == "ggplot2" && func_name %in% c("ggplot", "geom_point", "geom_line", "geom_bar", 
                                                           "geom_histogram", "geom_boxplot", "geom_density", "facet_wrap", "facet_grid", "theme_minimal")) {
          if (requireNamespace("ggplot2", quietly = TRUE)) {
            examples <- c(examples, self$generate_ggplot_example(func_name, df_name, df_info))
          } else {
            examples <- c(examples, sprintf("# To use ggplot2::%s with your '%s' data frame:\n# First install the package: install.packages(\"ggplot2\")", 
                                           func_name, df_name))
          }
        } 
        # Basic statistics functions
        else if ((func_package == "base" || func_package == "stats") && 
                func_name %in% c("mean", "median", "sum", "min", "max", "sd", "var", "quantile", "range", "summary", "IQR")) {
          examples <- c(examples, self$generate_stats_example(func_name, df_name, df_info))
        } 
        # Statistical modeling functions
        else if (func_package == "stats" && func_name %in% c("lm", "glm", "t.test", "cor", "cor.test", "aov", "anova", "TukeyHSD")) {
          examples <- c(examples, self$generate_stats_model_example(func_name, df_name, df_info))
        }
        # purrr functions
        else if (func_package == "purrr" && func_name %in% c("map", "map_dbl", "map_chr", "map_int", "map_lgl", "map_df", "reduce", "keep", "discard")) {
          if (requireNamespace("purrr", quietly = TRUE)) {
            examples <- c(examples, self$generate_purrr_example(func_name, df_name, df_info))
          } else {
            examples <- c(examples, sprintf("# To use purrr::%s with your '%s' data frame:\n# First install the package: install.packages(\"purrr\")", 
                                           func_name, df_name))
          }
        }
        # Survival analysis
        else if (func_package == "survival" && func_name %in% c("Surv", "survfit", "coxph")) {
          if (requireNamespace("survival", quietly = TRUE)) {
            examples <- c(examples, self$generate_survival_example(func_name, df_name, df_info))
          } else {
            examples <- c(examples, sprintf("# To use survival::%s with your '%s' data frame:\n# First install the package: install.packages(\"survival\")", 
                                           func_name, df_name))
          }
        }
        # data.table functions
        else if (func_package == "data.table" && func_name %in% c("[", "data.table", "setkey", "setDT", "setDF", "fread", "fwrite", "melt", "dcast", "rbindlist")) {
          if (requireNamespace("data.table", quietly = TRUE)) {
            examples <- c(examples, self$generate_data_table_example(func_name, df_name, df_info))
          } else {
            examples <- c(examples, sprintf("# To use data.table::%s with your '%s' data frame:\n# First install the package: install.packages(\"data.table\")", 
                                           func_name, df_name))
          }
        }
        # Machine learning functions
        else if (func_package %in% c("caret", "randomForest", "rpart", "glmnet") || 
                func_name %in% c("train", "trainControl", "createDataPartition", "createFolds", 
                                "randomForest", "importance", "varImpPlot", 
                                "rpart", "rpart.plot", "prune", 
                                "glmnet", "cv.glmnet")) {
          # Check if the relevant ML package is installed
          required_pkg <- func_package
          if (func_package == "unknown" || func_package == "base" || func_package == "stats") {
            # Guess the package based on function name
            if (func_name %in% c("train", "trainControl", "createDataPartition", "createFolds")) {
              required_pkg <- "caret"
            } else if (func_name %in% c("randomForest", "importance", "varImpPlot")) {
              required_pkg <- "randomForest"
            } else if (func_name %in% c("rpart", "prune")) {
              required_pkg <- "rpart"
            } else if (func_name %in% c("glmnet", "cv.glmnet")) {
              required_pkg <- "glmnet"
            }
          }
          
          if (requireNamespace(required_pkg, quietly = TRUE)) {
            examples <- c(examples, self$generate_ml_example(func_name, df_name, df_info))
          } else {
            examples <- c(examples, sprintf("# To use %s::%s with your '%s' data frame:\n# First install the package: install.packages(\"%s\")", 
                                           required_pkg, func_name, df_name, required_pkg))
          }
        }
        # Generic examples for any other function
        else {
          # Generic example
          examples <- c(examples, sprintf("# Using your data frame '%s'\n%s(%s)", df_name, func_name, df_name))
        }
      }
      
      examples
    },
    
    #' @description Generate example for dplyr functions
    #' @param func_name Name of the function
    #' @param df_name Name of the data frame
    #' @param df_info Information about the data frame
    #' @return Example string
    generate_dplyr_example = function(func_name, df_name, df_info) {
      # Get column names
      col_names <- df_info$column_names
      col_types <- df_info$column_types
      
      if (length(col_names) == 0) {
        return(sprintf("# Using your data frame '%s'\n%s(%s)", df_name, func_name, df_name))
      }
      
      # For filter
      if (func_name == "filter") {
        # Select a column for filtering
        col <- col_names[1]
        # If we have column types, use appropriate filtering condition based on column type
        if (!is.null(col_types) && length(col_types) > 0) {
          type <- col_types[col]
          if (type %in% c("character", "factor")) {
            return(sprintf("# Filter rows in your '%s' data frame\n%s |> %s(!is.na(%s))", 
                          df_name, df_name, func_name, col))
          } else if (type %in% c("Date", "POSIXct", "POSIXlt")) {
            return(sprintf("# Filter rows in your '%s' data frame by date\n%s |> %s(%s > as.Date('2023-01-01'))", 
                          df_name, df_name, func_name, col))
          }
        }
        return(sprintf("# Filter rows in your '%s' data frame\n%s |> %s(%s > mean(%s, na.rm = TRUE))", 
                      df_name, df_name, func_name, col, col))
      }
      
      # For select
      if (func_name == "select") {
        cols <- paste(head(col_names, min(3, length(col_names))), collapse = ", ")
        return(sprintf("# Select specific columns from your '%s' data frame\n%s |> %s(%s)", 
                      df_name, df_name, func_name, cols))
      }
      
      # For mutate
      if (func_name == "mutate") {
        if (length(col_names) >= 2) {
          # Check if column types are available and both columns are numeric
          if (!is.null(col_types) && length(col_types) >= 2) {
            type1 <- col_types[col_names[1]]
            type2 <- col_types[col_names[2]]
            
            if (type1 %in% c("character", "factor") && type2 %in% c("character", "factor")) {
              return(sprintf("# Add a new column combining text from your '%s' data frame\n%s |> %s(combined = paste(%s, %s, sep = ' - '))", 
                            df_name, df_name, func_name, col_names[1], col_names[2]))
            } else if (type1 %in% c("Date", "POSIXct", "POSIXlt") || type2 %in% c("Date", "POSIXct", "POSIXlt")) {
              return(sprintf("# Add a date-related column to your '%s' data frame\n%s |> %s(year = lubridate::year(%s))", 
                            df_name, df_name, func_name, col_names[1]))
            }
          }
          return(sprintf("# Add a new column to your '%s' data frame\n%s |> %s(new_var = %s + %s)", 
                        df_name, df_name, func_name, col_names[1], col_names[2]))
        } else {
          return(sprintf("# Add a new column to your '%s' data frame\n%s |> %s(new_var = %s * 2)", 
                        df_name, df_name, func_name, col_names[1]))
        }
      }
      
      # For group_by and summarise
      if (func_name %in% c("group_by", "summarise")) {
        if (length(col_names) >= 2) {
          if (func_name == "group_by") {
            return(sprintf("# Group your '%s' data frame by '%s'\n%s |> %s(%s)", 
                          df_name, col_names[1], df_name, func_name, col_names[1]))
          } else { # summarise
            # If we have column types, use appropriate summarize function based on column type
            if (!is.null(col_types) && length(col_types) >= 2) {
              type2 <- col_types[col_names[2]]
              if (type2 %in% c("character", "factor")) {
                return(sprintf("# Summarize your grouped '%s' data frame\n%s |> group_by(%s) |> %s(count = n(), unique_values = n_distinct(%s))", 
                              df_name, df_name, col_names[1], func_name, col_names[2]))
              }
            }
            return(sprintf("# Summarize your grouped '%s' data frame\n%s |> group_by(%s) |> %s(avg_%s = mean(%s, na.rm = TRUE))", 
                          df_name, df_name, col_names[1], func_name, col_names[2], col_names[2]))
          }
        }
      }
      
      # For arrange
      if (func_name == "arrange") {
        return(sprintf("# Sort your '%s' data frame by '%s'\n%s |> %s(%s)", 
                      df_name, col_names[1], df_name, func_name, col_names[1]))
      }
      
      # For distinct
      if (func_name == "distinct") {
        col_subset <- paste(head(col_names, min(2, length(col_names))), collapse = ", ")
        return(sprintf("# Get unique combinations in your '%s' data frame\n%s |> %s(%s)", 
                      df_name, df_name, func_name, col_subset))
      }
      
      # For join functions
      if (func_name %in% c("left_join", "right_join", "inner_join", "full_join")) {
        return(sprintf("# Join your '%s' data frame with another data frame\n%s |> %s(another_df, by = '%s')", 
                      df_name, df_name, func_name, col_names[1]))
      }
      
      # Generic fallback
      sprintf("# Apply %s to your '%s' data frame\n%s |> %s()", func_name, df_name, df_name, func_name)
    },
    
    #' @description Generate example for ggplot2 functions
    #' @param func_name Name of the function
    #' @param df_name Name of the data frame
    #' @param df_info Information about the data frame
    #' @return Example string
    generate_ggplot_example = function(func_name, df_name, df_info) {
      # Get column names and types
      col_names <- df_info$column_names
      col_types <- df_info$column_types
      
      if (length(col_names) == 0) {
        return(sprintf("# Create a plot using your '%s' data frame\nggplot(%s, aes(x = column1)) + geom_histogram()", 
                      df_name, df_name))
      }
      
      # Find numeric and categorical columns if column types are available
      numeric_cols <- character(0)
      cat_cols <- character(0)
      date_cols <- character(0)
      
      if (!is.null(col_types) && length(col_types) > 0) {
        for (i in seq_along(col_types)) {
          col <- names(col_types)[i]
          type <- col_types[i]
          
          if (type %in% c("numeric", "integer", "double")) {
            numeric_cols <- c(numeric_cols, col)
          } else if (type %in% c("character", "factor")) {
            cat_cols <- c(cat_cols, col)
          } else if (type %in% c("Date", "POSIXct", "POSIXlt")) {
            date_cols <- c(date_cols, col)
          }
        }
      }
      
      # For ggplot base function
      if (func_name == "ggplot") {
        # If we have both numeric and categorical columns, create a boxplot
        if (length(numeric_cols) > 0 && length(cat_cols) > 0) {
          return(sprintf("# Create a boxplot using your '%s' data frame\nggplot(%s, aes(x = %s, y = %s)) + geom_boxplot()", 
                        df_name, df_name, cat_cols[1], numeric_cols[1]))
        } 
        # If we have date and numeric columns, create a time series plot
        else if (length(date_cols) > 0 && length(numeric_cols) > 0) {
          return(sprintf("# Create a time series plot using your '%s' data frame\nggplot(%s, aes(x = %s, y = %s)) + geom_line()", 
                        df_name, df_name, date_cols[1], numeric_cols[1]))
        }
        # If we have two numeric columns, create a scatter plot
        else if (length(numeric_cols) >= 2) {
          return(sprintf("# Create a scatter plot using your '%s' data frame\nggplot(%s, aes(x = %s, y = %s)) + geom_point()", 
                        df_name, df_name, numeric_cols[1], numeric_cols[2]))
        } 
        # If we have one numeric column, create a histogram
        else if (length(numeric_cols) == 1) {
          return(sprintf("# Create a histogram using your '%s' data frame\nggplot(%s, aes(x = %s)) + geom_histogram(bins = 30)", 
                        df_name, df_name, numeric_cols[1]))
        }
        # Fallback using first two columns or just first column
        else if (length(col_names) >= 2) {
          return(sprintf("# Create a scatter plot using your '%s' data frame\nggplot(%s, aes(x = %s, y = %s)) + geom_point()", 
                        df_name, df_name, col_names[1], col_names[2]))
        } else {
          return(sprintf("# Create a histogram using your '%s' data frame\nggplot(%s, aes(x = %s)) + geom_histogram()", 
                        df_name, df_name, col_names[1]))
        }
      }
      
      # For geom_point (scatter plot)
      if (func_name == "geom_point") {
        # If we have two numeric columns, use them
        if (length(numeric_cols) >= 2) {
          return(sprintf("# Create a scatter plot using your '%s' data frame\nggplot(%s, aes(x = %s, y = %s)) + %s(alpha = 0.7) + labs(title = 'Scatter Plot')", 
                        df_name, df_name, numeric_cols[1], numeric_cols[2], func_name))
        } else if (length(col_names) >= 2) {
          return(sprintf("# Create a scatter plot using your '%s' data frame\nggplot(%s, aes(x = %s, y = %s)) + %s()", 
                        df_name, df_name, col_names[1], col_names[2], func_name))
        }
      }
      
      # For geom_line (line plots - good for time series)
      if (func_name == "geom_line") {
        # If we have date and numeric columns, create a time series
        if (length(date_cols) > 0 && length(numeric_cols) > 0) {
          return(sprintf("# Create a time series plot using your '%s' data frame\nggplot(%s, aes(x = %s, y = %s)) + %s() + labs(title = 'Time Series Plot')", 
                        df_name, df_name, date_cols[1], numeric_cols[1], func_name))
        } else if (length(col_names) >= 2) {
          return(sprintf("# Create a line plot using your '%s' data frame\nggplot(%s, aes(x = %s, y = %s)) + %s()", 
                        df_name, df_name, col_names[1], col_names[2], func_name))
        }
      }
      
      # For geom_bar (bar charts - good for categorical variables)
      if (func_name == "geom_bar") {
        # If we have categorical columns, use one
        if (length(cat_cols) > 0) {
          return(sprintf("# Create a bar plot using your '%s' data frame\nggplot(%s, aes(x = %s)) + %s() + labs(title = 'Bar Chart')", 
                        df_name, df_name, cat_cols[1], func_name))
        } else {
          return(sprintf("# Create a bar plot using your '%s' data frame\nggplot(%s, aes(x = %s)) + %s()", 
                        df_name, df_name, col_names[1], func_name))
        }
      }
      
      # For geom_boxplot (box plots - good for categorical vs numeric)
      if (func_name == "geom_boxplot") {
        # If we have both numeric and categorical columns
        if (length(numeric_cols) > 0 && length(cat_cols) > 0) {
          return(sprintf("# Create a boxplot using your '%s' data frame\nggplot(%s, aes(x = %s, y = %s)) + %s() + labs(title = 'Box Plot')", 
                        df_name, df_name, cat_cols[1], numeric_cols[1], func_name))
        } else if (length(col_names) >= 2) {
          return(sprintf("# Create a boxplot using your '%s' data frame\nggplot(%s, aes(x = %s, y = %s)) + %s()", 
                        df_name, df_name, col_names[1], col_names[2], func_name))
        }
      }
      
      # For geom_histogram (histograms - good for distribution of numeric variables)
      if (func_name == "geom_histogram") {
        # If we have numeric columns, use one
        if (length(numeric_cols) > 0) {
          return(sprintf("# Create a histogram using your '%s' data frame\nggplot(%s, aes(x = %s)) + %s(bins = 30) + labs(title = 'Histogram')", 
                        df_name, df_name, numeric_cols[1], func_name))
        } else {
          return(sprintf("# Create a histogram using your '%s' data frame\nggplot(%s, aes(x = %s)) + %s(bins = 30)", 
                        df_name, df_name, col_names[1], func_name))
        }
      }
      
      # For geom_density (density plots - good for distribution of numeric variables)
      if (func_name == "geom_density") {
        # If we have numeric columns, use one
        if (length(numeric_cols) > 0) {
          return(sprintf("# Create a density plot using your '%s' data frame\nggplot(%s, aes(x = %s)) + %s(fill = 'blue', alpha = 0.3) + labs(title = 'Density Plot')", 
                        df_name, df_name, numeric_cols[1], func_name))
        } else {
          return(sprintf("# Create a density plot using your '%s' data frame\nggplot(%s, aes(x = %s)) + %s()", 
                        df_name, df_name, col_names[1], func_name))
        }
      }
      
      # Generic fallback
      sprintf("# Create a plot using your '%s' data frame\nggplot(%s, aes(x = %s)) + %s()", 
              df_name, df_name, col_names[1], func_name)
    },
    
    #' @description Generate example for basic statistics functions
    #' @param func_name Name of the function
    #' @param df_name Name of the data frame
    #' @param df_info Information about the data frame
    #' @return Example string
    generate_stats_example = function(func_name, df_name, df_info) {
      # Get column names and types
      col_names <- df_info$column_names
      col_types <- df_info$column_types
      
      if (length(col_names) == 0) {
        return(sprintf("# Calculate %s from your '%s' data frame\n%s(%s$column_name, na.rm = TRUE)", 
                      func_name, df_name, func_name, df_name))
      }
      
      # Find numeric columns if column types are available
      numeric_cols <- character(0)
      
      if (!is.null(col_types) && length(col_types) > 0) {
        for (i in seq_along(col_types)) {
          col <- names(col_types)[i]
          type <- col_types[i]
          
          if (type %in% c("numeric", "integer", "double")) {
            numeric_cols <- c(numeric_cols, col)
          }
        }
      }
      
      # Use a numeric column if available, otherwise use first column
      target_col <- if (length(numeric_cols) > 0) numeric_cols[1] else col_names[1]
      
      # For different stat functions
      if (func_name == "mean" || func_name == "median") {
        return(sprintf("# Calculate %s of '%s' in your '%s' data frame\n%s(%s$%s, na.rm = TRUE)", 
                      func_name, target_col, df_name, func_name, df_name, target_col))
      } else if (func_name == "sd" || func_name == "var") {
        return(sprintf("# Calculate %s (variability) of '%s' in your '%s' data frame\n%s(%s$%s, na.rm = TRUE)", 
                      func_name, target_col, df_name, func_name, df_name, target_col))
      } else if (func_name == "min" || func_name == "max") {
        return(sprintf("# Find %s value of '%s' in your '%s' data frame\n%s(%s$%s, na.rm = TRUE)", 
                      func_name, target_col, df_name, func_name, df_name, target_col))
      } else if (func_name == "range") {
        return(sprintf("# Find range of values for '%s' in your '%s' data frame\n%s(%s$%s, na.rm = TRUE)", 
                      target_col, df_name, func_name, df_name, target_col))
      } else if (func_name == "sum") {
        return(sprintf("# Calculate sum of '%s' in your '%s' data frame\n%s(%s$%s, na.rm = TRUE)", 
                      target_col, df_name, func_name, df_name, target_col))
      } else if (func_name == "summary") {
        # Summary is special as it works on entire data frames
        return(sprintf("# Get summary statistics for your '%s' data frame\n%s(%s)", 
                      df_name, func_name, df_name))
      }
      
      # Generic fallback
      return(sprintf("# Calculate %s from your '%s' data frame\n%s(%s$%s, na.rm = TRUE)", 
                    func_name, df_name, func_name, df_name, target_col))
    },
    
    #' @description Generate example for statistical modeling functions
    #' @param func_name Name of the function
    #' @param df_name Name of the data frame
    #' @param df_info Information about the data frame
    #' @return Example string
    #' @description Generate example for tidyr functions
    #' @param func_name Name of the function
    #' @param df_name Name of the data frame
    #' @param df_info Information about the data frame
    #' @return Example string
    generate_tidyr_example = function(func_name, df_name, df_info) {
      # Get column names and types
      col_names <- df_info$column_names
      col_types <- df_info$column_types
      
      if (length(col_names) == 0) {
        return(sprintf("# Using your data frame '%s'\n%s(%s)", df_name, func_name, df_name))
      }
      
      # For pivot_longer (wide to long format)
      if (func_name == "pivot_longer") {
        # Select a subset of columns to pivot
        col_subset <- head(col_names, min(3, length(col_names)))
        if (length(col_subset) <= 1) {
          col_subset <- col_names  # Use all columns if there are very few
        }
        
        cols_str <- paste(col_subset, collapse = "', '")
        return(sprintf("# Convert your '%s' data frame from wide to long format\n%s |> %s(cols = c('%s'), names_to = 'variable', values_to = 'value')", 
                      df_name, df_name, func_name, cols_str))
      }
      
      # For pivot_wider (long to wide format)
      if (func_name == "pivot_wider") {
        if (length(col_names) >= 2) {
          return(sprintf("# Convert your '%s' data frame from long to wide format\n%s |> %s(names_from = %s, values_from = %s)", 
                        df_name, df_name, func_name, col_names[1], col_names[2]))
        }
      }
      
      # For separate (split a column into multiple columns)
      if (func_name == "separate") {
        if (length(col_names) > 0) {
          # Try to find a character column if types are available
          target_col <- col_names[1]
          if (!is.null(col_types) && length(col_types) > 0) {
            char_cols <- names(col_types)[col_types %in% c("character", "factor")]
            if (length(char_cols) > 0) {
              target_col <- char_cols[1]
            }
          }
          
          return(sprintf("# Split column '%s' in your '%s' data frame\n%s |> %s(%s, into = c('part1', 'part2'), sep = '_')", 
                        target_col, df_name, df_name, func_name, target_col))
        }
      }
      
      # For unite (combine multiple columns into one)
      if (func_name == "unite") {
        if (length(col_names) >= 2) {
          return(sprintf("# Combine columns '%s' and '%s' in your '%s' data frame\n%s |> %s('combined', %s, %s, sep = '_')", 
                        col_names[1], col_names[2], df_name, df_name, func_name, col_names[1], col_names[2]))
        }
      }
      
      # For drop_na (remove rows with missing values)
      if (func_name == "drop_na") {
        if (length(col_names) > 0) {
          return(sprintf("# Remove rows with NA values in your '%s' data frame\n%s |> %s(%s)", 
                        df_name, df_name, func_name, col_names[1]))
        } else {
          return(sprintf("# Remove rows with any NA values in your '%s' data frame\n%s |> %s()", 
                        df_name, df_name, func_name))
        }
      }
      
      # For fill (fill in missing values)
      if (func_name == "fill") {
        if (length(col_names) > 0) {
          return(sprintf("# Fill missing values in your '%s' data frame\n%s |> %s(%s, .direction = 'down')", 
                        df_name, df_name, func_name, col_names[1]))
        }
      }
      
      # Generic fallback
      sprintf("# Apply %s to your '%s' data frame\n%s(%s)", func_name, df_name, func_name, df_name)
    },
    
    #' @description Generate example for purrr functions
    #' @param func_name Name of the function
    #' @param df_name Name of the data frame
    #' @param df_info Information about the data frame
    #' @return Example string
    generate_purrr_example = function(func_name, df_name, df_info) {
      # Get column names and types
      col_names <- df_info$column_names
      col_types <- df_info$column_types
      
      if (length(col_names) == 0) {
        return(sprintf("# Using your data frame '%s' with purrr\n%s(list(), identity)", func_name, func_name))
      }
      
      # Find numeric columns if column types are available
      numeric_cols <- character(0)
      
      if (!is.null(col_types) && length(col_types) > 0) {
        for (i in seq_along(col_types)) {
          col <- names(col_types)[i]
          type <- col_types[i]
          
          if (type %in% c("numeric", "integer", "double")) {
            numeric_cols <- c(numeric_cols, col)
          }
        }
      }
      
      # For basic map functions
      if (func_name == "map") {
        if (length(col_names) > 0) {
          return(sprintf("# Apply a function to each column in your '%s' data frame\n%s |> %s(summary)", 
                        df_name, df_name, func_name))
        }
      }
      
      # For typed map functions (map_dbl, map_int, etc.)
      if (func_name %in% c("map_dbl", "map_int")) {
        if (length(numeric_cols) > 0) {
          return(sprintf("# Apply a function to numeric columns in your '%s' data frame\n%s |> select(%s) |> %s(mean, na.rm = TRUE)", 
                        df_name, df_name, paste(numeric_cols, collapse = ", "), func_name))
        } else if (length(col_names) > 0) {
          return(sprintf("# Get a numeric result from each column in your '%s' data frame\n%s |> %s(~ length(unique(.x)))", 
                        df_name, df_name, func_name))
        }
      }
      
      # For map_chr
      if (func_name == "map_chr") {
        return(sprintf("# Convert each column to a string representation in your '%s' data frame\n%s |> %s(~ paste(head(.x, 3), collapse = ', '))", 
                      df_name, df_name, func_name))
      }
      
      # For map_lgl
      if (func_name == "map_lgl") {
        return(sprintf("# Check a condition for each column in your '%s' data frame\n%s |> %s(~ any(is.na(.x)))", 
                      df_name, df_name, func_name))
      }
      
      # For map_df
      if (func_name == "map_df") {
        if (length(col_names) > 0) {
          return(sprintf("# Create a data frame from applying a function in your '%s' data frame\n%s |> %s(~ data.frame(n = length(.x), mean = mean(.x, na.rm = TRUE)))", 
                        df_name, df_name, func_name))
        }
      }
      
      # For keep/discard
      if (func_name %in% c("keep", "discard")) {
        comparison <- if (func_name == "keep") "TRUE" else "FALSE"
        return(sprintf("# %s columns meeting a condition in your '%s' data frame\n%s |> %s(is.numeric)", 
                      ifelse(func_name == "keep", "Keep", "Discard"), df_name, df_name, func_name))
      }
      
      # For reduce
      if (func_name == "reduce") {
        if (length(numeric_cols) >= 2) {
          num_cols <- paste(numeric_cols, collapse = ", ")
          return(sprintf("# Combine numeric columns with a function in your '%s' data frame\n%s |> select(%s) |> %s(`+`)", 
                        df_name, df_name, num_cols, func_name))
        } else {
          return(sprintf("# Combine values using a function in your '%s' data frame\n%s |> pull(%s) |> %s(`+`)", 
                        df_name, df_name, col_names[1], func_name))
        }
      }
      
      # Generic fallback
      sprintf("# Apply purrr::%s to your '%s' data frame\n%s |> %s(~ .x)", func_name, df_name, df_name, func_name)
    },
    
    #' @description Generate example for survival analysis functions
    #' @param func_name Name of the function
    #' @param df_name Name of the data frame
    #' @param df_info Information about the data frame
    #' @return Example string
    generate_survival_example = function(func_name, df_name, df_info) {
      # Get column names and types
      col_names <- df_info$column_names
      col_types <- df_info$column_types
      
      if (length(col_names) < 2) {
        return(sprintf("# Apply survival::%s to your data\n# Typically needs time and event status variables\nlibrary(survival)\n%s", func_name, func_name))
      }
      
      # Find numeric and logical columns if column types are available
      numeric_cols <- character(0)
      logical_cols <- character(0)
      
      if (!is.null(col_types) && length(col_types) > 0) {
        for (i in seq_along(col_types)) {
          col <- names(col_types)[i]
          type <- col_types[i]
          
          if (type %in% c("numeric", "integer", "double")) {
            numeric_cols <- c(numeric_cols, col)
          } else if (type == "logical") {
            logical_cols <- c(logical_cols, col)
          }
        }
      }
      
      # For Surv objects
      if (func_name == "Surv") {
        # We need at least one time variable and ideally a status indicator
        time_var <- if (length(numeric_cols) > 0) numeric_cols[1] else col_names[1]
        status_var <- if (length(logical_cols) > 0) logical_cols[1] else 
                      if (length(numeric_cols) > 1) numeric_cols[2] else 
                      if (length(col_names) > 1) col_names[2] else "status"
        
        return(sprintf("# Create a survival object using your '%s' data frame\nlibrary(survival)\n%s_surv <- with(%s, %s(%s, %s))", 
                      df_name, df_name, df_name, func_name, time_var, status_var))
      }
      
      # For survfit
      if (func_name == "survfit") {
        time_var <- if (length(numeric_cols) > 0) numeric_cols[1] else col_names[1]
        status_var <- if (length(logical_cols) > 0) logical_cols[1] else 
                      if (length(numeric_cols) > 1) numeric_cols[2] else 
                      if (length(col_names) > 1) col_names[2] else "status"
        
        return(sprintf("# Fit a survival curve using your '%s' data frame\nlibrary(survival)\nfit <- %s(Surv(%s, %s) ~ 1, data = %s)\nplot(fit)", 
                      df_name, func_name, time_var, status_var, df_name))
      }
      
      # For coxph
      if (func_name == "coxph") {
        time_var <- if (length(numeric_cols) > 0) numeric_cols[1] else col_names[1]
        status_var <- if (length(logical_cols) > 0) logical_cols[1] else 
                      if (length(numeric_cols) > 1) numeric_cols[2] else 
                      if (length(col_names) > 1) col_names[2] else "status"
        
        predictor <- if (length(col_names) > 2) col_names[3] else "predictor"
        
        return(sprintf("# Fit a Cox proportional hazards model using your '%s' data frame\nlibrary(survival)\nmodel <- %s(Surv(%s, %s) ~ %s, data = %s)\nsummary(model)", 
                      df_name, func_name, time_var, status_var, predictor, df_name))
      }
      
      # Generic fallback
      sprintf("# Apply survival::%s to your '%s' data frame\nlibrary(survival)\n# Requires appropriate survival data\n", 
              func_name, df_name)
    },
    
    #' @description Generate example for statistical modeling functions
    #' @param func_name Name of the function
    #' @param df_name Name of the data frame
    #' @param df_info Information about the data frame
    #' @return Example string
    generate_stats_model_example = function(func_name, df_name, df_info) {
      # Get column names and types
      col_names <- df_info$column_names
      col_types <- df_info$column_types
      
      if (length(col_names) < 2) {
        return(sprintf("# Fit a model to your '%s' data frame\n%s(y ~ x, data = %s)", 
                      df_name, func_name, df_name))
      }
      
      # Find numeric and categorical columns if column types are available
      numeric_cols <- character(0)
      cat_cols <- character(0)
      date_cols <- character(0)
      logical_cols <- character(0)
      
      if (!is.null(col_types) && length(col_types) > 0) {
        for (i in seq_along(col_types)) {
          col <- names(col_types)[i]
          type <- col_types[i]
          
          if (type %in% c("numeric", "integer", "double")) {
            numeric_cols <- c(numeric_cols, col)
          } else if (type %in% c("character", "factor")) {
            cat_cols <- c(cat_cols, col)
          } else if (type %in% c("Date", "POSIXct", "POSIXlt")) {
            date_cols <- c(date_cols, col)
          } else if (type == "logical") {
            logical_cols <- c(logical_cols, col)
          }
        }
      }
      
      # For linear models
      if (func_name == "lm") {
        # If we have numeric predictor and outcome, create a regression
        if (length(numeric_cols) >= 2) {
          return(sprintf("# Fit a linear model to your '%s' data frame\n%s(%s ~ %s, data = %s)", 
                        df_name, func_name, numeric_cols[1], numeric_cols[2], df_name))
        } else if (length(numeric_cols) == 1 && length(cat_cols) >= 1) {
          # Numeric outcome with categorical predictor - ANOVA-like model
          return(sprintf("# Fit a linear model (ANOVA-like) to your '%s' data frame\n%s(%s ~ %s, data = %s)", 
                        df_name, func_name, numeric_cols[1], cat_cols[1], df_name))
        } else {
          return(sprintf("# Fit a linear model to your '%s' data frame\n%s(%s ~ %s, data = %s)", 
                        df_name, func_name, col_names[2], col_names[1], df_name))
        }
      }
      
      # For generalized linear models
      if (func_name == "glm") {
        # If we have a logical/binary outcome, use logistic regression
        if (length(logical_cols) > 0 && length(numeric_cols) > 0) {
          return(sprintf("# Fit a logistic regression model to your '%s' data frame\n%s(%s ~ %s, data = %s, family = binomial())", 
                        df_name, func_name, logical_cols[1], numeric_cols[1], df_name))
        } else if (length(numeric_cols) >= 2) {
          return(sprintf("# Fit a regression model to your '%s' data frame\n%s(%s ~ %s, data = %s, family = gaussian())", 
                        df_name, func_name, numeric_cols[1], numeric_cols[2], df_name))
        } else if (length(cat_cols) > 0 && length(numeric_cols) > 0) {
          return(sprintf("# Fit a model with categorical predictor to your '%s' data frame\n%s(%s ~ %s, data = %s, family = gaussian())", 
                        df_name, func_name, numeric_cols[1], cat_cols[1], df_name))
        } else {
          return(sprintf("# Fit a logistic regression model to your '%s' data frame\n%s(%s ~ %s, data = %s, family = binomial())", 
                        df_name, func_name, col_names[2], col_names[1], df_name))
        }
      }
      
      # For t-tests
      if (func_name == "t.test") {
        if (length(numeric_cols) >= 2) {
          return(sprintf("# Perform a t-test comparing two numeric variables in your '%s' data frame\n%s(%s$%s, %s$%s, paired = FALSE)", 
                        df_name, func_name, df_name, numeric_cols[1], df_name, numeric_cols[2]))
        } else if (length(numeric_cols) == 1 && length(cat_cols) >= 1) {
          return(sprintf("# Perform a t-test comparing '%s' across groups in '%s'\n%s(%s ~ %s, data = %s)", 
                        numeric_cols[1], cat_cols[1], func_name, numeric_cols[1], cat_cols[1], df_name))
        } else {
          return(sprintf("# Perform a t-test on your '%s' data frame\n%s(%s$%s, %s$%s)", 
                        df_name, func_name, df_name, col_names[1], df_name, col_names[2]))
        }
      }
      
      # For correlation tests
      if (func_name %in% c("cor", "cor.test")) {
        if (length(numeric_cols) >= 2) {
          if (func_name == "cor") {
            # For cor(), we can use multiple columns
            if (length(numeric_cols) > 2) {
              num_cols_str <- paste(numeric_cols, collapse = ", ")
              return(sprintf("# Calculate correlation matrix between numeric columns in your '%s' data frame\n%s(%s[, c(\"%s\")], use = 'complete.obs')", 
                            df_name, func_name, df_name, paste(numeric_cols, collapse = "\", \"")))
            }
          }
          return(sprintf("# Calculate correlation between '%s' and '%s' in your '%s' data frame\n%s(%s$%s, %s$%s, use = 'complete.obs'%s)", 
                        numeric_cols[1], numeric_cols[2], df_name, func_name, df_name, numeric_cols[1], 
                        df_name, numeric_cols[2], if(func_name == "cor.test") ", method = 'pearson'" else ""))
        } else {
          return(sprintf("# Calculate correlation between columns in your '%s' data frame\n%s(%s$%s, %s$%s, use = 'complete.obs')", 
                        df_name, func_name, df_name, col_names[1], df_name, col_names[2]))
        }
      }
      
      # For ANOVA
      if (func_name == "aov") {
        if (length(numeric_cols) >= 1 && length(cat_cols) >= 1) {
          return(sprintf("# Perform ANOVA on '%s' across groups in '%s'\n%s(%s ~ %s, data = %s)", 
                        numeric_cols[1], cat_cols[1], func_name, numeric_cols[1], cat_cols[1], df_name))
        } else {
          return(sprintf("# Perform ANOVA on your '%s' data frame\n%s(%s ~ %s, data = %s)", 
                        df_name, func_name, col_names[2], col_names[1], df_name))
        }
      }
      
      # For survival analysis
      if (func_name %in% c("survfit", "coxph", "Surv")) {
        if (func_name == "Surv") {
          if (length(numeric_cols) >= 2) {
            return(sprintf("# Create survival object from your '%s' data frame\n%s(%s$%s, %s$%s)", 
                          df_name, func_name, df_name, numeric_cols[1], df_name, 
                          if(length(logical_cols) > 0) logical_cols[1] else numeric_cols[2]))
          }
        } else {
          return(sprintf("# Fit a survival model to your '%s' data frame\n%s(survival::Surv(time, status) ~ %s, data = %s)", 
                        df_name, func_name, if(length(numeric_cols) > 0) numeric_cols[1] else col_names[1], df_name))
        }
      }
      
      # Generic fallback
      sprintf("# Apply %s to your '%s' data frame\n%s(formula, data = %s)", 
              func_name, df_name, func_name, df_name)
    },
    
    #' @description Generate example for data.table functions
    #' @param func_name Name of the function
    #' @param df_name Name of the data frame
    #' @param df_info Information about the data frame
    #' @return Example string
    generate_data_table_example = function(func_name, df_name, df_info) {
      # Get column names and types
      col_names <- df_info$column_names
      col_types <- df_info$column_types
      
      if (length(col_names) == 0) {
        return(sprintf("# Using your data frame '%s' with data.table\nlibrary(data.table)\ndt <- as.data.table(%s)", df_name, df_name))
      }
      
      # Find numeric, character and date columns if column types are available
      numeric_cols <- character(0)
      char_cols <- character(0)
      date_cols <- character(0)
      logical_cols <- character(0)
      
      if (!is.null(col_types) && length(col_types) > 0) {
        for (i in seq_along(col_types)) {
          col <- names(col_types)[i]
          type <- col_types[i]
          
          if (type %in% c("numeric", "integer", "double")) {
            numeric_cols <- c(numeric_cols, col)
          } else if (type %in% c("character", "factor")) {
            char_cols <- c(char_cols, col)
          } else if (type %in% c("Date", "POSIXct", "POSIXlt")) {
            date_cols <- c(date_cols, col)
          } else if (type == "logical") {
            logical_cols <- c(logical_cols, col)
          }
        }
      }
      
      # For data.table's special [ function (the core of data.table)
      if (func_name == "[") {
        # Basic filtering with i
        if (length(char_cols) > 0) {
          # Character column for grouping
          return(sprintf("# Query your '%s' data frame using data.table syntax\nlibrary(data.table)\ndt <- as.data.table(%s)\ndt[%s == \"value\"]  # Filter rows\ndt[, .(%s = mean(%s)), by = %s]  # Aggregate by group", 
                        df_name, df_name, char_cols[1], 
                        if(length(numeric_cols) > 0) paste0("avg_", numeric_cols[1]) else "count", 
                        if(length(numeric_cols) > 0) numeric_cols[1] else ".N", 
                        char_cols[1]))
        } else if (length(numeric_cols) > 0) {
          # Numeric column for filtering
          return(sprintf("# Query your '%s' data frame using data.table syntax\nlibrary(data.table)\ndt <- as.data.table(%s)\ndt[%s > mean(%s)]  # Filter rows\ndt[, .(count = .N, mean_val = mean(%s))]  # Aggregate", 
                        df_name, df_name, numeric_cols[1], numeric_cols[1], numeric_cols[1]))
        } else {
          # Generic example
          return(sprintf("# Query your '%s' data frame using data.table syntax\nlibrary(data.table)\ndt <- as.data.table(%s)\ndt[1:5]  # First 5 rows\ndt[, .(count = .N)]  # Count rows", 
                        df_name, df_name))
        }
      }
      
      # For data.table() constructor
      if (func_name == "data.table") {
        col_subset <- head(col_names, min(3, length(col_names)))
        return(sprintf("# Create a data.table from your '%s' data frame\nlibrary(data.table)\ndt <- data.table(%s)", 
                      df_name, paste(sprintf("%s = %s$%s", col_subset, df_name, col_subset), collapse = ", ")))
      }
      
      # For setkey
      if (func_name == "setkey") {
        key_col <- if (length(char_cols) > 0) char_cols[1] else col_names[1]
        return(sprintf("# Set a key on your '%s' data table for faster operations\nlibrary(data.table)\ndt <- as.data.table(%s)\nsetkey(dt, %s)  # Now dt[\"value\"] will be fast", 
                      df_name, df_name, key_col))
      }
      
      # For setDT
      if (func_name == "setDT") {
        return(sprintf("# Convert your '%s' data frame to a data.table in place\nlibrary(data.table)\n%s_copy <- %s  # Create a copy to work with\nsetDT(%s_copy)  # Now %s_copy is a data.table", 
                      df_name, df_name, df_name, df_name, df_name))
      }
      
      # For setDF
      if (func_name == "setDF") {
        return(sprintf("# Convert your '%s' data.table to a data.frame in place\nlibrary(data.table)\ndt <- as.data.table(%s)\nsetDF(dt)  # Now dt is a data.frame again", 
                      df_name, df_name))
      }
      
      # For fread
      if (func_name == "fread") {
        return(sprintf("# Read a CSV file quickly into a data.table (similar to your '%s')\nlibrary(data.table)\n# Assuming you have a file 'data.csv' with similar structure\nnew_dt <- fread(\"data.csv\")\n# Compare with your existing data\nstr(%s)\nstr(new_dt)", 
                      df_name, df_name))
      }
      
      # For fwrite
      if (func_name == "fwrite") {
        return(sprintf("# Write your '%s' data frame to a CSV file quickly\nlibrary(data.table)\ndt <- as.data.table(%s)\nfwrite(dt, \"output_data.csv\")", 
                      df_name, df_name))
      }
      
      # For melt (reshape from wide to long)
      if (func_name == "melt") {
        if (length(col_names) >= 3) {
          id_vars <- col_names[1]
          measure_vars <- paste(col_names[2:min(4, length(col_names))], collapse = "\", \"")
          return(sprintf("# Reshape your '%s' data frame from wide to long format\nlibrary(data.table)\ndt <- as.data.table(%s)\nlong_dt <- melt(dt, id.vars = \"%s\", measure.vars = c(\"%s\"))", 
                        df_name, df_name, id_vars, measure_vars))
        } else {
          return(sprintf("# Reshape your '%s' data frame from wide to long format\nlibrary(data.table)\ndt <- as.data.table(%s)\n# Assuming first column is the ID variable\nlong_dt <- melt(dt, id.vars = 1, measure.vars = 2:ncol(dt))", 
                        df_name, df_name))
        }
      }
      
      # For dcast (reshape from long to wide)
      if (func_name == "dcast") {
        if (length(col_names) >= 3) {
          formula <- sprintf("%s ~ %s", col_names[1], col_names[2])
          value_var <- col_names[3]
          return(sprintf("# Reshape your '%s' data frame from long to wide format\nlibrary(data.table)\ndt <- as.data.table(%s)\nwide_dt <- dcast(dt, %s, value.var = \"%s\")", 
                        df_name, df_name, formula, value_var))
        } else {
          return(sprintf("# Reshape your '%s' data frame from long to wide format\nlibrary(data.table)\ndt <- as.data.table(%s)\n# Assuming format needs to be row_id ~ variable\nwide_dt <- dcast(dt, %s ~ variable, value.var = \"value\")", 
                        df_name, df_name, col_names[1]))
        }
      }
      
      # For rbindlist
      if (func_name == "rbindlist") {
        return(sprintf("# Combine multiple data frames like your '%s' efficiently\nlibrary(data.table)\n# Create a list of similar data frames\ndf_list <- list(%s, %s[1:5,], %s[6:10,])\n# Combine them\ncombined_dt <- rbindlist(df_list)", 
                      df_name, df_name, df_name, df_name))
      }
      
      # Generic fallback
      sprintf("# Apply data.table::%s to your '%s' data frame\nlibrary(data.table)\ndt <- as.data.table(%s)\n%s(dt)", 
              func_name, df_name, df_name, func_name)
    },
    
    #' @description Generate example for machine learning functions
    #' @param func_name Name of the function
    #' @param df_name Name of the data frame
    #' @param df_info Information about the data frame
    #' @return Example string
    generate_ml_example = function(func_name, df_name, df_info) {
      # Get column names and types
      col_names <- df_info$column_names
      col_types <- df_info$column_types
      
      if (length(col_names) == 0) {
        return(sprintf("# Using your data frame '%s' for machine learning\nlibrary(caret)\n# More details needed to create specific ML example", df_name))
      }
      
      # Find numeric, categorical, and logical columns
      numeric_cols <- character(0)
      cat_cols <- character(0)
      logical_cols <- character(0)
      
      if (!is.null(col_types) && length(col_types) > 0) {
        for (i in seq_along(col_types)) {
          col <- names(col_types)[i]
          type <- col_types[i]
          
          if (type %in% c("numeric", "integer", "double")) {
            numeric_cols <- c(numeric_cols, col)
          } else if (type %in% c("character", "factor")) {
            cat_cols <- c(cat_cols, col)
          } else if (type == "logical") {
            logical_cols <- c(logical_cols, col)
          }
        }
      }
      
      # For caret functions
      if (func_name %in% c("train", "trainControl", "createDataPartition", "createFolds")) {
        # For train function (core of caret)
        if (func_name == "train") {
          # Determine possible target variable
          # Prefer logical/categorical for classification, numeric for regression
          target_var <- NULL
          
          if (length(logical_cols) > 0) {
            target_var <- logical_cols[1]  # Logical is ideal for binary classification
            model_type <- "classification"
          } else if (length(cat_cols) > 0) {
            target_var <- cat_cols[1]      # Categorical for multi-class classification
            model_type <- "classification"
          } else if (length(numeric_cols) > 0) {
            target_var <- numeric_cols[1]  # Numeric for regression
            model_type <- "regression"
          } else {
            target_var <- col_names[1]     # Fallback to first column
            model_type <- "unknown"
          }
          
          # Get predictors - exclude the target variable
          predictors <- setdiff(col_names, target_var)
          if (length(predictors) > 4) {
            predictors <- predictors[1:4]  # Take first 4 predictors for example
          }
          
          # Build example based on model type
          if (model_type == "classification") {
            return(sprintf("# Train a classification model using your '%s' data\nlibrary(caret)\n\n# Create training/testing split\nset.seed(123)\ntrain_idx <- createDataPartition(%s$%s, p = 0.7, list = FALSE)\ntrain_data <- %s[train_idx, ]\ntest_data <- %s[-train_idx, ]\n\n# Train model\nmodel <- train(\n  %s ~ %s,\n  data = train_data,\n  method = \"rf\",  # Random Forest\n  trControl = trainControl(method = \"cv\", number = 5)\n)\n\n# Evaluate model\npredictions <- predict(model, test_data)\nconfusionMatrix(predictions, test_data$%s)",
                          df_name, df_name, target_var, df_name, df_name, target_var, paste(predictors, collapse = " + "), target_var))
          } else {
            return(sprintf("# Train a regression model using your '%s' data\nlibrary(caret)\n\n# Create training/testing split\nset.seed(123)\ntrain_idx <- createDataPartition(%s$%s, p = 0.7, list = FALSE)\ntrain_data <- %s[train_idx, ]\ntest_data <- %s[-train_idx, ]\n\n# Train model\nmodel <- train(\n  %s ~ %s,\n  data = train_data,\n  method = \"lm\",  # Linear model\n  trControl = trainControl(method = \"cv\", number = 5)\n)\n\n# Evaluate model\npredictions <- predict(model, test_data)\nRMSE <- sqrt(mean((predictions - test_data$%s)^2))\nR2 <- cor(predictions, test_data$%s)^2\ncat(\"RMSE:\", RMSE, \"\\nR-squared:\", R2)",
                          df_name, df_name, target_var, df_name, df_name, target_var, paste(predictors, collapse = " + "), target_var, target_var))
          }
        }
        
        # For createDataPartition
        if (func_name == "createDataPartition") {
          # Pick a logical or categorical column for stratified sampling if available
          target_var <- if (length(logical_cols) > 0) logical_cols[1] else 
                       if (length(cat_cols) > 0) cat_cols[1] else col_names[1]
          
          return(sprintf("# Create a stratified train/test split using your '%s' data\nlibrary(caret)\n\n# Create indices for 70%% training, 30%% testing\nset.seed(123) # For reproducibility\ntrain_idx <- createDataPartition(%s$%s, p = 0.7, list = FALSE)\n\n# Create the train/test datasets\ntrain_data <- %s[train_idx, ]\ntest_data <- %s[-train_idx, ]", 
                        df_name, df_name, target_var, df_name, df_name))
        }
        
        # For createFolds
        if (func_name == "createFolds") {
          # Pick a logical or categorical column for stratified folds if available
          target_var <- if (length(logical_cols) > 0) logical_cols[1] else 
                       if (length(cat_cols) > 0) cat_cols[1] else col_names[1]
          
          return(sprintf("# Create cross-validation folds using your '%s' data\nlibrary(caret)\n\n# Create 5 stratified folds\nset.seed(123) # For reproducibility\nfolds <- createFolds(%s$%s, k = 5, list = TRUE, returnTrain = FALSE)\n\n# Example usage in cross-validation loop\nresults <- list()\nfor (i in 1:length(folds)) {\n  # Get the training and test data for this fold\n  test_idx <- folds[[i]]\n  train_idx <- setdiff(1:nrow(%s), test_idx)\n  \n  # Use indices for model training and evaluation\n  # train_data <- %s[train_idx, ]\n  # test_data <- %s[test_idx, ]\n}", 
                        df_name, df_name, target_var, df_name, df_name, df_name))
        }
        
        # For trainControl
        if (func_name == "trainControl") {
          return(sprintf("# Configure training parameters for your '%s' data\nlibrary(caret)\n\n# Basic cross-validation control\nctrl <- trainControl(\n  method = \"cv\",       # Cross-validation\n  number = 5,          # 5 folds\n  classProbs = TRUE,   # Calculate class probabilities\n  summaryFunction = defaultSummary,\n  verboseIter = TRUE   # Print progress\n)\n\n# Use in train function\n# model <- train(\n#   target ~ predictors,\n#   data = %s,\n#   method = \"rf\",\n#   trControl = ctrl\n# )", 
                        df_name, df_name))
        }
      }
      
      # For randomForest functions
      if (func_name %in% c("randomForest", "importance", "varImpPlot")) {
        # For the main randomForest function
        if (func_name == "randomForest") {
          # Determine possible target variable
          # Prefer logical/categorical for classification, numeric for regression
          target_var <- NULL
          
          if (length(logical_cols) > 0) {
            target_var <- logical_cols[1]  # Logical is ideal for binary classification
            model_type <- "classification"
          } else if (length(cat_cols) > 0) {
            target_var <- cat_cols[1]      # Categorical for multi-class classification
            model_type <- "classification"
          } else if (length(numeric_cols) > 0) {
            target_var <- numeric_cols[1]  # Numeric for regression
            model_type <- "regression"
          } else {
            target_var <- col_names[1]     # Fallback to first column
            model_type <- "unknown"
          }
          
          # Get predictors - exclude the target variable
          predictors <- setdiff(col_names, target_var)
          predictor_subset <- if (length(predictors) > 0) paste0(df_name, "[, c(\"", paste(predictors, collapse = "\", \""), "\")]") else paste0(df_name, "[, -which(names(", df_name, ") == \"", target_var, "\")]")
          
          # Build example based on model type
          if (model_type == "classification") {
            return(sprintf("# Train a Random Forest classification model using your '%s' data\nlibrary(randomForest)\n\n# Prepare data\nset.seed(123)\n# Create a 70%% training, 30%% testing split\ntrain_idx <- sample(1:nrow(%s), 0.7 * nrow(%s))\ntrain_data <- %s[train_idx, ]\ntest_data <- %s[-train_idx, ]\n\n# Train model\nrf_model <- randomForest(\n  x = %s,\n  y = %s$%s,\n  ntree = 500,\n  importance = TRUE\n)\n\n# Evaluate model\npredictions <- predict(rf_model, test_data)\nconfusion_matrix <- table(predictions, test_data$%s)\nprint(confusion_matrix)\naccuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)\ncat(\"Accuracy:\", round(accuracy, 4))",
                          df_name, df_name, df_name, df_name, df_name, predictor_subset, df_name, target_var, target_var))
          } else {
            return(sprintf("# Train a Random Forest regression model using your '%s' data\nlibrary(randomForest)\n\n# Prepare data\nset.seed(123)\n# Create a 70%% training, 30%% testing split\ntrain_idx <- sample(1:nrow(%s), 0.7 * nrow(%s))\ntrain_data <- %s[train_idx, ]\ntest_data <- %s[-train_idx, ]\n\n# Train model\nrf_model <- randomForest(\n  x = %s,\n  y = %s$%s,\n  ntree = 500,\n  importance = TRUE\n)\n\n# Evaluate model\npredictions <- predict(rf_model, test_data)\nRMSE <- sqrt(mean((predictions - test_data$%s)^2))\nR2 <- 1 - sum((test_data$%s - predictions)^2) / sum((test_data$%s - mean(test_data$%s))^2)\ncat(\"RMSE:\", round(RMSE, 4), \"\\nR-squared:\", round(R2, 4))",
                          df_name, df_name, df_name, df_name, df_name, predictor_subset, df_name, target_var, target_var, target_var, target_var, target_var))
          }
        }
        
        # For importance
        if (func_name == "importance") {
          return(sprintf("# Get variable importance from Random Forest model using your '%s' data\nlibrary(randomForest)\n\n# Assuming you've already created a Random Forest model\n# rf_model <- randomForest(%s$target ~ ., data = %s, importance = TRUE)\n\n# Get variable importance\nimp <- importance(rf_model)\nprint(imp)\n\n# Sort variables by importance\nimp_sorted <- imp[order(imp[, \"%%IncMSE\"], decreasing = TRUE), ]\nprint(imp_sorted)", 
                        df_name, df_name, df_name))
        }
        
        # For varImpPlot
        if (func_name == "varImpPlot") {
          return(sprintf("# Plot variable importance from Random Forest model using your '%s' data\nlibrary(randomForest)\n\n# Assuming you've already created a Random Forest model\n# rf_model <- randomForest(%s$target ~ ., data = %s, importance = TRUE)\n\n# Plot variable importance\nvarImpPlot(rf_model, sort = TRUE, main = \"Variable Importance for %s\")", 
                        df_name, df_name, df_name, df_name))
        }
      }
      
      # For rpart (decision trees)
      if (func_name %in% c("rpart", "rpart.plot", "prune")) {
        # For rpart main function
        if (func_name == "rpart") {
          # Determine possible target variable
          target_var <- NULL
          
          if (length(logical_cols) > 0) {
            target_var <- logical_cols[1]  # Logical is ideal for binary classification
            model_type <- "classification"
          } else if (length(cat_cols) > 0) {
            target_var <- cat_cols[1]      # Categorical for multi-class classification
            model_type <- "classification"
          } else if (length(numeric_cols) > 0) {
            target_var <- numeric_cols[1]  # Numeric for regression
            model_type <- "regression"
          } else {
            target_var <- col_names[1]     # Fallback to first column
            model_type <- "unknown"
          }
          
          # Get predictors - exclude the target variable
          predictors <- setdiff(col_names, target_var)
          if (length(predictors) > 4) {
            predictors <- predictors[1:4]  # Take first 4 predictors for example
          }
          predictors_formula <- paste(predictors, collapse = " + ")
          
          # Build example
          return(sprintf("# Create a decision tree model using your '%s' data\nlibrary(rpart)\nlibrary(rpart.plot)  # For visualization\n\n# Build decision tree model\ntree_model <- rpart(\n  %s ~ %s,\n  data = %s,\n  method = \"%s\",\n  control = rpart.control(cp = 0.01)  # Complexity parameter\n)\n\n# Visualize the tree\nrpart.plot(tree_model, extra = 104, box.palette = \"RdBu\")\n\n# Make predictions\npredictions <- predict(tree_model, %s)\n\n# For classification, get class predictions\n# predictions <- predict(tree_model, %s, type = \"class\")",
                        df_name, target_var, predictors_formula, df_name, 
                        ifelse(model_type == "classification", "class", "anova"),
                        df_name, df_name))
        }
        
        # For prune
        if (func_name == "prune") {
          return(sprintf("# Prune a decision tree model using your '%s' data\nlibrary(rpart)\nlibrary(rpart.plot)\n\n# Assuming you've built a decision tree\n# tree_model <- rpart(target ~ predictors, data = %s)\n\n# Plot complexity parameter (CP) vs error\nprintcp(tree_model)\nplotcp(tree_model)\n\n# Find the optimal CP value\nopt_cp <- tree_model$cptable[which.min(tree_model$cptable[,\"xerror\"]),\"CP\"]\n\n# Prune the tree using the optimal CP\npruned_tree <- prune(tree_model, cp = opt_cp)\n\n# Visualize the pruned tree\nrpart.plot(pruned_tree, extra = 104, box.palette = \"RdBu\")", 
                        df_name, df_name))
        }
      }
      
      # For glmnet
      if (func_name %in% c("glmnet", "cv.glmnet")) {
        # For glmnet main function
        if (func_name == "glmnet") {
          # Select numeric predictors
          if (length(numeric_cols) >= 2) {
            target_var <- numeric_cols[1]  # First numeric as target
            predictor_cols <- numeric_cols[-1]  # Rest as predictors
            
            return(sprintf("# Create a regularized regression model using your '%s' data\nlibrary(glmnet)\n\n# Prepare data matrix for glmnet\nx <- as.matrix(%s[, c(\"%s\")])\ny <- %s$%s\n\n# Train models with different regularization penalties\n# alpha=1 for LASSO, alpha=0 for Ridge, alpha=0.5 for Elastic Net\nlasso_model <- glmnet(x, y, alpha = 1)\nridge_model <- glmnet(x, y, alpha = 0)\n\n# Plot coefficient paths\npar(mfrow = c(1, 2))\nplot(lasso_model, main = \"LASSO\")\nplot(ridge_model, main = \"Ridge\")\n\n# For finding optimal lambda, use cv.glmnet\ncv_model <- cv.glmnet(x, y, alpha = 1)\nbest_lambda <- cv_model$lambda.min\n\n# Get coefficients at optimal lambda\ncoefs <- coef(cv_model, s = best_lambda)\nprint(coefs)",
                          df_name, df_name, paste(predictor_cols, collapse = "\", \""), df_name, target_var))
          } else {
            return(sprintf("# Create a regularized model using your '%s' data\nlibrary(glmnet)\n\n# Prepare data matrix for glmnet\n# Need numeric predictors and outcome\nx <- as.matrix(%s[, -which(names(%s) == \"target\")])\ny <- %s$target\n\n# Train models with different regularization\nlasso_model <- glmnet(x, y, alpha = 1)  # LASSO\nridge_model <- glmnet(x, y, alpha = 0)  # Ridge\n\n# Plot coefficient paths\npar(mfrow = c(1, 2))\nplot(lasso_model, main = \"LASSO\")\nplot(ridge_model, main = \"Ridge\")",
                          df_name, df_name, df_name, df_name))
          }
        }
        
        # For cv.glmnet
        if (func_name == "cv.glmnet") {
          # Select numeric predictors
          if (length(numeric_cols) >= 2) {
            target_var <- numeric_cols[1]  # First numeric as target
            predictor_cols <- numeric_cols[-1]  # Rest as predictors
            
            return(sprintf("# Cross-validated regularized model using your '%s' data\nlibrary(glmnet)\n\n# Prepare data matrix for glmnet\nx <- as.matrix(%s[, c(\"%s\")])\ny <- %s$%s\n\n# Perform cross-validation to find optimal lambda\nset.seed(123)\ncv_model <- cv.glmnet(x, y, alpha = 1)  # alpha=1 for LASSO\n\n# Plot cross-validation results\nplot(cv_model)\n\n# Get best lambda values\nlambda_min <- cv_model$lambda.min  # Lambda that gives minimum CV error\nlambda_1se <- cv_model$lambda.1se  # Largest lambda within 1 standard error\n\n# Get coefficients at optimal lambda\ncoefs_min <- coef(cv_model, s = lambda_min)\ncoefs_1se <- coef(cv_model, s = lambda_1se)\n\ncat(\"Lambda min:\", lambda_min, \"with\", sum(coefs_min != 0), \"non-zero coefficients\\n\")\ncat(\"Lambda 1se:\", lambda_1se, \"with\", sum(coefs_1se != 0), \"non-zero coefficients\\n\")",
                          df_name, df_name, paste(predictor_cols, collapse = "\", \""), df_name, target_var))
          } else {
            return(sprintf("# Cross-validated regularized model using your '%s' data\nlibrary(glmnet)\n\n# Prepare data matrix for glmnet\n# Need numeric predictors and outcome\nx <- as.matrix(%s[, -which(names(%s) == \"target\")])\ny <- %s$target\n\n# Perform cross-validation to find optimal lambda\nset.seed(123)\ncv_model <- cv.glmnet(x, y, alpha = 1)  # alpha=1 for LASSO\n\n# Plot cross-validation results\nplot(cv_model)\n\n# Get best lambda values\nlambda_min <- cv_model$lambda.min  # Lambda that gives minimum CV error\nlambda_1se <- cv_model$lambda.1se  # Largest lambda within 1 standard error",
                          df_name, df_name, df_name, df_name))
          }
        }
      }
      
      # Generic fallback
      sprintf("# Apply machine learning function to your '%s' data\nlibrary(caret)  # Load caret package for ML workflows\n\n# Example ML workflow\n# 1. Split data into training and testing\n# 2. Preprocess data if needed\n# 3. Train model\n# 4. Evaluate performance", df_name)
    },
    
    #' @description Format context data for inclusion in AI prompt
    #' @param func_name Name of the function
    #' @param func_metadata Metadata about the function
    #' @return Formatted context string
    format_context_for_prompt = function(func_name, func_metadata) {
      context_string <- "USER ENVIRONMENT CONTEXT:\n"
      
      # Add active packages
      if (length(self$context_data$active_packages) > 0) {
        context_string <- paste0(context_string, "Active packages: ", 
                               paste(self$context_data$active_packages, collapse = ", "), "\n")
      }
      
      # Add recent functions
      if (length(self$context_data$recent_functions) > 0) {
        context_string <- paste0(context_string, "Recently used functions: ", 
                               paste(head(self$context_data$recent_functions, 5), collapse = ", "), "\n")
      }
      
      # Add data frames info
      if (length(self$context_data$data_frames) > 0) {
        # Score data frames for relevance to this function
        relevance_scores <- self$score_data_frame_relevance(func_name, func_metadata)
        
        # Get top data frames (max 3)
        top_dfs <- names(head(sort(relevance_scores, decreasing = TRUE), 3))
        
        context_string <- paste0(context_string, "Available data frames (most relevant first):\n")
        
        for (df_name in top_dfs) {
          df_info <- self$context_data$data_frames[[df_name]]
          
          # Basic info
          df_context <- sprintf("- %s: %d rows × %d columns", 
                              df_name, df_info$rows, df_info$cols)
          
          # Add column names
          if (length(df_info$column_names) > 0) {
            col_str <- paste(df_info$column_names, collapse = ", ")
            if (nchar(col_str) > 100) {
              # Truncate long column lists
              visible_cols <- df_info$column_names[1:min(5, length(df_info$column_names))]
              col_str <- paste0(paste(visible_cols, collapse = ", "), 
                              ", ... and ", length(df_info$column_names) - 5, " more columns")
            }
            df_context <- paste0(df_context, "\n  Columns: ", col_str)
          }
          
          # Add column types if available
          if (!is.null(df_info$column_types) && length(df_info$column_types) > 0) {
            # Select a few column types to display
            sample_cols <- names(df_info$column_types)[1:min(3, length(df_info$column_types))]
            type_str <- paste(
              sapply(sample_cols, function(col) sprintf("%s: %s", col, df_info$column_types[col])),
              collapse = ", "
            )
            
            if (length(df_info$column_types) > 3) {
              type_str <- paste0(type_str, ", ...")
            }
            
            df_context <- paste0(df_context, "\n  Types: ", type_str)
          }
          
          context_string <- paste0(context_string, df_context, "\n")
        }
      }
      
      # Remove the last newline
      context_string <- substr(context_string, 1, nchar(context_string) - 1)
      
      context_string
    },
    
    #' @description Suggest next logical steps based on context
    #' @param func_name Name of the function
    #' @param func_metadata Metadata about the function
    #' @return Character vector of suggested next steps
    suggest_next_steps = function(func_name, func_metadata) {
      suggestions <- character(0)
      
      # Get package and function info
      func_package <- func_metadata$package
      
      # Suggest based on package and function
      if (func_package == "dplyr") {
        if (func_name %in% c("filter", "select", "mutate")) {
          suggestions <- c(suggestions, 
                         "Next steps: Consider piping the result to other dplyr functions like summarise() or group_by()")
        } else if (func_name == "group_by") {
          suggestions <- c(suggestions, 
                         "Next steps: After grouping, use summarise() to calculate group statistics")
        }
      } else if (func_package == "ggplot2") {
        if (func_name == "ggplot" || grepl("^geom_", func_name)) {
          suggestions <- c(suggestions, 
                         "Next steps: Customize your plot with theme(), labs(), or add more geoms")
        }
      } else if (func_package == "stats" && func_name %in% c("lm", "glm")) {
        suggestions <- c(suggestions, 
                       "Next steps: Examine the model with summary(), plot diagnostics with plot(), or make predictions with predict()")
      }
      
      # Add general workflow suggestions based on recent function usage
      recent_pkgs <- unique(sapply(self$context_data$recent_functions, function(func) {
        find_package(func)
      }))
      
      if ("readr" %in% recent_pkgs || "data.table" %in% recent_pkgs) {
        suggestions <- c(suggestions, 
                       "Workflow hint: After importing data, inspect it with glimpse() or str() before analysis")
      }
      
      suggestions
    }
  ),
  
  private = list(
    # Null-coalescing operator as a private helper
    `%||%` = function(x, y) {
      if (is.null(x)) y else x
    }
  )
)

#' Find the package that a function belongs to
#'
#' @param func_name The name of the function (can include or exclude parentheses)
#'
#' @return Character string with the package name
#' @keywords internal
find_package <- function(func_name) {
  # Remove parentheses if present
  func_name <- sub("\\(.*$", "", func_name)
  
  # Look for the function in all loaded namespaces
  for (pkg in loadedNamespaces()) {
    if (exists(func_name, envir = asNamespace(pkg), inherits = FALSE)) {
      return(pkg)
    }
  }
  
  # Check if it's in base R
  if (exists(func_name, envir = baseenv(), inherits = FALSE)) {
    return("base")
  }
  
  # Default to unknown
  "unknown"
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Configure context awareness settings for tldrAI
#'
#' @param enable_context_awareness Logical indicating whether to enable context awareness
#' @param analyze_data_frames Logical indicating whether to analyze data frames
#' @param analyze_packages Logical indicating whether to analyze active packages
#' @param analyze_history Logical indicating whether to analyze command history
#' @param anonymize_data Logical indicating whether to anonymize data samples
#' @param max_rows_sample Integer specifying maximum rows to sample from data frames
#' @param max_cols_sample Integer specifying maximum columns to sample from data frames
#' @param include_row_count Logical indicating whether to include row counts in context
#' @param include_class_info Logical indicating whether to include class information
#' @param include_column_types Logical indicating whether to include column types
#' @param max_history_commands Integer specifying maximum number of history commands to analyze
#'
#' @return Invisibly returns the updated configuration
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_context_config(enable_context_awareness = TRUE)
#' tldr_context_config(analyze_data_frames = TRUE, anonymize_data = TRUE)
#' tldr_context_config(max_rows_sample = 3, max_cols_sample = 5)
#' tldr_context_config(analyze_history = FALSE)  # Disable command history analysis
#' }
tldr_context_config <- function(
  enable_context_awareness = NULL,
  analyze_data_frames = NULL,
  analyze_packages = NULL,
  analyze_history = NULL,
  anonymize_data = NULL,
  max_rows_sample = NULL,
  max_cols_sample = NULL,
  include_row_count = NULL,
  include_class_info = NULL,
  include_column_types = NULL,
  max_history_commands = NULL
) {
  # Get current config
  config <- get_config_all()
  
  # Initialize context_settings if it doesn't exist
  if (is.null(config$context_settings)) {
    config$context_settings <- list(
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
  }
  
  # Update settings with non-NULL values
  if (!is.null(enable_context_awareness)) {
    if (!is.logical(enable_context_awareness)) {
      stop("enable_context_awareness must be a logical value (TRUE or FALSE)")
    }
    config$context_settings$enable_context_awareness <- enable_context_awareness
  }
  
  if (!is.null(analyze_data_frames)) {
    if (!is.logical(analyze_data_frames)) {
      stop("analyze_data_frames must be a logical value (TRUE or FALSE)")
    }
    config$context_settings$analyze_data_frames <- analyze_data_frames
  }
  
  if (!is.null(analyze_packages)) {
    if (!is.logical(analyze_packages)) {
      stop("analyze_packages must be a logical value (TRUE or FALSE)")
    }
    config$context_settings$analyze_packages <- analyze_packages
  }
  
  if (!is.null(analyze_history)) {
    if (!is.logical(analyze_history)) {
      stop("analyze_history must be a logical value (TRUE or FALSE)")
    }
    config$context_settings$analyze_history <- analyze_history
  }
  
  if (!is.null(anonymize_data)) {
    if (!is.logical(anonymize_data)) {
      stop("anonymize_data must be a logical value (TRUE or FALSE)")
    }
    config$context_settings$anonymize_data <- anonymize_data
  }
  
  if (!is.null(max_rows_sample)) {
    if (!is.numeric(max_rows_sample) || max_rows_sample < 0 || max_rows_sample != as.integer(max_rows_sample)) {
      stop("max_rows_sample must be a non-negative integer")
    }
    config$context_settings$max_rows_sample <- as.integer(max_rows_sample)
  }
  
  if (!is.null(max_cols_sample)) {
    if (!is.numeric(max_cols_sample) || max_cols_sample < 0 || max_cols_sample != as.integer(max_cols_sample)) {
      stop("max_cols_sample must be a non-negative integer")
    }
    config$context_settings$max_cols_sample <- as.integer(max_cols_sample)
  }
  
  if (!is.null(include_row_count)) {
    if (!is.logical(include_row_count)) {
      stop("include_row_count must be a logical value (TRUE or FALSE)")
    }
    config$context_settings$include_row_count <- include_row_count
  }
  
  if (!is.null(include_class_info)) {
    if (!is.logical(include_class_info)) {
      stop("include_class_info must be a logical value (TRUE or FALSE)")
    }
    config$context_settings$include_class_info <- include_class_info
  }
  
  if (!is.null(include_column_types)) {
    if (!is.logical(include_column_types)) {
      stop("include_column_types must be a logical value (TRUE or FALSE)")
    }
    config$context_settings$include_column_types <- include_column_types
  }
  
  if (!is.null(max_history_commands)) {
    if (!is.numeric(max_history_commands) || max_history_commands < 0 || max_history_commands != as.integer(max_history_commands)) {
      stop("max_history_commands must be a non-negative integer")
    }
    config$context_settings$max_history_commands <- as.integer(max_history_commands)
  }
  
  # Save the updated config
  save_config(config)
  
  # Print a message to confirm
  status <- if (config$context_settings$enable_context_awareness) "enabled" else "disabled"
  message("Context awareness is now ", status)
  
  invisible(config)
}

#' Test the context analyzer by showing what data it collects
#'
#' @return Invisibly returns the context data
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_test_context()
#' }
tldr_test_context <- function() {
  # Get current config
  config <- get_config_all()
  
  # Initialize the context analyzer with current settings
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
  
  context_analyzer <- ContextAnalyzer$new(context_settings)
  
  # Analyze the environment
  context_analyzer$analyze_environment()
  
  # Print the context data in a nicely formatted way
  cat("\n")
  cli::cli_h1("Context Analysis Summary")
  
  # Active packages
  if (length(context_analyzer$context_data$active_packages) > 0) {
    cli::cli_h2("Active Packages")
    cat(paste(context_analyzer$context_data$active_packages, collapse = ", "), "\n\n")
  }
  
  # Recent functions
  if (length(context_analyzer$context_data$recent_functions) > 0) {
    cli::cli_h2("Recently Used Functions")
    cat(paste(context_analyzer$context_data$recent_functions, collapse = ", "), "\n\n")
  }
  
  # Data frames
  if (length(context_analyzer$context_data$data_frames) > 0) {
    cli::cli_h2("Available Data Frames")
    
    for (df_name in names(context_analyzer$context_data$data_frames)) {
      df_info <- context_analyzer$context_data$data_frames[[df_name]]
      
      cli::cli_h3(df_name)
      cat(sprintf("Dimensions: %d rows × %d columns\n", df_info$rows, df_info$cols))
      
      if (length(df_info$column_names) > 0) {
        cat("Columns: ", paste(df_info$column_names, collapse = ", "), "\n")
      }
      
      if (!is.null(df_info$column_types) && length(df_info$column_types) > 0) {
        cat("Column Types:\n")
        for (col in names(df_info$column_types)) {
          cat(sprintf("  - %s: %s\n", col, df_info$column_types[col]))
        }
      }
      
      if (!is.null(df_info$class)) {
        cat("Class: ", paste(df_info$class, collapse = ", "), "\n")
      }
      
      # Sample data (if available and anonymization is disabled)
      if (!is.null(df_info$data_sample)) {
        cat("Data Sample:\n")
        print(df_info$data_sample)
      }
      
      cat("\n")
    }
  }
  
  # Environment info
  if (length(context_analyzer$context_data$environment_info) > 0) {
    cli::cli_h2("Environment Information")
    cat(sprintf("R Version: %s\n", context_analyzer$context_data$environment_info$r_version))
    cat(sprintf("Platform: %s\n", context_analyzer$context_data$environment_info$platform))
    
    cat(sprintf("Tidyverse Available: %s\n", 
               ifelse(context_analyzer$context_data$environment_info$has_tidyverse, "Yes", "No")))
    cat(sprintf("data.table Available: %s\n", 
               ifelse(context_analyzer$context_data$environment_info$has_data_table, "Yes", "No")))
  }
  
  # Context analyzer settings
  cli::cli_h2("Context Analyzer Settings")
  cat(sprintf("Analyze Data Frames: %s\n", 
             ifelse(context_analyzer$privacy_settings$analyze_data_frames, "Yes", "No")))
  cat(sprintf("Analyze Packages: %s\n", 
             ifelse(context_analyzer$privacy_settings$analyze_packages, "Yes", "No")))
  cat(sprintf("Analyze History: %s\n", 
             ifelse(context_analyzer$privacy_settings$analyze_history, "Yes", "No")))
  cat(sprintf("Anonymize Data: %s\n", 
             ifelse(context_analyzer$privacy_settings$anonymize_data, "Yes", "No")))
  
  cat("\n")
  cat("Note: This information is what would be analyzed and potentially included in AI prompts\n")
  cat("based on your current context awareness settings.\n")
  
  invisible(context_analyzer$context_data)
}