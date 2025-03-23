#' Package initialization
#'
#' @importFrom utils packageVersion
#'
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # Display welcome message
  version <- utils::packageVersion("tldrAI")
  packageStartupMessage(
    cli::col_cyan("\u2500\u2500 tldrAI ", version, " \u2500\u2500\n"),
    "Welcome to tldrAI! Get AI-powered quick reference for R functions.\n",
    cli::col_green("• "), "Use tldr(\"function_name\") to get concise help for a function\n",
    cli::col_green("• "), "Try character voices with tldr(\"mean\", voice = \"theatrical_villain\")\n",
    cli::col_green("• "), "See all available voices with tldr_list_voices()\n",
    cli::col_green("• "), "Store API keys securely with tldr_set_api_key()\n",
    cli::col_green("• "), "View the GitHub repo with tldr_open_repo()\n",
    "\nIf you find tldrAI helpful, please consider starring the GitHub repository!",
    "\nFor more information, see help(package = \"tldrAI\")"
  )
  
  # Check if keyring is available
  if (requireNamespace("keyring", quietly = TRUE)) {
    # Check if we should migrate any keys
    # We only do this in interactive sessions to avoid prompting in scripts
    if (interactive()) {
      tryCatch({
        config <- get_config_all()
        if (is.list(config) && 
            ((length(config$api_key) > 0 && nchar(config$api_key) > 0) || 
             (length(config$openai_api_key) > 0 && nchar(config$openai_api_key) > 0))) {
          packageStartupMessage(
            "\nNOTE: API keys found in configuration file that could be stored more securely.",
            "\nRun tldr_key_migrate() to move keys to your system's secure keyring."
          )
        }
      }, error = function(e) {
        # Continue on error - don't bother user with errors on startup
      })
    }
  }
}

#' .onLoad hook
#'
#' Functions to run when package is loaded
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Nothing to do here for now
}