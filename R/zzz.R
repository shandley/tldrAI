#' Package startup functions
#' 
#' @importFrom utils packageVersion
#' @importFrom utils browseURL
NULL

.onAttach <- function(libname, pkgname) {
  pkg_version <- utils::packageVersion("tldrAI")
  
  startup_msg <- paste0(
    cli::col_cyan("── tldrAI "), cli::col_silver(pkg_version), cli::col_cyan(" ──"), "\n",
    "Welcome to ", cli::col_green("tldrAI"), "! Get AI-powered quick reference for R functions.\n",
    "• Use ", cli::col_yellow("tldr(\"function_name\")"), " to get concise help for a function\n",
    "• Try character voices with ", cli::col_yellow("tldr(\"mean\", voice = \"theatrical_villain\")"), "\n",
    "• See all available voices with ", cli::col_yellow("tldr_list_voices()"), "\n",
    "• Configure API keys with ", cli::col_yellow("tldr_config()"), "\n",
    "• View the GitHub repo with ", cli::col_yellow("tldr_open_repo()"), "\n",
    "\nIf you find tldrAI helpful, please consider starring the GitHub repository!\n",
    "For more information, see ", cli::col_blue("help(package = \"tldrAI\")")
  )
  
  packageStartupMessage(startup_msg)
}

#' Open the tldrAI GitHub repository in a web browser
#'
#' This function opens the tldrAI GitHub repository in your default web browser.
#' If you find the package useful, please consider starring the repository!
#'
#' @return Opens a browser window/tab pointing to the tldrAI GitHub repository
#' @export
#'
#' @examples
#' \dontrun{
#' tldr_open_repo()
#' }
tldr_open_repo <- function() {
  repo_url <- "https://github.com/shandley/tldrAI"
  message("Opening ", repo_url, " in your browser")
  utils::browseURL(repo_url)
}