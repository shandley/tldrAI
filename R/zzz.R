#' Package startup functions
#' 
#' @importFrom utils packageVersion
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
    "\nFor more information, see ", cli::col_blue("help(package = \"tldrAI\")")
  )
  
  packageStartupMessage(startup_msg)
}