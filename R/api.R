#' Get AI response from Claude API
#'
#' @param prompt The prompt text to send to the API
#'
#' @return The API response text
#' @keywords internal
get_ai_response <- function(prompt) {
  api_key <- get_config("api_key")
  model <- get_config("model", default = "claude-3-opus-20240229")
  
  if (api_key == "") {
    stop("API key not configured. Use tldr_config(api_key = 'your_api_key') to set it.")
  }
  
  # Construct API request
  response <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      max_tokens = 1024,
      temperature = 0.3,
      messages = list(
        list(
          role = "user",
          content = prompt
        )
      )
    )) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  
  # Parse response
  response_data <- httr2::resp_body_json(response)
  
  # Check for API errors
  if (httr2::resp_status(response) != 200) {
    error_msg <- paste("API error:", httr2::resp_status(response), 
                     response_data$error$type, response_data$error$message)
    stop(error_msg)
  }
  
  # Extract the content from the response
  response_data$content[[1]]$text
}
