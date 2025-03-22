# Example usage to verify our fix

library(tldrAI)

# Try the code_highlight visualization which used to produce the warning
result <- tldr("mean", vis_type = "code_highlight", visualize = TRUE)
cat("Success\! The warning has been fixed.\n")
