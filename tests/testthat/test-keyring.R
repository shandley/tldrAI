test_that("keyring functions work correctly when keyring is available", {
  # Skip test if keyring is not available
  skip_if_not_installed("keyring")
  
  # Mock the keyring functions
  mockr::with_mock(
    keyring::key_set_with_value = function(service, username, password) TRUE,
    keyring::key_get = function(service, username) "mock_api_key",
    keyring::key_list = function(service) data.frame(service = service, username = c("claude", "openai")),
    keyring::key_delete = function(service, username) TRUE,
    {
      # Test tldr_set_api_key function
      expect_message(
        result <- tldr_set_api_key("test_key", "claude", update_config = FALSE),
        "API key for claude securely stored in system keyring"
      )
      expect_true(result)
      
      # Test tldr_has_api_key function
      expect_true(tldr_has_api_key("claude"))
      
      # Test get_api_key function (internal)
      api_key <- get_api_key("claude")
      expect_equal(api_key, "mock_api_key")
      
      # Test tldr_clear_api_key function
      # Mock the menu function to return 1 ("Yes")
      mockr::with_mock(
        utils::menu = function(choices, title) 1,
        {
          expect_message(
            result <- tldr_clear_api_key("claude"),
            "API key for claude has been removed from the system keyring"
          )
          expect_true(result)
        }
      )
    }
  )
})

test_that("keyring functions gracefully handle missing keyring package", {
  # Skip test if keyring is installed (we want to test the fallback)
  skip_if(requireNamespace("keyring", quietly = TRUE))
  
  # Test tldr_set_api_key falls back to config
  expect_warning(
    result <- tldr_set_api_key("test_key", "claude"),
    "The keyring package is not installed"
  )
  expect_false(result)
  
  # Test tldr_clear_api_key gracefully warns
  expect_warning(
    result <- tldr_clear_api_key("claude"),
    "The keyring package is not installed"
  )
  expect_false(result)
})

test_that("tldr_key_migrate requests keyring if needed", {
  # Skip test if keyring is installed
  skip_if(requireNamespace("keyring", quietly = TRUE))
  
  # Test tldr_key_migrate warns and suggests keyring
  expect_message(
    result <- tldr_key_migrate(),
    "The keyring package is required for secure API key storage"
  )
  expect_equal(result, c(claude = FALSE, openai = FALSE))
})

test_that("migrate_api_keys works with mocked keyring", {
  # Skip test if mockr is not available
  skip_if_not_installed("mockr")
  
  # Mock the keyring functions and config
  mockr::with_mock(
    keyring::key_set_with_value = function(service, username, password) TRUE,
    get_config_all = function() list(api_key = "mock_claude_key", openai_api_key = "mock_openai_key"),
    save_config = function(config) invisible(config),
    utils::menu = function(choices, title) 1,  # Always select "Yes"
    requireNamespace = function(package, quietly) TRUE,
    {
      # Test migrate_api_keys
      result <- migrate_api_keys(ask_confirmation = TRUE)
      expect_equal(result, c(claude = TRUE, openai = TRUE))
    }
  )
})

test_that("get_api_key function correctly prioritizes sources", {
  # Set up temporary environment variables
  old_claude <- Sys.getenv("CLAUDE_API_KEY")
  old_openai <- Sys.getenv("OPENAI_API_KEY")
  
  # Clean up after the test
  on.exit({
    Sys.setenv(CLAUDE_API_KEY = old_claude)
    Sys.setenv(OPENAI_API_KEY = old_openai)
  })
  
  # Mock environment variables
  Sys.setenv(CLAUDE_API_KEY = "env_claude_key")
  Sys.setenv(OPENAI_API_KEY = "env_openai_key")
  
  # Test environment variable priority
  expect_equal(get_api_key("claude"), "env_claude_key")
  expect_equal(get_api_key("openai"), "env_openai_key")
  
  # Clear environment variables
  Sys.setenv(CLAUDE_API_KEY = "")
  Sys.setenv(OPENAI_API_KEY = "")
  
  # Mock keyring and config
  if (requireNamespace("keyring", quietly = TRUE)) {
    mockr::with_mock(
      keyring::key_list = function(service) data.frame(service = service, username = "claude"),
      keyring::key_get = function(service, username) "keyring_claude_key",
      get_config_all = function() list(api_key = "config_claude_key", openai_api_key = "config_openai_key"),
      {
        # Test keyring priority (should be higher than config)
        expect_equal(get_api_key("claude"), "keyring_claude_key")
        # Test config fallback when not in keyring
        expect_equal(get_api_key("openai"), "config_openai_key")
      }
    )
  } else {
    mockr::with_mock(
      get_config_all = function() list(api_key = "config_claude_key", openai_api_key = "config_openai_key"),
      {
        # Test config fallback when keyring not available
        expect_equal(get_api_key("claude"), "config_claude_key")
        expect_equal(get_api_key("openai"), "config_openai_key")
      }
    )
  }
})