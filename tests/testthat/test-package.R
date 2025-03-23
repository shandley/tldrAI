test_that("tldr_package function exists", {
  # Just check that the main function exists
  expect_true(exists("tldr_package"))
  
  # Verify it has the expected parameters
  func_formals <- formals(tldr_package)
  expect_true("package_name" %in% names(func_formals))
  expect_true("focus" %in% names(func_formals))
  expect_true("interactive" %in% names(func_formals))
  expect_true("detail" %in% names(func_formals))
  expect_true("categories" %in% names(func_formals))
  expect_true("examples" %in% names(func_formals))
  expect_true("voice" %in% names(func_formals))
  expect_true("provider" %in% names(func_formals))
  expect_true("refresh" %in% names(func_formals))
  expect_true("output" %in% names(func_formals))
  
  # Check default values
  expect_equal(func_formals$focus, "overview")
  expect_equal(func_formals$interactive, FALSE)
  expect_equal(func_formals$detail, "standard")
  expect_equal(func_formals$categories, TRUE)
  expect_equal(func_formals$examples, 2)
  expect_equal(func_formals$refresh, FALSE)
  expect_equal(func_formals$output, "console")
})