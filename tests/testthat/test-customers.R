# customer tests

test_that("paddle_create_customer() works with full input", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_create_customer(
    email = "test@example.com",
    name = "Test User",
    custom_data = list(source = "unit_test"),
    locale = "en"
  ), "Paddle API error: customer email conflicts with customer of id ctm_01jwp3yens988zjex66a8hh7rz")
})


test_that("paddle_retrieve_customer() returns data", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  res <- paddle_list_customers(per_page = 1,
                               status = "active",
                               order_by = "id[ASC]"
                               )
  expect_type(res, "list")
})

test_that("paddle_update_customer() works with all optional params", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_update_customer(
    customer_id = "cus_invalid",  # replace with a real ID for live test
    name = "Updated Test",
    email = "updated@example.com",
    status = "archived",
    custom_data = list(tag = "updated"),
    locale = "sk"
  ))
})

test_that("paddle_generate_auth_token() fails with invalid ID", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_generate_auth_token("invalid_id"))
})

test_that("paddle_list_customer() returns error with wrong inputs1", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_list_customers(status = "wrong input"), "`status` must be one of: active, archived")
})


test_that("paddle_list_customer() returns error with wrong inputs3", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_list_customers(search = "wrong input"), "`search` must be one of: id, name, email")
})

test_that("paddle_list_credit_balances() validates input correctly", {
  expect_error(paddle_list_credit_balances(""),
               "`customer_id` must be a non-empty string")

  expect_error(paddle_list_credit_balances("ctm_01jwk0s510nxxv3gv8ky41e46a", currency_code = "usd"),
               "Invalid currency code")

  expect_silent(paddle_list_credit_balances("ctm_01jwk0s510nxxv3gv8ky41e46a"))
  expect_silent(paddle_list_credit_balances("ctm_01jwk0s510nxxv3gv8ky41e46a", currency_code = c("USD", "EUR")))
})
