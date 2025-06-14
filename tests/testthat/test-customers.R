# customer tests

test_that("paddle_create_customer() returns error when email is already used", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_create_customer(
    email = "test@example.com",
    name = "Test User",
    custom_data = list(source = "unit_test"),
    locale = "en"
  ), "Paddle API error: customer email conflicts with customer of id ctm_01jwp3yens988zjex66a8hh7rz")
})

test_that("paddle_create_customer() returns error when email is not filled", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_create_customer(
    name = "Test User",
    custom_data = list(source = "unit_test"),
    locale = "en"
  ), "`email` is required and must be a non-empty string.")
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

test_that("paddle_update_customer() returns error with wrong status", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_update_customer(
    id = "cus_invalid",  # replace with a real ID for live test
    name = "Updated Test",
    email = "updated@example.com",
    status = "archived",
    custom_data = list(tag = "updated"),
    locale = "sk"
  ))
})

test_that("paddle_update_customer() returns error with wrond customer id", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_update_customer(
    id = "ctm_01jwk0s510nxxv3gv8ky41e46a",  # replace with a real ID for live test
    name = "Updated Test",
    email = "updated@example.com",
    status = "wrong",
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

test_that("paddle_list_customer() returns error with wrong inputs4", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_list_customers(order_by = "wrong input"))
})

test_that("paddle_list_credit_balances() validates input correctly", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_list_credit_balances(""),
               "`id` must be a non-empty string")

  expect_error(paddle_list_credit_balances("ctm_01jwk0s510nxxv3gv8ky41e46a", currency_code = "usd"),
               "Invalid currency code")

  expect_silent(paddle_list_credit_balances("ctm_01jwk0s510nxxv3gv8ky41e46a"))
  expect_silent(paddle_list_credit_balances("ctm_01jwk0s510nxxv3gv8ky41e46a", currency_code = c("USD", "EUR")))
})

test_that("paddle_list_customer_addresses() validates inputs correctly", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_list_customer_addresses(""),
               "`id` must be a non-empty string")

  expect_error(paddle_list_customer_addresses("ctm_01jwk0s510nxxv3gv8ky41e46a", order_by = "created_at[ASC]"),
               "`order_by` must be one of")

  expect_error(paddle_list_customer_addresses("ctm_01jwk0s510nxxv3gv8ky41e46a", status = "deleted"),
               "`status` must be one of")

  expect_error(paddle_list_customer_addresses("ctm_01jwk0s510nxxv3gv8ky41e46a", status = c("archived", "bad")),
               "`status` must be one of")

  expect_silent(paddle_list_customer_addresses("ctm_01jwk0s510nxxv3gv8ky41e46a", order_by = "id[DESC]"))
  expect_silent(paddle_list_customer_addresses("ctm_01jwk0s510nxxv3gv8ky41e46a", search = "billing"))
  expect_silent(paddle_list_customer_addresses("ctm_01jwk0s510nxxv3gv8ky41e46a", per_page = 50, status = "active"))
})

test_that("paddle_create_customer_address() validates inputs correctly", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_create_customer_address("", "US"),
               "`id` must be a non-empty string")

  expect_error(paddle_create_customer_address("ctm_01jwk0s510nxxv3gv8ky41e46a", ""),
               "`country_code` must be a valid 2-letter")

  expect_error(paddle_create_customer_address("ctm_01jwk0s510nxxv3gv8ky41e46a", "usa"),
               "`country_code` must be a valid 2-letter")

  # expect_silent(
  #   paddle_create_customer_address(
  #     id = "ctm_01jwk0s510nxxv3gv8ky41e46a",
  #     country_code = "US",
  #     city = "New York",
  #     postal_code = "10001"
  #   )
  # )
})

test_that("paddle_update_customer_address() validates inputs correctly", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_update_customer_address("", "add_1"),
               "`id` must be a non-empty string")

  expect_error(paddle_update_customer_address("ctm_01jwk0s510nxxv3gv8ky41e46a", ""),
               "`address_id` must be a non-empty string")

  expect_error(paddle_update_customer_address("ctm_01jwk0s510nxxv3gv8ky41e46a", "add_1", country_code = "US1"),
               "`country_code` must be a valid 2-letter")

  expect_error(paddle_update_customer_address("ctm_01jwk0s510nxxv3gv8ky41e46a", "add_1", status = "disabled"),
               "`status` must be one of")

  expect_silent(
    paddle_update_customer_address(
      id = "ctm_01jwk0s510nxxv3gv8ky41e46a",
      address_id = "add_01jx207kz1cc4txvm4fzshmwpp",
      city = "Berlin",
      country_code = "DE",
      status = "active"
    )
  )
})

test_that("paddle_list_customer_businesses() validates input correctly", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_list_customer_businesses(""),
               "`id` must be a non-empty string")

  expect_error(paddle_list_customer_businesses("ctm_01jwk0s510nxxv3gv8ky41e46a", order_by = "name[ASC]"),
               "`order_by` must be one of")

  expect_error(paddle_list_customer_businesses("ctm_01jwk0s510nxxv3gv8ky41e46a", status = c("invalid")),
               "`status` must be one of")

  expect_error(paddle_list_customer_businesses("ctm_01jwk0s510nxxv3gv8ky41e46a", status = c("active", "deleted")),
               "`status` must be one of")

  expect_silent(paddle_list_customer_businesses("ctm_01jwk0s510nxxv3gv8ky41e46a"))
  expect_silent(paddle_list_customer_businesses("ctm_01jwk0s510nxxv3gv8ky41e46a", order_by = "id[DESC]", status = "archived", search = "LLC"))
})

test_that("paddle_create_customer_business() validates inputs correctly", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_create_customer_business("", name = "Test"),
               "`id` must be a non-empty string")

  expect_error(paddle_create_customer_business("ctm_01jwk0s510nxxv3gv8ky41e46a", name = ""),
               "`name` must be a non-empty string")

  expect_error(paddle_create_customer_business("ctm_01jwk0s510nxxv3gv8ky41e46a", name = "Business", contacts = list(list(name = "X"))),
               "Each contact must be a list containing at least an `email` field")

  expect_error(paddle_create_customer_business("ctm_01jwk0s510nxxv3gv8ky41e46a", name = "Business",
                                               contacts = list(list(name = "X", email = "invalidemail"))),
               "Each contact must contain a valid email address")

  expect_error(
    paddle_create_customer_business(
      id = "ctm_01jwk0s510nxxv3gv8ky41e46a",
      name = "Acme Inc.",
      tax_identifier = "VAT123456",  # invalid: contains letters
      contacts = list(list(email = "alice@example.com"))
    ),
    "`tax_identifier` must contain only digits"
  )
})

test_that("paddle_update_customer_business() validates inputs correctly", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_update_customer_business("", "biz_01jx23b3fhk07ngxddazz0e9a1"),
               "`id` must be a non-empty string")

  expect_error(paddle_update_customer_business("ctm_01jwk0s510nxxv3gv8ky41e46a", ""),
               "`business_id` must be a non-empty string")

  expect_error(paddle_update_customer_business("ctm_01jwk0s510nxxv3gv8ky41e46a", "biz_01jx23b3fhk07ngxddazz0e9a1", tax_identifier = "VAT1234"),
               "`tax_identifier` must contain only digits")

  expect_error(paddle_update_customer_business("ctm_01jwk0s510nxxv3gv8ky41e46a", "biz_01jx23b3fhk07ngxddazz0e9a1", status = "deleted"),
               "`status` must be one of: active, archived")

  expect_error(paddle_update_customer_business("ctm_01jwk0s510nxxv3gv8ky41e46a", "biz_01jx23b3fhk07ngxddazz0e9a1", contacts = list(list(name = "Jane"))),
               "Each contact must include a valid `email` field")

  # expect_silent(
  #   paddle_update_customer_business(
  #     id = "ctm_01jwk0s510nxxv3gv8ky41e46a",
  #     business_id = "biz_01jx23b3fhk07ngxddazz0e9a1",
  #     name = "New Co.",
  #     tax_identifier = "12345678",
  #     status = "active",
  #     contacts = list(list(name = "Jane Doe", email = "jane@example.com"))
  #   )
  # )
})
