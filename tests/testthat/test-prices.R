test_that("paddle_list_prices() returns a list with filters", {
  skip_on_cran()
  set_paddle_mode("sandbox")

  res <- paddle_list_prices(
    status = c("active"),
    per_page = 5,
    include = "product",
    order_by = "id[ASC]",
    recurring = TRUE,
    type = "standard"
  )

  expect_type(res, "list")
  expect_true("data" %in% names(res))
})

test_that("paddle_list_prices() returns errors with wrong inputs", {
  skip_on_cran()
  set_paddle_mode("sandbox")

  expect_error(
    paddle_list_prices(
      order_by = "worng input"
  ))

  expect_error(
    paddle_list_prices(
    type = "worng input"
  ))

  expect_error(
    paddle_list_prices(
    status = "worng input"
  ))

})

test_that("paddle_create_price() errors with invalid tax_mode", {
  skip_on_cran()
  expect_error(
    paddle_create_price(
      description = "Test Invalid Tax Mode",
      product_id = "pro_01jvpnemqfw4fh3nypjhxzpxwh",  # Replace with valid ID for live test
      unit_price = list(amount = "1000", currency_code = "USD"),
      tax_mode = "illegal_value"
    ),
    "`tax_mode` must be one of"
  )
})


# test_that("paddle_create_price() works with valid inputs", {
#   skip_on_cran()
#   expect_type(
#     paddle_create_price(
#       description = "paddleR unit test",
#       name = "paddleR unit test price",
#       product_id = "pro_01jvpnemqfw4fh3nypjhxzpxwh",  # Replace with valid ID for live test
#       unit_price = list(amount = "1000", currency_code = "USD"),
#       billing_cycle = list(frequency = 12, interval = "month"),
#       trial_period = list(frequency = 14, interval = "day"),
#       quantity = list(minimum = 1, maximum = 5),
#       tax_mode = "account_setting",
#       type = "standard",
#       custom_data = list(modified_by = "unit_test")
#     ),
#     "list"
#   )
# })

test_that("paddle_update_price() errors with invalid tax_mode", {
  skip_on_cran()
  expect_error(
    paddle_update_price(
      id = "pro_01jvpnemqfw4fh3nypjhxzpxwh",  # Replace with valid ID if testing live
      tax_mode = "bad_value"
    ),
    "`tax_mode` must be one of"
  )
})

test_that("paddle_update_price() errors with invalid type", {
  skip_on_cran()
  expect_error(
    paddle_update_price(
      id = "pro_01jvpnemqfw4fh3nypjhxzpxwh",  # Replace with valid ID if testing live
      type = "bad_value"
    )
  )
})

test_that("paddle_update_price() errors with invalid status", {
  skip_on_cran()
  expect_error(
    paddle_update_price(
      id = "pro_01jvpnemqfw4fh3nypjhxzpxwh",  # Replace with valid ID if testing live
      status = "bad_value")
  )
})

test_that("paddle_update_price() works with all optional fields", {
  skip_on_cran()
  expect_type(paddle_update_price(
    id = "pri_01jvpq30eqev9nmyt50rpe1zvz",  # Replace with valid sandbox ID to test
    description = "Updated price description",
    name = "Test Annual Plan",
    type = "standard",
    billing_cycle = list(frequency = 12, interval = "month"),
    trial_period = list(frequency = 14, interval = "day"),
    tax_mode = "account_setting",
    unit_price = list(amount = "11500", currency_code = "USD"),
    quantity = list(minimum = 1, maximum = 5),
    status = "active",
    custom_data = list(modified_by = "unit_test")
  ), "list")
})
