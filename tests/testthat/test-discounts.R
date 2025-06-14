test_that("paddle_list_discounts() validates inputs correctly", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_list_discounts(order_by = "invalid_field[ASC]"),
               "`order_by` must be one of")

  expect_error(paddle_list_discounts(status = "invalid"),
               "`status` must be one of: active, archived")

  expect_error(paddle_list_discounts(mode = "invalid"),
               "`mode` must be one of: standard, custom")

  expect_error(paddle_list_discounts(status = c("active", "invalid")),
               "`status` must be one of: active, archived")

  expect_silent(paddle_list_discounts(order_by = "id[ASC]"))
  expect_silent(paddle_list_discounts(status = c("active")))
  expect_silent(paddle_list_discounts(code = c("WELCOME10", "SUMMER25")))
})

test_that("paddle_create_discount() validates required and conditional fields", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_create_discount(description = "New Discount", type = "percentage"),
               "`amount`, `description`, and `type` are required")

  expect_error(paddle_create_discount(amount = "10", description = "New Discount", type = "flat", currency_code = "USD", mode = "invalid"),
               "`mode` must be one of: standard, custom")

  expect_error(paddle_create_discount(amount = "10", description = "New Discount", type = "invalid"),
               "`type` must be one of")

  expect_error(paddle_create_discount(amount = "10", description = "Flat Discount", type = "flat"),
               "`currency_code` is required")

  expect_error(paddle_create_discount(amount = "10", description = "New Discount", type = "percentage", code = "%%INVALID%%"),
               "`code` must be alphanumeric")


})

test_that("paddle_update_discount() validates inputs correctly", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_update_discount(id = ""),
               "`id` must be a non-empty string")

  expect_error(paddle_update_discount(id = "dsc_01jwxsz8xrf3j3g32nrx3eycn9", type = "invalid"),
               "`type` must be one of")


  expect_error(paddle_update_discount(id = "dsc_01jwxsz8xrf3j3g32nrx3eycn9", status = "invalid"),
               "`status` must be one of")

  expect_error(paddle_update_discount(id = "dsc_01jwxsz8xrf3j3g32nrx3eycn9", mode = "invalid"),
               "`mode` must be one of: standard, custom")

  expect_error(paddle_update_discount(id = "dsc_01jwxsz8xrf3j3g32nrx3eycn9", type = "flat"),
               "`currency_code` is required")

  expect_error(paddle_update_discount(id = "dsc_01jwxsz8xrf3j3g32nrx3eycn9", code = "###BADCODE###"),
               "`code` must be alphanumeric")

  expect_silent(
    paddle_update_discount(
      id = "dsc_01jwxsz8xrf3j3g32nrx3eycn9",
      description = "Updated promo",
      type = "percentage",
      amount = "20",
      status = "active",
      mode = "standard",
      code = "SUMMER20",
      currency_code = "USD",
      usage_limit = 100,
      custom_data = list(key = "value")
    )
  )
})
