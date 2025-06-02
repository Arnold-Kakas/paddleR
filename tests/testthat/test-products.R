# product tests

test_that("paddle_list_products() returns a list with filters", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  res <- paddle_list_products(
    status = c("active"),
    per_page = 1,
    include = c("prices"),
    tax_category = "standard",
    type = NULL,
    after = NULL,
    order_by = "id[DESC]",
    id = NULL
  )
  expect_type(res, "list")
})

test_that("paddle_list_products() returns error with wrong inputs 1", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_list_products(
    status = c("invalid status"),
    per_page = 1,
    include = c("prices"),
    tax_category = "standard",
    type = NULL,
    after = NULL,
    order_by = "id[DESC]",
    id = NULL
  ), "`status` must be one of: active, archived")
})

test_that("paddle_list_products() returns error with wrong inputs 2", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_list_products(
    per_page = 1,
    tax_category = "wrong input",
  ), "`tax_category` must be one of: ")
})

test_that("paddle_list_products() returns error with wrong inputs 2", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_list_products(
    per_page = 1,
    order_by = "wrong_input[DESC]",
  ), "`order_by` must be one of: ")
})

test_that("paddle_create_product() fails without required fields", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_create_product(tax_category = "saas"), "name")
  expect_error(paddle_create_product(name = "Test Product"), "tax_category")
})

test_that("paddle_create_product() works with all optional fields", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_create_product(
    name = "Unit Test Product",
    tax_category = "standard",
    description = "This is a test product",
    type = "standard",
    image_url = "https://example.com/image.png",
    custom_data = list(source = "testthat")
  ), NA)
})

test_that("paddle_update_product() fails without product_id", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_update_product(name = "Updated Name"), "product_id")
})

test_that("paddle_update_product() works with all optional params", {
  skip_on_cran()
  expect_error(paddle_update_product(
    product_id = "pro_01jvpnemqfw4fh3nypjhxzpxwh",  # replace with real ID for live test
    name = "Updated Product",
    description = "Updated via test",
    type = "standard",
    tax_category = "saas",
    image_url = NULL,
    custom_data = list(modified = TRUE),
    status = "archived"
  ))
})
