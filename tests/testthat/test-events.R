test_that("paddle_get_events() validates event_type argument", {
  skip_on_cran()
  set_paddle_mode("sandbox")

  # invalid types
  expect_error(paddle_get_events(per_page = 500), "`per_page` must be between 1 and 200.")
  #expect_error(paddle_get_events(order_by = "wrong input"), "`order_by` must be 'id[ASC]' or 'id[DESC]'.")
  })

test_that("paddle_get_events() returns data with and without filter", {
  skip_on_cran()
  set_paddle_mode("sandbox")

  # without filter
  res1 <- paddle_get_events(order_by = "id[DESC]", per_page = 10)
  expect_type(res1, "list")
  expect_true("data" %in% names(res1))

})
