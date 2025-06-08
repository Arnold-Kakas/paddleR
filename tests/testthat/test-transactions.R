test_that("paddle_list_transactions() validates inputs correctly", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_list_transactions(order_by = "amount[ASC]"), "`order_by` must be one of")
  expect_error(paddle_list_transactions(status = c("completed", "failed")), "`status` must be one of")
  expect_error(paddle_list_transactions(include = c("customer", "bad_value")), "`include` must be one of")
  expect_error(paddle_list_transactions(collection_mode = "bank_transfer"), "`collection_mode` must be one of")

  expect_type(paddle_list_transactions(order_by = "created_at[DESC]"), "list")
  expect_silent(paddle_list_transactions(status = c("completed", "billed")))
  expect_silent(paddle_list_transactions(include = c("customer", "address")))
  expect_silent(paddle_list_transactions(per_page = 50, billed_at = "2023-04-18T17:03:26"))
  expect_silent(paddle_list_transactions(customer_id = "ctm_01jvprk7zfmz4xe5298np09ck3",
                                         subscription_id = "sub_01jvptej6fxyctrdt8ty45gw4k",
                                         collection_mode = "automatic"))
})



test_that("paddle_create_transaction() validates inputs strictly", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_create_transaction(NULL), "`items` must be a non-empty list")

  expect_error(paddle_create_transaction(list(list(price_id = "pri_01jwqx23k0b5q42f9g71zjzby7", quantity = ""))),
               "`quantity` in each item must be a positive number")

  expect_error(paddle_create_transaction(list(list(price_id = "", quantity = 2))),
               "`price_id` in each item must be a non-empty string")

  expect_error(
    paddle_create_transaction(
      currency_code = "EUR",
      items = list(list(price_id = "pri_01jwqx23k0b5q42f9g71zjzby7", quantity = 1)),
      collection_mode = "manual"
    ),
    "`billing_details` is required when `collection_mode` is 'manual'"
  )

  expect_error(
    paddle_create_transaction(
      items = list(list(price_id = "pri_01jwqx23k0b5q42f9g71zjzby7", quantity = 1)),
      collection_mode = "manual"
    ),
    "`currency_code` is required and must be one of: 'USD', 'EUR', 'GBP' when `collection_mode` is 'manual'"
  )


  expect_error(
    paddle_create_transaction(
      currency_code = "EUR",
      items = list(list(price_id = "pri_01jwqx23k0b5q42f9g71zjzby7", quantity = 1)),
      collection_mode = "manual",
      billing_details = list()
    ),
    "`billing_details` must include `payment_terms`"
  )

  expect_silent(
    paddle_create_transaction(
      items = list(list(price_id = "pri_01jx33y8mcbkhw0cfbdrq6kkdx", quantity = 2)),
      customer_id = "ctm_01jwk0s510nxxv3gv8ky41e46a",
      address_id = "add_01jx22twq46b1f7c13gcss7466"
    )
  )
})

test_that("paddle_update_transaction() handles include query and validation", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_update_transaction(""), "`transaction_id` must be")

  expect_error(
    paddle_update_transaction("txn_01jx34btjb292adefxgkrj6t4j", status = "invalid"),
    "`status` must be one of"
  )

  expect_error(
    paddle_update_transaction("txn_01jx34btjb292adefxgkrj6t4j", collection_mode = "auto"),
    "`collection_mode` must be"
  )

  expect_error(
    paddle_update_transaction("txn_01jx34btjb292adefxgkrj6t4j", collection_mode = "manual"),
    "`currency_code` is required"
  )

  expect_error(
    paddle_update_transaction("txn_01jx34btjb292adefxgkrj6t4j", include = "nonsense")
  )

  expect_silent(
    paddle_update_transaction(
      transaction_id = paddle_create_transaction(
        customer_id = "ctm_01jwk0s510nxxv3gv8ky41e46a",
        items = list(list(price_id = "pri_01jx33y8mcbkhw0cfbdrq6kkdx", quantity = 2)),
      )$data$id,
      address_id = "add_01jx22twq46b1f7c13gcss7466"
    ))
})

test_that("paddle_preview_transaction() validates inputs and succeeds", {
  good_items <- list(list(price_id = "pri_01jx33y8mcbkhw0cfbdrq6kkdx", quantity = 2))

  expect_error(paddle_preview_transaction(items = NULL), "must be a non-empty list")
  expect_error(paddle_preview_transaction(items = list()), "must be a non-empty list")
  expect_error(paddle_preview_transaction(items = list(list(quantity = 1))), "price_id")
  expect_error(paddle_preview_transaction(items = list(list(price_id = "x", quantity = 0))), "positive number")
  expect_error(paddle_preview_transaction(items = good_items, currency_code = "JPY"), "must be one of")

  # Valid call (mocked)
  expect_silent(
    paddle_preview_transaction(
      items = good_items,
      customer_id = "ctm_01jwk0s510nxxv3gv8ky41e46a",
      currency_code = "EUR",
      discount_id = "dsc_01jwxsz8xrf3j3g32nrx3eycn9",
      ignore_trials = TRUE,
      customer_ip_address = "8.8.8.8"
    )
  )
})
