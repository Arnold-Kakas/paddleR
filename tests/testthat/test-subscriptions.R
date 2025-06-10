test_that("paddle_list_subscriptions() input validation works", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_list_subscriptions(per_page = 0), "must be between 1 and 200")
  expect_error(paddle_list_subscriptions(order_by = "date[ASC]"), "must be 'id\\[ASC\\]' or 'id\\[DESC\\]'")
  expect_error(paddle_list_subscriptions(collection_mode = "invoice"), "must be 'automatic' or 'manual'")
  expect_error(paddle_list_subscriptions(scheduled_change_action = "incorrect"), "must be 'cancel', 'pause' or 'resume'.")
  expect_error(paddle_list_subscriptions(status = "incorrect"), "must be 'active', 'cancelled', 'past_due', 'paused' or 'trialing'")
})

test_that("paddle_list_subscriptions() works with valid inputs", {
  expect_silent(paddle_list_subscriptions(
    id = c("sub_01jvptej6fxyctrdt8ty45gw4k", "sub_01jvprmj2e0fn0ay9h0xjv375q"),
    customer_id = "ctm_01jvprk7zfmz4xe5298np09ck3",
    collection_mode = "automatic",
    per_page = 50,
    order_by = "id[ASC]"
  ))
})

test_that("paddle_update_subscription() validates and sends correct body", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  # Successful request with all parameters set
  expect_silent(
    paddle_update_subscription(
      subscription_id = "sub_01jvptej6fxyctrdt8ty45gw4k",
      customer_id = "ctm_01jvprk7zfmz4xe5298np09ck3",
      address_id = "add_01jx79vr5edpwz87rw0p3b74ad",
      business_id = "biz_01jx79wgp13c8wphsw36gx22xj",
      currency_code = "EUR",
      discount = list(
        id = "dsc_01jx7mfd3bdqgwvw9595c50jfq",
        effective_from = "immediately"
      ),
      items = list(
        list(price_id = "pri_01jvpq30eqev9nmyt50rpe1zvz", quantity = 2)
      ),
      proration_billing_mode = "prorated_immediately",
      on_payment_failure = "prevent_change",
      custom_data = list(plan = "custom", source = "landing"),
      scheduled_change = NULL
    )
  )
})

test_that("paddle_update_subscription() validates required args", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_update_subscription(), "must be a non-empty string")
  expect_error(paddle_update_subscription("sub_01jvptej6fxyctrdt8ty45gw4k", collection_mode = "pay"), "must be 'automatic' or 'manual'")
  expect_error(paddle_update_subscription("sub_01jvptej6fxyctrdt8ty45gw4k", currency_code = "CAD"), "must be one of: 'USD', 'EUR', 'GBP'")
  expect_error(paddle_update_subscription("sub_01jvptej6fxyctrdt8ty45gw4k", discount = list(id = "dsc_1")), "must be a list with `id` and `effective_from`")
  expect_error(paddle_update_subscription("sub_01jvptej6fxyctrdt8ty45gw4k", billing_details = "invalid"), "`billing_details` must be a list")
  expect_error(paddle_update_subscription("sub_01jvptej6fxyctrdt8ty45gw4k", items = "notalist"), "`items` must be a list of lists")
  expect_error(paddle_update_subscription("sub_01jvptej6fxyctrdt8ty45gw4k", items = list(list())), "Each item must include at least `price_id`")
  expect_error(paddle_update_subscription("sub_01jvptej6fxyctrdt8ty45gw4k", items = list(list(price_id = "pri_1"))), "required when updating items or next_billed_at")
  expect_error(paddle_update_subscription("sub_01jvptej6fxyctrdt8ty45gw4k", proration_billing_mode = "invalid"), "must be one of")
})



test_that("paddle_preview_subscription_update() validates and sends correct body", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  # Successful request with all parameters set
  expect_silent(
    paddle_preview_subscription_update(
      subscription_id = "sub_01jvptej6fxyctrdt8ty45gw4k",
      customer_id = "ctm_01jvprk7zfmz4xe5298np09ck3",
      address_id = "add_01jx79vr5edpwz87rw0p3b74ad",
      business_id = "biz_01jx79wgp13c8wphsw36gx22xj",
      currency_code = "EUR",
      discount = list(
        id = "dsc_01jx7mfd3bdqgwvw9595c50jfq",
        effective_from = "immediately"
      ),
      items = list(
        list(price_id = "pri_01jvpq30eqev9nmyt50rpe1zvz", quantity = 2)
      ),
      proration_billing_mode = "prorated_immediately",
      on_payment_failure = "prevent_change",
      custom_data = list(plan = "custom", source = "landing"),
      scheduled_change = NULL
    )
  )
})

test_that("paddle_preview_subscription_update() validates required args", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_preview_subscription_update(), "must be a non-empty string")
  expect_error(paddle_preview_subscription_update("sub_01jvptej6fxyctrdt8ty45gw4k", collection_mode = "pay"), "must be 'automatic' or 'manual'")
  expect_error(paddle_preview_subscription_update("sub_01jvptej6fxyctrdt8ty45gw4k", currency_code = "CAD"), "must be one of: 'USD', 'EUR', 'GBP'")
  expect_error(paddle_preview_subscription_update("sub_01jvptej6fxyctrdt8ty45gw4k", discount = list(id = "dsc_1")), "must be a list with `id` and `effective_from`")
  expect_error(paddle_preview_subscription_update("sub_01jvptej6fxyctrdt8ty45gw4k", billing_details = "invalid"), "`billing_details` must be a list")
  expect_error(paddle_preview_subscription_update("sub_01jvptej6fxyctrdt8ty45gw4k", items = "notalist"), "`items` must be a list of lists")
  expect_error(paddle_preview_subscription_update("sub_01jvptej6fxyctrdt8ty45gw4k", items = list(list())), "Each item must include at least `price_id`")
  expect_error(paddle_preview_subscription_update("sub_01jvptej6fxyctrdt8ty45gw4k", items = list(list(price_id = "pri_1"))), "required when updating items or next_billed_at")
  expect_error(paddle_preview_subscription_update("sub_01jvptej6fxyctrdt8ty45gw4k", proration_billing_mode = "invalid"), "must be one of")
})


test_that("paddle_get_update_payment_transaction() validates input", {
  skip_on_cran()
  set_paddle_mode("sandbox")
  expect_error(paddle_get_update_payment_transaction(NULL), "must be a non-empty string")
  expect_error(paddle_get_update_payment_transaction(""), "must be a non-empty string")
  expect_error(paddle_get_update_payment_transaction(123), "must be a non-empty string")

  expect_silent(paddle_get_update_payment_transaction("sub_01jvptej6fxyctrdt8ty45gw4k"))
})


test_that("paddle_preview_one_time_charge() validates required args", {
  skip_on_cran()
  set_paddle_mode("sandbox")

  expect_error(
    paddle_preview_one_time_charge(),
    "`subscription_id` must be a non-empty string"
  )

  expect_error(
    paddle_preview_one_time_charge("sub_01jvptej6fxyctrdt8ty45gw4k"),
    "`effective_from` must be a non-empty RFC 3339 datetime string"
  )

  expect_error(
    paddle_preview_one_time_charge("sub_01jvptej6fxyctrdt8ty45gw4k", "2025-07-01T00:00:00Z"),
    "`items` must be a non-empty list of charge items"
  )

  expect_error(
    paddle_preview_one_time_charge(
      "sub_01jvptej6fxyctrdt8ty45gw4k",
      "2025-07-01T00:00:00Z",
      items = list("not_a_list")
    ),
    "Each item must include `price_id` and `quantity`"
  )

  expect_error(
    paddle_preview_one_time_charge(
      "sub_01jvptej6fxyctrdt8ty45gw4k",
      "2025-07-01T00:00:00Z",
      items = list(list(price_id = "pri_01jvpq30eqev9nmyt50rpe1zvz"))
    ),
    "Each item must include `price_id` and `quantity`"
  )

  expect_error(
    paddle_preview_one_time_charge(
      "sub_01jvptej6fxyctrdt8ty45gw4k",
      "2025-07-01T00:00:00Z",
      items = list(list(price_id = "pri_01jvpq30eqev9nmyt50rpe1zvz", quantity = 1)),
      on_payment_failure = "invalid_value"
    ),
    "`on_payment_failure` must be one of: 'prevent_change', 'allow_change'"
  )
})

test_that("paddle_activate_trial_subscription() validates required argument", {
  skip_on_cran()
  set_paddle_mode("sandbox")

  expect_error(paddle_activate_trial_subscription(), "`subscription_id` must be a non-empty string")
  expect_error(paddle_activate_trial_subscription(""), "`subscription_id` must be a non-empty string")
  #expect_silent(paddle_activate_trial_subscription("sub_01jvptej6fxyctrdt8ty45gw4k"))
})

test_that("paddle_pause_subscription() validates inputs", {
  skip_on_cran()
  set_paddle_mode("sandbox")

  effective_from <- format(seq(Sys.time(), by = "1 month", length.out = 2)[2], "%Y-%m-%dT%H:%M:%SZ")

  expect_error(paddle_pause_subscription(), "`subscription_id` must be a non-empty string")
  expect_error(paddle_pause_subscription("sub_01jvprmj2e0fn0ay9h0xjv375q", on_resume = "invalid"), "`on_resume` must be 'start_new_billing_period' or 'continue_billing_period'")
  #expect_silent(paddle_pause_subscription("sub_01jvptej6fxyctrdt8ty45gw4k", effective_from = "immediately", resume_at = effective_from, on_resume = "start_new_billing_period"))

  })

test_that("paddle_resume_subscription() validates inputs", {
  skip_on_cran()
  set_paddle_mode("sandbox")

  effective_from <- format(seq(Sys.time(), by = "1 month", length.out = 2)[2], "%Y-%m-%dT%H:%M:%SZ")

  expect_error(paddle_resume_subscription(), "`subscription_id` must be a non-empty string")
  expect_error(paddle_resume_subscription("sub_01jvptej6fxyctrdt8ty45gw4k"), "`effective_from` must be a non-empty RFC 3339 datetime string")
  expect_error(paddle_resume_subscription("sub_01jvptej6fxyctrdt8ty45gw4k", effective_from = effective_from, on_resume = "invalid"),
               "`on_resume` must be 'start_new_billing_period' or 'continue_billing_period'")
  #expect_silent(paddle_resume_subscription("sub_01jvptej6fxyctrdt8ty45gw4k", effective_from = effective_from, on_resume = "continue_billing_period"))
})


test_that("paddle_cancel_subscription() validates inputs", {
  skip_on_cran()
  set_paddle_mode("sandbox")

  expect_error(paddle_cancel_subscription(), "`subscription_id` must be a non-empty string")
  expect_error(paddle_cancel_subscription(""), "`subscription_id` must be a non-empty string")
  expect_error(paddle_cancel_subscription("sub_123", effective_from = 42), "`effective_from` must be one of: 'next_billing_period', 'immediately'")
})


# test_that("paddle_cancel_subscription() can cancel with scheduled future date", {
#   skip_on_cran()
#   set_paddle_mode("sandbox")
#
#   res <- paddle_cancel_subscription("sub_01jvprmj2e0fn0ay9h0xjv375q", effective_from = "immediately")
#   expect_type(res, "list")
# })
