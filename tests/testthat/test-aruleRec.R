# Small synthetic transaction dataset used across the tests.
# A handful of customers, a handful of items, in tidy (long) format:
# one customer-item purchase per row. Several customers share the
# {bread, butter} -> milk pattern so that apriori can find rules.
make_sales <- function() {
  data.frame(
    Customer = c(1, 1, 1,
                 2, 2, 2,
                 3, 3, 3,
                 4, 4, 4,
                 5, 5,
                 6, 6),
    Item = c("bread", "butter", "milk",
             "bread", "butter", "milk",
             "bread", "butter", "milk",
             "bread", "butter", "milk",
             "bread", "butter",
             "milk",  "bread"),
    stringsAsFactors = FALSE
  )
}

expected_rule_cols <- c("lhs", "rhs", "support", "confidence",
                        "coverage", "lift", "count")

test_that("complete_fun keeps only rows complete in the desired columns", {
  df <- data.frame(a = c(1, 2, 3), b = c("x", NA, "z"),
                   stringsAsFactors = FALSE)
  out <- complete_fun(df, "b")
  expect_equal(nrow(out), 2)
  expect_false(any(is.na(out$b)))
})

test_that("aruleTrain returns a rules data.frame with the expected columns", {
  skip_if_not_installed("arules")
  sales <- make_sales()

  rules <- aruleTrain(sales, productkey = Item, customerkey = Customer,
                      support = 0.1, confidence = 0.1, minlen = 2)

  expect_s3_class(rules, "data.frame")
  expect_true(all(expected_rule_cols %in% names(rules)))
})

test_that("aruleTrain default maxlen reflects the real product column", {
  # Regression test for the bug where the default maxlen referenced a literal
  # column named 'productkey' (NULL -> length 0 -> maxlen 1) instead of the
  # column the user actually passed. With the fix, rules longer than length 1
  # can be mined from data with several distinct products.
  skip_if_not_installed("arules")
  sales <- make_sales()

  rules <- aruleTrain(sales, productkey = Item, customerkey = Customer,
                      support = 0.1, confidence = 0.1, minlen = 2)

  expect_gt(nrow(rules), 0)
})

test_that("aruleRec returns per-customer recommendations", {
  skip_if_not_installed("arules")
  sales <- make_sales()

  recs <- aruleRec(sales, productkey = Item, customerkey = Customer,
                   support = 0.1, confidence = 0.1, minlen = 2)

  expect_s3_class(recs, "data.frame")
  expect_true(all(c("item_history", "recommendation") %in% names(recs)))
})

test_that("nrec limits the number of recommendations per customer", {
  skip_if_not_installed("arules")
  sales <- make_sales()

  recs <- aruleRec(sales, productkey = Item, customerkey = Customer,
                   support = 0.1, confidence = 0.1, minlen = 2, nrec = 1)

  if (nrow(recs) > 0) {
    per_customer <- table(recs$Customer)
    expect_true(all(per_customer <= 1))
  } else {
    succeed("No recommendations generated; nothing to cap.")
  }
})

test_that("aruleRec handles the no-rules case gracefully", {
  skip_if_not_installed("arules")
  sales <- make_sales()

  # Impossibly strict thresholds -> apriori returns no rules.
  expect_message(
    recs <- aruleRec(sales, productkey = Item, customerkey = Customer,
                     support = 0.999, confidence = 0.999, minlen = 2)
  )

  expect_s3_class(recs, "data.frame")
  expect_equal(nrow(recs), 0)
  expect_true(all(c("item_history", "recommendation") %in% names(recs)))
})

test_that("arulePredict produces recommendations from trained rules", {
  skip_if_not_installed("arules")
  sales <- make_sales()

  rules <- aruleTrain(sales, productkey = Item, customerkey = Customer,
                      support = 0.1, confidence = 0.1, minlen = 2)

  preds <- arulePredict(rules, sales, customerkey = Customer,
                        productkey = Item, keep_all = TRUE)

  expect_s3_class(preds, "data.frame")
  expect_true(all(c("item_history", "recommendation") %in% names(preds)))
})

test_that("arulePredict handles an empty rule set gracefully", {
  skip_if_not_installed("arules")
  sales <- make_sales()

  empty_rules <- data.frame(lhs = character(0), rhs = character(0),
                            support = numeric(0), confidence = numeric(0),
                            coverage = numeric(0), lift = numeric(0),
                            count = integer(0), stringsAsFactors = FALSE)

  expect_message(
    preds <- arulePredict(empty_rules, sales, customerkey = Customer,
                          productkey = Item)
  )

  expect_s3_class(preds, "data.frame")
  expect_equal(nrow(preds), 0)
})
