
test_that("correct values are redacterd", {
  expect_equal(redactor(c(2,3,4,8,9), 5L), c(TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("correct values are redacterd", {
  expect_equal(redactor2(c(2,3,4,8,9), 5L), c(NA, NA, NA, 8, 9))
})


set.seed(22)
testdata <- data.frame(
  a = sample(c("a","b","c"), size=1000, replace=TRUE),
  b = sample(c("x","y","z"), size=1000, replace=TRUE),
  c= rnorm(1000),
  d= rnorm(1000),
  date = as.Date(runif(1000,0,99999), origin="1970-01-01")
) %>%
  dplyr::mutate(dplyr::across(
    .cols = -date,
    ~{
      type <- typeof(.x)
      typedNA <- NA
      mode(typedNA) <- type
      ifelse(runif(dplyr::n())>0.1, ., typedNA)
    }
  )) %>%
  tibble::add_row(
    a=rep("d",5),
    b=rep("w",5),
    c=0,
    d=0,
    date = as.Date(NA, origin="1970-01-01")
  )

test_that("correct values are redacted",{
  expect_equal(redacted_summary_cat(testdata$a)$redacted, c(FALSE, FALSE, FALSE, TRUE, TRUE))
})

test_that("correct total",{
  expect_equal(redacted_summary_num(testdata$c)$n, 1005L)
})

test_that("correct total",{
  expect_equal(redacted_summary_date(testdata$date)$n, 1005L)
})

# redacted_summary_catcat(testdata$a, testdata$b, .total_name="Total")
# redacted_summary_catcnum(testdata$a, testdata$c, .total_name="Total")

