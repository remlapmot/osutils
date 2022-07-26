
test_that("correct IDs are sampled", {
  expect_equal(sample_random_n(c(1,2,3,4,5,6), 3), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(sample_random_prop(c(1,2,3,4,5,6), 0.5), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(sample_nonoutcomes_n(c(0,0,0,0,1,1), c(1,2,3,4,5,6), 2), c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(sample_nonoutcomes_prop(c(0,0,0,0,1,1), c(1,2,3,4,5,6), 0.5), c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
})

test_that("all IDs are sampled", {
  expect_equal(sample_random_n(c(1,2,3,4,5,6), 7), c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(sample_random_prop(c(1,2,3,4,5,6), 1), c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(sample_nonoutcomes_n(c(0,0,0,0,1,1), c(1,2,3,4,5,6), 6), c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(sample_nonoutcomes_prop(c(0,0,0,0,1,1), c(1,2,3,4,5,6), 1), c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
})
