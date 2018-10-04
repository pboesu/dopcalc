context("datetime handling")

test_that('offset for GPS dates is correct', {
  expect_equal(ymdhms2gps(1980,1,6,0,0,0), list(gps_week = 0, sec_of_week = 0))
})
