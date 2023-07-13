test_that("label wrap function works", {
  lab <- label_wrap_gen_both(10)
  expect_s3_class(lab, "labeller")
})

test_that("label wrap function works no multiline", {
  lab <- label_wrap_gen_both(10, multi_line = FALSE)
  expect_s3_class(lab, "labeller")
})
