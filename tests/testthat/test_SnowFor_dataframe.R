context("SnowFor_DF")

test_that("Basic usage", {
  params = data.frame(a = 1:6, b = 2:7)

  go_fun_2 = function(x) {
    x$a + x$b
  }

  a = snowFor(params, go_fun_2, cores = 2)

  expect_equal(unlist(a), c(3,5,7,9,11,13))


  go_fun_df = function(x) {
    data.frame(x = x ,x2 = x*2)
  }

  b = snowFor(1:10, go_fun_df, cores = 2,use_df = TRUE)

  expect_s3_class(b,"data.frame")

  expect_equal(b[10,2],20)

})

