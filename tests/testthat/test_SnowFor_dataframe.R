context("SnowFor_DF")

test_that("Basic usage", {
  params = data.frame(a = 1:6, b = 2:7)

  go_fun_2 = function(x) {
    x$a + x$b
  }

  a = snowFor(params, go_fun_2, cores = 2)

  expect_equal(unlist(a), c(3,5,7,9,11,13))
  
})

