context("SnowFor_ETA")
#skip("too long")

go_fun = function(x){
  Sys.sleep(1)
  #print(x)
  x
}

test_that("Basic usage", {

  #a = snowFor(1:12, go_fun,cores = 1)

  b = snowFor(1:10, go_fun,cores = 2)
  expect_equal(unlist(b),1:10)
})
