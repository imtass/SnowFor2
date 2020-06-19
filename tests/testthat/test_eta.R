context("SnowFor_ETA")
skip("too long")

go_fun = function(x){
  Sys.sleep(1)
  #print(x)
  #x
}

test_that("Basic usage", {

  a = snowFor(1:120, go_fun,cores = 2)


})
