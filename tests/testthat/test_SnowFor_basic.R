context("SnowFor_Base")

go_fun = function(x){
  Sys.sleep(1)
  print(x)
  x
}

test_that("Basic usage", {

  a = snowFor(1:6, go_fun,cores = 2)

  env = globalenv()
  env$some_value = 123
  expect_equal(unlist(a), c(1,2,3,4,5,6))
})



some_value = 123
go_fun = function(x){
  some_value * x
}
a = snowFor(1:6, function(x){
  #if(x == 3){stop("adsf")}
  some_value * x
},var_list = c("some_value"),cores = 2)

test_that("export", {
  expect_equal(unlist(a), (1:6) * 123)
})

go_fun = function(x){
  Sys.sleep(0.01)
  #print(x)
  x
}




