context("SnowFor_variable")

test_that("variable", {

  a = "hello"

  #varlist = c('a')
  pre_fun = function(){
    library(glue)
  }

  foo = function(x){
    x*2
  }

  var_list = c("a")

  result = snowFor(1:4,function(i){
    glue('adsf')
    foo(i)
  },cores = 2,pre_fun = pre_fun,var_list = var_list)

  expect_equal(unlist(result),c(2,4,6,8))


})
