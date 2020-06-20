context("SnowFor_error")

test_that("errors", {

  expect_warning({
    a = snowFor(1:6, function(x){
      if(x == 3){stop("adsf")}
      x
    },cores = 2)
  })

  expect_equal(class(a[[3]]), "ThreadError")

  expect_equal(a[[3]]$param,3)

})




