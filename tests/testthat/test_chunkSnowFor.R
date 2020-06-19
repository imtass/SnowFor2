context("chunkSnowFor")

test_that("Basic usage", {
  a_list = 1:16

  go_fun = function(x) {
    Sys.sleep(0.5)
    c(x,x)
  }

  a = chunkSnowFor(a_list, go_fun , chunk_size = 4, cores = 2 )

  b = snowFor(a_list, go_fun, cores = 2)

  cat("a\n")
  print(unlist(a))
  cat("b\n")
  print(unlist(b))
  expect_true(all.equal(a,b))

})



test_that("Sleep", {
  a_list = 1:4

  go_fun = function(x) {
    c(x,x)
  }

  tt = system.time({
    a = chunkSnowFor(a_list, go_fun , chunk_size = 2, deley_sec = 5, cores = 2 )
  })

  expect_true(tt['elapsed'] > 5)

})


test_that("chunkSnowFor: data.frame", {

  params = data.frame(a = 1:10, b = 2:11)

  go_fun_2 = function(x) {
    x$a + x$b
  }

  a = snowFor(params, go_fun_2, cores = 2)

  b = chunkSnowFor(params, go_fun_2, chunk_size = 4, cores = 2)

  expect_true(all.equal(a, b))

})
