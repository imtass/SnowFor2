#' snowFor multithreading map-like function based on snow and foreach
#'
#' Errors won't break the loop.
#' Instead the loop will return an ThreadError object and keep going.
#'
#' @param x list to loop. Accept vector, list and data.frame(by row)
#' @param FUN the function for mapping
#' @param pre_fun prepare function to init all nodes
#' @param var_list name string vector of objects to be exported to nodes: "a_variable"
#' @param cores number of threads
#' @param env env to store the cl object. defalut: globalenv()
#' @param use_df returns a data.frame
#' @param ... params pass to makeSOCKcluster()
#' @return
#' @export
#' @import snow
#' @import doSNOW
#' @import foreach
#' @import crayon
#' @import dplyr
#'
#' @examples
#' go_fun = function(x){
#'   Sys.sleep(1)
#'   x
#'  }
#' a = snowFor(1:10,go_fun,cores = 2)
#'
#'
snowFor = function(x,
                   FUN,
                   pre_fun = NULL,
                   var_list = NULL,
                   cores = parallel::detectCores(),
                   env = globalenv(),
                   use_df = FALSE,
                   ...) {


  if (is.data.frame(x)) {
    x = split(x, seq(nrow(x)))
  }

  tryCatch(
    stopCluster(env$.snowfor_cl),
    error = function(e) e
  )


  cores = min(cores,length(x))

  if(exists(".snowfor_cl",envir = env)){
    #cat(".snowfor_cl exists. Try to stop and clean.")
    try({
      stopCluster(env$.snowfor_cl)
      rm(env$.snowfor_cl)
    },silent = T)

  }

  cat("\nCores =",cores,"\n")
  cat("Make clusters ... ")


  assign(".snowfor_cl", makeSOCKcluster(cores, ...), envir = env)


  registerDoSNOW(env$.snowfor_cl)

  done_str = crayon::green("[√]\n")

  cat(done_str)

  if(!is.null(pre_fun)){
    cat("Call preparing function ... ")
    clusterCall(env$.snowfor_cl, pre_fun)
    cat(done_str)
  }

  if(!is.null(var_list)){
    cat("Copy variables ... ")
    clusterExport(env$.snowfor_cl, var_list)
    cat(done_str)
  }
#
#   if (!is.null(pre_fun) | !is.null(var_list)) {
#     cat("Perparing nodes ... ")
#     if(!is.null(pre_fun)){
#       clusterCall(env$.snowfor_cl, pre_fun)
#     }
#     if(!is.null(var_list)){
#       clusterExport(env$.snowfor_cl, var_list)
#     }
#     cat("done.\n")
#   }

  cat("\nProcessing ... \n")

  n = length(x)
  pb <- txtProgressBarETA(max = n)

  block_size = max(1,floor(n/100))

  makeProgress = function(total_size){
    progress <- function(i) {
      if(i %% block_size == 0){
        setTxtProgressBar(pb, i)
      }
    }
    progress
  }

  opts <- list(progress = makeProgress(length(x)))

  ret = vector(mode = "list", length = length(x))

  ret <- foreach(i = x, .options.snow = opts) %dopar% {
    tryCatch(
      FUN(i),
      error = function(e) {
        ThreadError(e,i)
      }
    )}

  cat("\nAll Done.\n")

  stopCluster(env$.snowfor_cl)
  rm(".snowfor_cl",envir = env)

  if (any(lapply(ret, class) == "ThreadError")) {
    warning("ThreadErrors! Check return.") }
  else if(use_df){
    ret = dplyr::bind_rows(ret)
  }

  ret
}

