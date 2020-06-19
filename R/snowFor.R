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
#' @param show_workers_msg show workers' message or not
#' @return
#' @export
#' @import snow
#' @import doSNOW
#' @import foreach
#' @import crayon
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
                   show_workers_msg = F) {


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

  if(show_workers_msg){
    assign(".snowfor_cl", makeSOCKcluster(cores, outfile = ""), envir = env)
  }else{
    assign(".snowfor_cl", makeCluster(cores, type = "SOCK"), envir = env)
  }

  registerDoSNOW(env$.snowfor_cl)

  done_str = crayon::green("[done]\n")

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

  pb <- txtProgressBarETA(max = length(x))

  makeProgress = function(total_size){
    progress <- function(n) {
      setTxtProgressBar(pb, n)
    }
    progress
  }

  opts <- list(progress = makeProgress(length(x)))

  ret = vector(mode = "list", length = length(x))

  tt = system.time({
    ret <- foreach(i = x, .options.snow = opts) %dopar% {
      tryCatch(
        FUN(i),
        error = function(e) {
          ThreadError(e,i)
        }
      )
    }
  })


  cat("\n")
  print(tt)

  stopCluster(env$.snowfor_cl)
  rm(".snowfor_cl",envir = env)

  if (any(lapply(ret, class) == "ThreadError")) { warning("ThreadErrors! Check return.") }

  ret
}

