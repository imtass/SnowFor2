#' ThreadError Error object for a thread
#'
#' @param e error
#' @param param the loop parameter when the error happen
#'
#' @return
#' @export
#'
#' @examples
ThreadError = function(e, param) {
  ret = list(pid = Sys.getpid(), error = e, param = param)
  class(ret) = "ThreadError"
  ret
}

#' @export
print.ThreadError = function(x, ...) {
  cat("Thread Error:" ,
      "\n** Pid:", x$pid,
      "\n** Message:", x$error$message,
      "\n** Param: <Check ThreadError$param>\n")
}

#' getErrors filter error ThreadError from result of snowFor()
#'
#' @param x return of snwoFor()
#'
#' @return
#' @export
#'
#' @examples
getErrors = function(x) {
  is_error = (sapply(x, class) == "ThreadError")
  x[is_error]
}
