#' Testing Stuff
#' 
#' Just a function to test changes.
#' 
#' @export
test_func <- function(f) {
  cpp_results <- .Call("test_func_cpp",
                       f)
  return(cpp_results)
}