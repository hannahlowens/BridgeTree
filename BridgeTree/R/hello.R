#' @title Hello
#'
#' @description Writes "Hello, world!"
#'
#' @param nTimes
#'
#' @return NULL
#'
#' @examples
#' hello(nTimes = 2);
#'
#' @export

hello <- function(nTimes) {
  count <- 1;
  while (count <= nTimes){
    print("Hello, world!");
    count <- count + 1;
  }
}
