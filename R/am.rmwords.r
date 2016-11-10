#' Remove words
#'
#' Remove specified words from character vector
#'
#' @param vc character vector of strings to modify
#' @param words character vector of words to remove
#' @return Character vector
#' @seealso ...
#' @export

am.rmwords <- function(vc
                    , words) {
  str_replace_all(vc, sprintf("\\b(%s)\\b", paste(words, collapse = "|")),"")
}
