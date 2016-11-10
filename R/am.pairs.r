#' Get word pairs
#'
#' Get word pairs from two strings
#'
#' @param term1 character vector
#' @param term2 character vector
#' @param minlen number: minimal word length
#' @param prfx character: prefix for the returned word pairs (optional)
#' @return Character
#' @seealso ...
#' @export

am.pairs <- function(term1
                     , term2
                     , prfx = ""
                     , minlen = 2) {

  term1 <- Corpus(VectorSource(term1))
  term2 <- Corpus(VectorSource(term2))

  term1 <- Terms(DocumentTermMatrix(term1, control=list(removeNumbers=FALSE, stopwords=TRUE, wordLengths = c(minlen, Inf), stemming=TRUE)))
  term2 <- Terms(DocumentTermMatrix(term2, control=list(removeNumbers=FALSE, stopwords=TRUE, wordLengths=c(minlen, Inf), stemming=TRUE)))

  term1 <- data.frame(term1 = term1)
  term2 <- data.frame(term2 = term2)

  rtrn <- merge(term1, term2, all=TRUE)
  rtrn <- data.table(rtrn)

  if (nrow(rtrn) > 0) {
    rtrn[, res := paste(prfx, term1, term2, sep="***")]
    rtrn <- paste(rtrn[, res], collapse = " ")
  } else {
    rtrn <- ""
  }

  rtrn
}
