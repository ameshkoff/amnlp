#' Count percentage of common words
#'
#' Count percentage of common words between two strings. Please remove extra spaces before usage. Vectorized.
#'
#' @param term1 character vector
#' @param term2 character vector
#' @return Number. Percentage (share) of common words between two input strings
#' @seealso am.linwordsu, am.linstems

am.linwords <- function(term1, term2){

  lt1 <- str_split(term1, "\\W+")
  lt2 <- str_split(term2, "\\W+")

  rtrn <- mapply(intersect, lt1, lt2)

  rtrn <- lapply(rtrn, length)
  lt1.l <- lapply(lt1, length)
  lt2.l <- lapply(lt2, length)

  lt.s <- mapply(sum, lt1.l, lt2.l)

  rtrn <- mapply(`/`, rtrn, lt.s)

  rtrn
}

#' Count percentage of common words to first string
#'
#' Count percentage of common words to the length of first input string. Please remove extra spaces before usage to get the right answer. Vectorized.
#'
#' @param term1 character vector
#' @param term2 character vector
#' @return Number. Percentage (share) of common words between two input strings
#' @seealso am.linwords, am.stems

am.linwordsu <- function(term1, term2){

  lt1 <- str_split(term1, "\\W+")
  lt2 <- str_split(term2, "\\W+")

  rtrn <- mapply(intersect, lt1, lt2)

  rtrn <- lapply(rtrn, length)
  lt1.l <- lapply(lt1, length)

  rtrn <- mapply(`/`, rtrn, lt1.l)

  rtrn
}

#' Count percentage of common stems to first string
#'
#' Count percentage of common stems to the length of first input string. Please remove extra spaces before usage to get the right answer. Not vectorized.
#'
#' @param term1 character vector
#' @param term2 character vector
#' @param minlen number: minimal word length
#' @return Number. Percentage (share) of common words between two input strings
#' @seealso am.linwords, am.linwordsu

am.linstems <- function(term1
                        , term2
                        , minlen = 2){

  term1 <- Corpus(VectorSource(term1))
  term2 <- Corpus(VectorSource(term2))

  term1 <- Terms(DocumentTermMatrix(term1, control = list(removeNumbers=FALSE, stopwords=TRUE, wordLengths=c(eval(minlen), Inf), stemming=TRUE)))
  term2 <- Terms(DocumentTermMatrix(term2, control = list(removeNumbers=FALSE, stopwords=TRUE, wordLengths=c(eval(minlen), Inf), stemming=TRUE)))

  rtrn <- length(intersect(term1, term2))
  rtrn <- rtrn / length(term1)

  rtrn
}





