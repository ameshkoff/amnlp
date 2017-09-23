#' Romanization of the cyrillic character vector. Vectorized
#'
#' Cyrillic letters to roman ones
#'
#' @param x character
#' @return Character
#' @seealso ...
#' @import data.table
#' @import stringr
#' @import stringi
#' @export

am.roman <- function(x) {

  alphabets.t <- fread(input = "
from to
а a
б b
в v
г g
д d
е e
ё e
ж zh
з z
и i
й y
к k
л l
м m
н n
о o
п p
р r
с s
т t
у u
ф f
х h
ц ts
ч ch
ш sh
щ shch
ъ NA
ы y
ь NA
э e
ю iu
я ia"
  , header = TRUE
  , stringsAsFactors = FALSE
  , encoding = "UTF-8")

  alphabets.t[is.na(to), to:=""]

  for(i in 1:nrow(alphabets.t) ) {
    x <- str_replace_all(x, alphabets.t[i, from], alphabets.t[i, to])
  }

  remove(alphabets.t)
  x
}
