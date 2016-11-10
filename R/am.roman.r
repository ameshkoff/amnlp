#' Romanization of cyrillic text
#'
#' Cyrillic text to roman letter
#'
#' @param x character
#' @return Character
#' @seealso ...
#' @export

am.roman <- function(x) {

  alphabets.t <- data.table(read.table(text="
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
  ,header=TRUE))
  alphabets.t[is.na(to),to:=""]

  for(i in 1:nrow(alphabets.t) ) {
    x <- gsub(alphabets.t[,from][i], alphabets.t[,to][i], x)
  }

  remove(alphabets.t)
  x
}
