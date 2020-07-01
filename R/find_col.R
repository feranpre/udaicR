#' Title
#'
#' @import stringr
#'
#' @param data              data.frame used
#' @param string            quoted string to search for (doesn't need wildcards) "dat" will look for "data" as well as "date"
#' @param show.col.number   sometimes it's usefull to know the number of the columns found, this is where you ask for it
#'
#' @return list of names that contains the string with or without the column number besides it
#' @export
#'
#' @examples
find_col <- function(data, string="", show.col.number= FALSE) {
  if (!show.col.number) return(names(data)[grep(regex(string),names(data), ignore.case=TRUE)]  )
  return(grep(regex(string),names(data), ignore.case=TRUE))
}


busca_col <- find_col
