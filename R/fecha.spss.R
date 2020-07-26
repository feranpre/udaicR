#' Title pss2datetime
#'
#' Para obtener la fecha con horas minutos y segundos
#' @param x   Fecha en formato SPSS con HORA
#'
#' @return    Fecha en formato R
#' @export
#'
#' @examples
pss2datetime <- function(x) {
  return(as.POSIXct(x, origin = "1582-10-14", tz="+1"))
}

#' Title spss.date.time
#'
#' Para obtener la fecha con horas minutos y segundos
#' @param x   Fecha en formato SPSS con HORA
#'
#' @return    Fecha en formato R
#' @export
#'
#' @examples
spss.date.time <- pss2datetime


#' Title pss2date
#'
#' Para obtener la fecha
#' @param x   Fecha en formato SPSS sin HORA
#'
#' @return    Fecha en formato R
#' @export
#'
#' @examples
pss2date <- function(x) {
  return(as.Date(x/86400, origin = "1582-10-14"))
}

#' Title spss.date
#'
#' Para obtener la fecha
#' @param x   Fecha en formato SPSS sin HORA
#'
#' @return    Fecha en formato R
#' @export
#'
#' @examples
spss.date <- pss2date
