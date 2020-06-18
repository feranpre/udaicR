#Para obtener la fecha con horas minutos y segundos
pss2datetime <- function(x) as.POSIXct(x, origin = "1582-10-14", tz="+1")


#Para obtener la fecha sin hora

pss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")
