#' ---
#' title: "Resultados temporales"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'    html_document:
#'         toc: yes
#' ---

#+ , echo=FALSE, include=FALSE
rm(list=ls())
rm(list = ls())

library(udaicR)
data_ <- data.frame(AGE=sample(x = 65:100, size=30, replace = TRUE ),
                    HEIGHT=sample(x = 120:205, size=30, replace = TRUE ),
                    SEX=sample(x = c("Male", "Female"), prob = c(.5,.5), size = 30, replace = TRUE),
                    BLOND=sample(x = c("Yes", "No"), prob = c(.2,.8), size = 30, replace = TRUE)
)
data_ <- rbind(data_, list(34,NA,NA,NA))
data_ <- rbind(data_, list(33,NA,"Male",NA))
data_ <- rbind(data_, list(22,NA,NA,"No"))
data_$EMPTY <- rep(NA,nrow(data_))
source("R/correlation.R")
source("R/normality.R")
#'
#' # NORMALIDAD
#'
#' ## is.normal
#'
#+ , results = 'asis', echo = FALSE, message = FALSE, warning = FALSE, eval =TRUE, width = 25, height = 25, unit = "cm"
is.normal(data_$AGE)

#'
#' ## Tests de normalidad - SIN explicacion
#'
#+ , results = 'asis', echo = FALSE, message = FALSE, warning = FALSE, eval =TRUE, width = 25, height = 25, unit = "cm"
norm.test(data_$AGE)


#'
#' ## Tests de normalidad - CON explicacion
#'
#+ , echo = FALSE, message = FALSE, warning = FALSE, eval =TRUE, width = 25, height = 25, unit = "cm", results = 'asis'
norm.test(data_$AGE, show.interpretation = TRUE)



#'
#' ## Tests de normalidad - CON teoria en español
#'
#+ , echo = FALSE, message = FALSE, warning = FALSE, eval =TRUE, width = 25, height = 25, unit = "cm", results = 'asis'
norm.test(data_$AGE, show.theory = TRUE, lang = "es")

#'
#'
#' # CORRELACION
#+ , echo = FALSE, message = FALSE, warning = FALSE, eval =TRUE, fig.width = 25, fig.height = 25, unit = "cm", paged.print=TRUE, results = 'asis'
data("mtcars")
cor <- correlation(mtcars[,c("mpg","cyl","wt","drat","carb")], show.warnings = FALSE, show.theory = TRUE)

cat(cor$interpretacion_texto)

knitr::kable(cor$main)
knitr::kable(cor$interpretacion)
