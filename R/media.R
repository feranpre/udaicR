#' @usage
#' --- using data as a data.frame ---
#' media(data.frame)
#'
#' 'variables' must be valid columns of the data.frame
#' media(data.frame, variables = c("var1","var2"))
#'
#' 'by' must be a valid variable of the data.frame
#' media(data.frame, by = "var1", ...)
#'
#' 'by' must have the same length (and order) than the data.frame
#' media(data.frame, by = vector)
#'
#' --- using data as a numeric vector ---
#' media(numeric.vector)
#'
#' 'by' must a vector with the same length as 'data' numeric vector
#' media(numeric.vector, by = vector)
#'
#' @param data               either a vector or a data.frame with all the variables
#' @param variables          name/s of varible/s of 'data' data.frame
#' @param by                 either a string with the data.frame variable name or a vector with the categories
#' @param decimals           number of decimal to show
#' @param show.warnings      show warnings
#' @param show.errors        show errors
#' @param n                  show n (without missing values)
#' @param missing            show number of missing values
#' @param min                show min value (or NA)
#' @param max                show max value (or NA)
#' @param mean               show mean value
#' @param sd                 show standard deviation
#' @param median             show median
#' @param IQR                show range
#' @param norm.test          use udaicR::is.normal(variable) to determine if each variable is normal or not
#' @param col.names          column names for all columns (must set all or none)
#' @param lang               languanje for texts and labels ("en" or "es")
#' @param show.help          logical parameter that, if true, will prompt an interpretation text before the result as an interpretation aid
#'
# @importFrom rlang .data
#' @import dplyr
#' @export
#'
#' @example
#' \dontrun{
#'
#' }
#'
media <- function(data, variables = NA, by = NA, decimals = 2, show_warnings = TRUE, show_errors = FALSE,
                  n=TRUE, missing=TRUE,
                  min=TRUE, max= TRUE, mean=TRUE, sd=TRUE,
                  median=TRUE, IQR=TRUE, norm.test = TRUE,
                  col.names = NA,
                  lang = "en", DEBUG = FALSE, show.help = FALSE) {
  # source("R/normality.R")
  if (!require("dplyr", quietly = TRUE)) {
    stop(error.text[[lang]]["DPLYR"], call. = FALSE)
  }

  stopifnot(!missing(data))

  data.validate <- .media.validate.data(data=data, variables = variables, by=by, lang=lang)

  data.final <- data.validate[["data"]]
  by.name <- data.validate[["by"]]
  variables.names <- data.validate[["variables"]]

  if (is.na(col.names)) {
    if (lang == "es") col.names <- c(var="var",groups="grupos",n="n.validos",missing="n.perdidos",
                                     min="min",max="max",mean="media",sd="desv. est.",median="mediana",IQR="Rang.Inter",norm.test="es.normal")
    else col.names <- c(var="var",groups="groups",n="n.valid",missing="n.missing",
                        min="min",max="max",mean="mean",sd="sd",median="median",IQR="IQR",norm.test="is.normal")
  }

  if(DEBUG) cat("[DEBUG] (media) by.name:", by.name, "\n")

  data.name <- deparse(substitute(data))
  if (length(grep("$",data.name, fixed=TRUE)) == 1) data.name <- sub(".*\\$","",data.name)

  if(!is.data.frame(data)) names(data.final)[1] <- data.name

  result.final <- media.data.frame(data.final, by = by.name, decimals = decimals, DEBUG = DEBUG)

  if (!is.na(by.name)) names(result.final) <- col.names
  else names(result.final) <- col.names[-2]


  attr(result.final, "by") <- by.name
  attr(result.final, "variables") <- variables.names
  attr(result.final, "help") <- show.help
  attr(result.final, "help_text") <- .media.text(lang, "HELP")

  class(result.final) <- append("udaicR_mean", class(result.final))

  if(n == FALSE) result.final[, col.names["n"]] <- NULL
  if(missing == FALSE) result.final[, col.names["missing"]] <- NULL
  if(min == FALSE) result.final[, col.names["min"]] <- NULL
  if(max == FALSE) result.final[, col.names["max"]] <- NULL
  if(mean == FALSE) result.final[, col.names["mean"]] <- NULL
  if(sd == FALSE) result.final[, col.names["sd"]] <- NULL
  if(median == FALSE) result.final[, col.names["median"]] <- NULL
  if(IQR == FALSE) result.final[, col.names["IQR"]] <- NULL
  if(norm.test == FALSE) result.final[, col.names["norm.test"]] <- NULL

  return(result.final)
}



.media.validate.data <- function(data, variables, by, lang = "en") {

  #-- data vector
  if (!is.data.frame(data)) data.final <- as.data.frame(data)
  else data.final <- data

  if(rlang::is_empty(data.final)) stop(.media.error.text(lang,"DATA_EMPTY"))

  check.by = FALSE
  if (length(by) == 1) {
    if (!is.na(by)) check.by = TRUE
  } else if (length(by) > 1) check.by = TRUE


  if(check.by) {
    if ((!is.character(by)) & (!is.factor(by))) stop(.media.error.text(lang,"BY_CLASS"))

    #.. is a string or a factor
    if(length(by) > 1) { #.. is a vector
      by.name <- deparse(substitute(by))
      if (length(grep("$",by.name, fixed=TRUE)) == 1) by.name <- sub(".*\\$","",by.name)
      if (length(by) != nrow(data.final)) stop(.media.error.text(lang,"BY_LENGTH"))
      if (by.name %in% names(data.final)) by.name <- paste0(by.name,"_",sum(names(data.final)==by.name)+1)
      data.final <- cbind(data.final, by)
      names(data.final)[ncol(data.final)] <- by.name
    }
    else if (length(by) == 1) { #.. is the name of the variable
      if (!is.data.frame(data)) stop(.media.error.text(lang,"BY_STRING_NO_DF"))
      if (!(by %in% names(data))) stop(.media.error.text(lang,"BY_NOT_IN_DF"))
      else by.name = by
    }
    else stop(.media.error.text(lang,"BY_GENERIC_ERROR"))
  }
  else by.name <- NA

  check.vars = FALSE
  if (length(variables) == 1) {
    if (!is.na(variables)) check.vars = TRUE
  } else if (length(variables) > 1) check.vars = TRUE

  if(check.vars){
    if(!is.character(variables)) stop(.media.error.text(lang, "VARIABLES_NOT_CHAR"))
    vars.in.data.frame <- sum(variables %in% names(data))
    if(vars.in.data.frame != length(variables)) stop(.media.error.text(lang, "VARIABLES_NOT_IN_DF"))

    #--- AT THIS POINT by is either a var.name or NA and VARIABLES is a vector with valid variable names
    #-- we limit the data.frame to those variables

    if (!is.na(by.name)) variables.tot <- c(by.name, variables)
    else variables.tot <- variables

    data.final <- data.final[, variables.tot]
    names(data.final) <- variables.tot
  }

  data.validate <- list()
  data.validate[["data"]] = data.final
  data.validate[["variables"]] = variables
  data.validate[["by"]] = by.name

  return(data.validate)
}


media.data.frame <- function(data, by, decimals = 2, DEBUG = FALSE){
  grouping = FALSE
  if(!is.na(by)) {
    grouping = TRUE
    by.name <- eval(by)
    if (DEBUG) cat("[DEBUG] (media.data.frame) by:", by.name, "\n")
    levels.list <- levels(as.factor(data[,by.name]))
    if (DEBUG) cat("[DEBUG] (media.data.frame) levels.list:", levels.list, "\n")
    for(l in levels.list){
      d <- data[data[,eval(by)] == l, ]
      r <- lapply(d,.media, decimals = decimals)
      r <- cbind(cat = l,as.data.frame(do.call(rbind, r)))
      r <- cbind(var = rownames(r),r)

      if (!exists("result")) result <- as.data.frame(r)
      else result <- rbind(result, r)
    }
    if(exists("result")){
      result <- arrange(result, var, cat)
      result <- result[rowSums(is.na(result)) != ncol(result) - 2,]
    }
  } else {
    if (DEBUG) cat("[DEBUG] (media.data.frame) No grouping\n")
    result <- lapply(data,.media, decimals = decimals)
    result <- as.data.frame(do.call(rbind, result))
    result <- cbind(Var = rownames(result),result)
    result <- result[rowSums(is.na(result)) != ncol(result) - 1,]
  }

  rownames(result) <- NULL

  #.. if there are no categories then we don't need to "divide" the data.frame
  if (!grouping) result.final <- result
  else {
    for(v in unique(result$var)) {
      if (!exists("result.final")) result.final <- result[result$var == v,]
      else result.final <- rbind(result.final, rep("---",ncol(result)),result[result$var == v,])
    }
  }

  return(result.final)
}



.media <- function(datos, decimals = 2){
  library(dplyr, quietly = TRUE)

  if(is.numeric(datos)) {
    df <- as.data.frame(datos)
    if (sum(is.na(df[,1])) == nrow(df)) result <- NA #.. if its completely empty...
    else {
      result <- df %>% summarize(
        n = n()-sum(is.na(df[,1])),
        missing = sum(is.na(df[,1])),
        min=round(min(df[,1], na.rm = TRUE),digits = decimals),
        max=round(max(df[,1], na.rm = TRUE),digits = decimals),
        mean=round(mean(df[,1], na.rm = TRUE), digits = decimals),
        sd=round(sd(df[,1], na.rm = TRUE),digits = decimals),
        median=round(median(df[,1], na.rm = TRUE),digits = decimals),
        IQR=round(IQR(df[,1], na.rm = TRUE),digits = decimals),
        normal = udaicR::is_normal(df[,1])
      )
    }
  } else result <- NA
  return(result)
}

# ============================================================
#  -------- GENERICS ----------
# ============================================================

#' @export
print.udaicR_mean <- function(obj, ...) {
  if (attr(obj,"help")) cat(attr(obj,"help_text"))
  if ("knitr" %in% rownames(installed.packages()))
    print(knitr::kable(as.data.frame(obj)))
  else
    print(as.data.frame(obj))
}

#' @export
is.udaicR_mean <- function(obj){
  if ("udaicR_mean" %in% class(obj)) return(TRUE)
  return(FALSE)
}

# ============================================================
#  -------- FUNCIONES DE TEXTO ----------
# ============================================================

.media.text <- function(lang = "en", code = "") {
  text <- list()
  text[["en"]] <- c(
    HELP = "
              \nThe mean function returns a data.frame with these columns:
              \n
              \n* n.valid: number of valid (not missing) observations
              \n* n.missing: number of missing observations
              \n* min: minimun
              \n* max: maximun
              \n* mean: mean value
              \n* sd: standard deviation
              \n* median: median
              \n* IQR: interquanntile range
              \n* is.normal:
              \n  - TRUE -> the variable **IS** normal (follows a normal distribution)
              \n  - FALSE-> the variable is **NOT** normal (doesn't follow a normal distribution)
              \n
              \nThe the variable follows a normal distribution we can describe it as:
              \nThe mean age for the participant was XXX years with a stardard deviation (SD) of xxx.
              \n
              \nIf the varible doesn't follow a normal distribution, we use median:
              \nThe median age for the participants was XXX years, with an interquanntile range of XXXX.
              \n"
  )
  text[["es"]] <- c(
    HELP = "
              \nLas funcion devuelve una tabla con las siguientes columnas:
              \n
              \n* n.valid: cuantas observaciones teneis (valores no perdidos)
              \n* n.missing: cuantas respuestas perdidas teneis
              \n* min: valor mínimo
              \n* max: valor máximo
              \n* mean: media
              \n* sd: desviación estandar
              \n* median: mediana
              \n* IQR: rango intercuartílico
              \n* is.normal:
              \n  - TRUE -> signfica que la variable **ES** normal
              \n  - FALSE-> significa que la variable **NO** es normal
              \n
              \nSi la variable ES normal la describimos como:
              \nLa edad media de los participantes era de XX años con una desviación estandar (DE) de xxx.
              \n
              \nSi la variable NO es normal la describimos como:
              \nLa mediana de edad de los participantes era XXX años y su rango intercuartílico es igual a xxxx
              \n"
  )

  if(!is.na(text[[lang]][code])) return(text[[lang]][code])
  else return(text[[lang]]["GENERIC"])
}

.media.error.text <- function(lang = "en", code = "") {
  error.text <- list()
  error.text[["en"]] <- c(
    GENERIC = "generic error in udaicR::media",
    DATA_EMPTY = "'data' vector or data frame has no observations",
    DATA_CLASS = "'data' must be a numeric vector or data.frame",
    VARIABLES_NOT_CHAR = "'variables' no es un vector de texto",
    VARIABLES_NOT_IN_DF = "hay variables indicadas en 'variables' que no se encuentran en el data.frame",
    BY_CLASS = "'by' argument must be either a string with the name of a variable present in the 'data' data.frame or a vector with different categories",
    BY_LENGTH = "'by' vector must be the same length as 'data' vector (or data.frame)",
    BY_STRING_NO_DF = "'by' is a string but 'data' is not a data.frame",
    BY_NOT_IN_DF = "'by' variable is not in 'data' data.frame",
    BY_GENERIC_ERROR = "error with 'by' variable"
  )
  error.text[["es"]] <- c(
    GENERIC = "error generico en la función udaicR::media",
    DATA_EMPTY = "el vector (o data.frame) 'data' no tiene observaciones (esta vacio)",
    DATA_CLASS = "'data' debe ser un vector numerico o un data.frame",
    VARIABLES_NOT_CHAR = "'variables' is not a character vector",
    VARIABLES_NOT_IN_DF = "there are variables indicated in 'variables' that could not be found",
    BY_CLASS = "el argumento 'by' debe ser o una cadena de texto con el nombre de la variable de agrupación dentro del data.frame o un vector con las categorias para usar como grupos",
    BY_LENGTH = "variable de datos y variable de agrupación con longitudes diferentes",
    BY_STRING_NO_DF = "'by' es una cadena de texto pero 'data' no es un data.frame",
    BY_NOT_IN_DF = "la variable de agrupacion 'by' no está presente en el data.frame 'data'",
    BY_GENERIC_ERROR = "error con la variable 'by'"
  )

  if(!is.na(error.text[[lang]][code])) return(error.text[[lang]][code])
  else return(error.text[[lang]]["GENERIC"])
}

