error.text <- list()
error.text[["es"]] <- c(MISSING_VAR = "no se ha indicado 'variable'",
                        DATA_TYPE = "el primer parámetro debe ser un data.frame, o un vector de números",
                        NO_NUM = "se ha detectado una variable o vector que no es un número",
                        NUMERIC_NOT_FOUND = "no se ha detectado ninguna variable numérica aceptable para la correlación",
                        DIFF_LENGTH = "algun vector o variable tiene un tamaño diferente al resto",
                        MISSING_DATA = "'variable' se ha indicado como texto sin pasar base de datos en 'data'",
                        MISSING_IN_DATA.FRAME = " no existe en la base de datos", # hay que pegarle antes el nombre de la variable,
                        WRONG_LANG <- "Error en selección de idioma. Use 'en' or 'es'",
                        ONLY_1_VECTOR = "La correlación no puede hacerse solo con una variable",
                        TIES.SPEARMAN = "Se han encontrado empates, usando la tau de Kendall.\n",
                        TIES.KWNDALL = "Se han encontrado empates incluso usando la tau de Kendall.\n",
                        DPLYR = "El paquete \"dplyr\" es necesario para poder usar esta función. Por favor instalelo con install.packages(\"dplyr\")"
                        )



error.text[["en"]] <- c(MISSING_VAR = "'variable' is missing",
                       DATA_TYPE = "the first argument must be either a data.frame or a numeric vector",
                       NO_NUM = "non numeric variable or vector detected",
                       NUMERIC_NOT_FOUND = "no numeric variable has been detected, therefore the correlation can't procede",
                       DIFF_LENGTH = "vector or variable of different length to the rest",
                       MISSING_DATA = "'variable' is a string but no data.frame was specified in 'data'",
                       MISSING_IN_DATA.FRAME = " not pressent in the data.frame", # hay que pegarle antes el nombre de la variable,
                       WRONG_LANG <- "Languaje selected is wrong. Use 'en' or 'es'",
                       ONLY_1_VECTOR = "Correlation needs more than 1 variable",
                       TIES.SPEARMAN = "There are ties in the data, using Kendall tau instead \n",
                       TIES.KWNDALL = "There are ties in the data, even after using Kendall's tau.\n",
                       DPLYR = "Package \"dplyr\" needed for this function to work. Please install it using install.packages(\"dplyr\")."
                      )

theory.text <- list()
theory.text[["es"]] <- paste0(
                "\n**LA CORRELACION** \n\n",
                "\nLa correlación indica la fuerza y dirección de una relación lineal y proporcional entre dos variables numéricas. Lo que indica el valor de la correlación es qué ocurre a la variable B cuando una variable A modifica su valor. Digamos que A incrementa su valor, entonces si la correlación es positiva B también incrementará su valor. Si la correlación es negativa entonces B decrementa su valor y se dice que la correlación es inversa.\n",
                "\n**VALORACION DEL RESULTADO** \n\n",
                "\nLos valores de la correlación van de -1 a 1, siendo 0 un efecto nulo, 1 una correlación perfecta y -1 una correlación perfecta e inversa.",
                "\nAl margen de los extremos (1 y -1) que se consideran perfectos, los valores de corte para las interpretaciones son muy variados, nosotros hemos usado los siguientes:\n",
                "\n* (> 0     y < 0.25) - Debil",
                "\n* (>= 0.25 y < 0.5)  - Moderada",
                "\n* (>= 0.5  y < 0.75) - Fuerte",
                "\n* (>= 0.75)          - Muy Fuerte",
                "\n",
                "\nAdemás hace falta tener en cuenta que existen diferentes métodos para calcular las correlaciones:\n",
                "\n* Si ambas variables siguen una distribución normal:  correlación de Pearson",
                "\n* Si alguna variable no sigue una correlación normal: correlación de Spearman",
                "\n* Si la correlación de Spearman tiene empates:        tau de Kendall"
              )

theory.text[["en"]] <- paste0(
                      "\n**CORRELATION** \n\n",
                      "\nCorrelation indicate the strength and direction of the association (linear and proportional) between two numeric variables. The value of the correlation means what happends to values of variable B when the values of A change. If the values of A increase and the correlation is positive, then the values of B will also increse. If the correlation is negative then the values of B will decrease and we say that there's an inverse correlation.\n",
                      "\n**RESULT ASSESSMENT** \n\n",
                      "\nThe correlation value goes from -1 to 1, being 0 the null effect. A value of 1 means that the correlation is 'perfect' while a value of -1 means that it's 'perfectly inverse'",
                      "\nOther than the extreme values (1 and -1), that we can consider 'perfect', other values have cutoff points to facilitate interpretation. These values may vary from author to author. We've chosen these cutoff points:\n",
                      "\n* (> 0     y < 0.25) - Weak",
                      "\n* (>= 0.25 y < 0.5)  - Moderate",
                      "\n* (>= 0.5  y < 0.75) - Strong",
                      "\n* (>= 0.75)          - Very Strong",
                      "\n",
                      "\nThere are different ways to calculate the correlation value:\n",
                      "\n* Both variables (A and B) follow a normal distribution        - Pearson's correlation",
                      "\n* Any or both of variables do NOT follow a normal distribution - Spearman's rho",
                      "\n* Spearman's correlation present ties                          - Kendall's tau"
                    )

#---> https://adv-r.hadley.nz/s3.html
# para leer
# setClass("udaic.cor",
#          slots = c(
#            # main = "data.frame",
#            # cor = "data.frame",
#            # p = "data.frame",
#            # methods = "data.frame",
#            # interpretation = "data.frame",
#            # theory = "character",
#            # lang = "character",
#            # call.method = "character"
#            name = "character"
#          )
#        )
#
# udaic.cor <- function(name){
#   new("udaic.cor", name)
# }
#
# setMethod("show", "udaic.cor", function(obj){
#   cat(obj@name)
# })


#' correlation
#'
#' @param data
#' @param ...
#' @param decimals
#' @param show.errors
#' @param show.warnings
#' @param DEBUG
#' @param p.sig
#' @param spearman.cutoff
#' @param spearman.labels
#' @param pearson.cutoff
#' @param pearson.labels
#' @param kendall.cutoff
#' @param kendall.labels
#' @param inv.string
#' @param method
#' @param lang
#'
#' @return
#' @export
#'
#' @examples
correlation <- function(data, ..., decimals = 2,
                 show.errors = TRUE, show.warnings = TRUE, DEBUG = FALSE,
                 p.sig = 0.05,
                 spearman.cutoff = c(0.25,0.5,0.7), spearman.labels = c("weak", "moderate", "strong"),
                 pearson.cutoff = c(0.25,0.5,0.7), pearson.labels = c("weak", "moderate", "strong"),
                 kendall.cutoff = c(0.25,0.5,0.7), kendall.labels = c("weak", "moderate", "strong"),
                 inv.string = "(inv)",
                 method = c("auto", "pearson", "spearman", "kendall"), lang = c("en","es")) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop(error.text[[lang]]["DPLYR"], call. = FALSE)
  }


  # requireMethods("print", "udaicR.correlation")
  # source("R/correlation.print.R")
  # importFrom(udaicR, is.normal)
  # stopifnot(!missing(data))

  suppressMessages(suppressWarnings(library(dplyr)))

  lang <- rlang::arg_match(lang)
  method <- rlang::arg_match(method)


  #============================================================================= FORMING WORKING DATA
  # ------------------------------------------------- NOT A DATA.FRAME
  if(!is.data.frame(data)) {
    vars <- enquos(...)
    vars.num <- length(vars)


    if (vars.num <= 0) stop(error.text[[lang]]["ONLY_1_VECTOR"])

    if (!is.numeric(data)) data <- as.numeric(v)
    DATOS <- as.data.frame(data)
    names(DATOS) <- .get.var.name(substitute(data))

    for(v in list(...)) {
      if (!is.numeric(v)) v <- as.numeric(v)
      DATOS <- cbind(DATOS,v)
    }
    var.names <- names(DATOS)[1]
    for(v in vars) {
      var.n <- .get.var.name(v)
      if (var.n %in% var.names) var.n <- paste0(var.n,"_", length(var.names))
      var.names <- c(var.names, var.n)

    }
    names(DATOS)<- var.names
  }
  # ------------------------------------------------- IS a DATA.FRAME
  else if (is.data.frame(data)) {

    for(v.name in names(data)) {
      v <- data[, v.name]
      if(!is.numeric(v)) v <- as.numeric(v)
      if(!exists("DATOS")) {
        DATOS <- as.data.frame(v)
        var.names <- c(v.name)
      }
      else {
        DATOS <- cbind(DATOS,v)
        if (v.name %in% var.names) v.name <- paste0(v.name,"_", length(var.names))
        var.names <- c(var.names,v.name)
      }
    }
    names(DATOS) <- names(data)
    DATOS <- as.data.frame(DATOS)
  }
  #------------------------------------------------ DATOS is the working data.frame
  if (!exists("DATOS")) {
    if (show.warnings) print(error.text[[lang]]["NUMERIC_NOT_FOUND"])
    return(NA)
  }

  resultados = .correlation(DATOS,  show.errors = show.errors, show.warnings = show.warnings, decimals = decimals, method = method, lang = lang)

  interp = .corr.interpretation(resultados$cor, resultados$p, resultados$method, spearman.cutoff = spearman.cutoff, spearman.labels = spearman.labels,
                                pearson.cutoff = pearson.cutoff, pearson.labels = pearson.labels,
                                kendall.cutoff = kendall.cutoff, kendall.labels = kendall.labels, p.sig = p.sig, inv.string = inv.string)


  corr <- .new_correlation(original.data = DATOS, main=resultados$main, corr.matrix = resultados$cor,
                          p.matrix = resultados$p, method.matrix = resultados$method, method = method,
                          theory.text = theory.text[[lang]],
                          interpretation.matrix = interp)
  return(corr)

}


#----- CONSTRUCTOR
#' Title
#'
#' @param original.data
#' @param main
#' @param corr.matrix
#' @param p.matrix
#' @param method.matrix
#' @param method
#' @param theory.text
#' @param interpretation.matrix
#'
#' @return
#' @export
#'
#' @examples
.new_correlation <- function(original.data = data.frame(), main = data.frame(),
                            corr.matrix = data.frame(), p.matrix = data.frame(),
                            method.matrix = data.frame(),
                            method = c("auto", "sw", "lillie", "ks"), theory.text = character(),
                            interpretation.matrix = data.frame()){

  stopifnot(is.data.frame(original.data))

  obj <- list(main = main,
              extra = list(
                original.data = original.data,
                main.matrix = main,
                corr.matrix = corr.matrix,
                p.matrix = p.matrix,
                method.matrix = method.matrix,
                interpretation.matrix = interpretation.matrix,
                method = method,
                theory.text = theory.text
              )
              )
  # print(obj)
  class(obj) <- append(class(obj), "udaicR_correlation")



  # -- hacer calculos y añadir slots a la clase para los resultados
  return(obj)
}

.corr.interpretation <- function(cor.data.frame, p.data.frame, method.data.frame, spearman.cutoff, spearman.labels,
                                 pearson.cutoff, pearson.labels,
                                 kendall.cutoff, kendall.labels, inv.string, p.sig){

  cutoff.points <- list()
  cutoff.points[["pearson"]] <- pearson.cutoff
  cutoff.points[["spearman"]] <- spearman.cutoff
  cutoff.points[["kendall"]] <- kendall.cutoff

  cutoff.labels <- list()
  cutoff.labels[["pearson"]] <- pearson.labels
  cutoff.labels[["spearman"]] <- spearman.labels
  cutoff.labels[["kendall"]] <- kendall.labels

  for(vx in names(cor.data.frame)) {
    vector <- "--"
    for(vy in rownames(cor.data.frame)){
      valor  <- cor.data.frame[vy,vx]
      metodo <- method.data.frame[vy,vx]
      # print(paste("V ->", valor, "  M ->",metodo))
      if (!is.na(metodo)) {

        for(cutoff in length(cutoff.points):1) {
          # print(paste("V ->", valor, "  M ->",metodo, " P --> ", p.data.frame[vx,vy]))
          int.text <- "-"

          if ((abs(valor) < cutoff.points[[metodo]][cutoff]) & (p.data.frame[vx,vy] < p.sig)) {
            if (valor < 0) int.text <- paste0(cutoff.labels[[metodo]][cutoff], inv.string)
            else int.text <- cutoff.labels[[metodo]][cutoff]
          }
        } # -- for cutoff

        vector <- c(vector,int.text)
      }
      else vector <- c(vector, "NA")
    }

    vector <- vector[-1]
    if (!exists("resultados.interpretacion")) resultados.interpretacion <- as.data.frame(vector)
    else resultados.interpretacion <- cbind(resultados.interpretacion, vector)
  }

  # print(resultados.interpretacion)
  names(resultados.interpretacion) <- names(cor.data.frame)
  rownames(resultados.interpretacion) <- rownames(cor.data.frame)

  return(resultados.interpretacion)
}


.get.var.name <- function(var){
  if(is.call(var)) var <- deparse(var)

  var.name <- var
  if (length(grep("$",var.name, fixed=TRUE)) == 1) var.name <- sub(".*\\$","",var.name)
  return(var.name)
}


#' print.udaicR_correlation
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
print.udaicR_correlation <- function(obj){
  print(obj$main)
}



.correlation <- function(datos, decimals, DEBUG, show.warnings, show.errors, method, lang) {
  variables_X <- names(datos)
  variables_Y <- variables_X
  method.picked <- method

  for(varx in variables_X) {

    for(vary in variables_Y) {
      varx.datos <- datos[,varx]
      vary.datos <- datos[,vary]

      varx.n <- sum(!is.na(varx.datos))
      vary.n <- sum(!is.na(vary.datos))

      if (varx == vary) {
        rho <- 1
        p.rho <- NA
        method <- NA
      }
      else {
        var.complete <- !is.na(varx.datos) & !is.na(vary.datos)
        var.total <- sum(var.complete)
        if (var.total > 1){
          if (method.picked == "auto") {

            varx.normal <- udaicR::is.normal(varx.datos)
            vary.normal <- udaicR::is.normal(vary.datos)


            if (varx.normal & vary.normal) {
              rho <- round(cor(varx.datos, vary.datos, method = "pearson", use = "complete.obs"), digits = decimals)
              p.rho <- round(cor.test(varx.datos, vary.datos, method = "pearson", use = "complete.obs")$p.value, digits = decimals)
              method <- "pearson"
            }
            else {


              # --------------------------------------- checkinf for ties in Spearman rho
              ties <- tryCatch({
                cor.test(varx.datos, vary.datos, method = "spearman", use = "pairwise.complete.obs")$p.value
              }, warning = function(warn)  {
                if (warn$message == "Cannot compute exact p-value with ties") {
                  if (show.warnings) warning(paste(error.text[[lang]]["TIES.SPEARMAN"],warn))
                  return(TRUE)
                }

              }, error = function(err) {
                if (show.errors) cat(paste("\n ERROR udaicR::correlations ->",err))
                return(TRUE)
              }
              )

              if (is.numeric(ties)) {
                rho <- round(cor(varx.datos, vary.datos, method = "spearman", use="pairwise.complete.obs"), digits = decimals)
                p.rho <- round(cor.test(varx.datos, vary.datos, method = "spearman", use = "pairwise.complete.obs")$p.value, digits = decimals)
                method <- "spearman"
              }
              else  {
                # ---------- checkinf for ties in Kendall tau
                ties <- tryCatch({
                  cor.test(varx.datos, vary.datos, method = "kendall", use = "pairwise.complete.obs")$p.value
                }, warning = function(warn)  {
                  if (warn$message == "Cannot compute exact p-value with ties") {
                    if (show.warnings) warning(paste(error.text[[lang]]["TIES.KENDALL"],warn))
                    return(TRUE)
                  }

                }, error = function(err) {
                  if (show.errors) cat(paste("\nERROR udaicR::correlations ->",err))
                  return(TRUE)
                }
                )
                if (is.numeric(ties)) {
                  rho <- round(cor(varx.datos, vary.datos, method = "kendall", use="pairwise.complete.obs"), digits = decimals)
                  p.rho <- round(cor.test(varx.datos, vary.datos, method = "kendall", use = "pairwise.complete.obs")$p.value, digits = decimals)
                  method <- "kendall"
                } else {
                  rho <- round(cor(varx.datos, vary.datos, method = "kendall", use="pairwise.complete.obs"), digits = decimals)
                  p.rho <- round(cor.test(varx.datos, vary.datos, method = "kendall", use = "pairwise.complete.obs", exact = FALSE)$p.value, digits = decimals)
                  method <- "kendall"

                } #--- kendall ties
              }# --- spearman ties
            } # --- vars not normal
          }
          else {
            rho <- round(cor(varx.datos, vary.datos, method = method.picked, use = "pairwise.complete.obs"), digits = decimals)
            p.rho <- round(cor.test(varx.datos, vary.datos, method = method.picked, use = "pairwise.complete.obs")$p.value, digits = decimals)
            method <- method.picked
          } # --- method is not FALSE

        } # ------------------------------------- end if(var.total > 1)
        else { #-------------------------------------------------------- var.total <= 1
          rho <- NA
          p.rho <- NA
          method <- NA
        }
      }


      if (!is.na(p.rho)) {
        if (p.rho < 10^((decimals + 1)*-1)) p.rho.text = paste0("<",10^((decimals + 1)*-1))
        else p.rho.text <- p.rho
      } else p.rho.text <- "-"

      # ================================================================== FORMANDO VECTORES DE RESULTADOS
      if(!exists("cor.rho")) {
        cor.rho <- rho
        cor.method <- method
        cor.p <- p.rho
        cor.p.text <- p.rho.text
        cor.main <- paste0(rho,"(",p.rho.text,")[",substr(method,1,1),"]")
      }
      else {
        cor.rho <- c(cor.rho, rho)
        cor.method <- c(cor.method,method)
        cor.p <- c(cor.p,p.rho)
        cor.p.text <- c(cor.p.text, p.rho.text)
        cor.main <- c(cor.main,paste0(rho,"(",p.rho.text,")[",substr(method,1,1),"]"))
      }
    } # --- for interno

    # ================================================================== UNIENDO VECTORES DE RESULTADOS a los DATA.FRAME de resultados
    if(!exists("result_temp.rho")) {
      result_temp.rho <- data.frame(cor.rho)
      result_temp.P <- data.frame(cor.p)
      result_temp.P.text <- data.frame(cor.p.text)
      result_temp.method <- data.frame(cor.method)
      result_temp.main <- data.frame(cor.main)
    }
    else {
      result_temp.rho <- cbind(result_temp.rho, cor.rho)
      result_temp.P <- cbind(result_temp.P, cor.p)
      result_temp.P.text <- cbind(result_temp.P.text, cor.p.text)
      result_temp.method <- cbind(result_temp.method, cor.method)
      result_temp.main <- cbind(result_temp.main, cor.main)
    }

    cor.rho <- NULL
    cor.method <- NULL
    cor.p <- NULL
    cor.p.text <- NULL
    cor.varY <- NULL
    cor.main <- NULL

  } # --- for externo

  #====================================================================== NOMBRES A FILAS Y COLUMNAS
  rownames(result_temp.rho) <- names(datos)
  rownames(result_temp.P) <- names(datos)
  rownames(result_temp.P.text) <- names(datos)
  rownames(result_temp.method) <- names(datos)
  rownames(result_temp.main) <- names(datos)
  names(result_temp.rho) <- names(datos)
  names(result_temp.P) <- names(datos)
  names(result_temp.P.text) <- names(datos)
  names(result_temp.method) <- names(datos)
  names(result_temp.main) <- names(datos)

  resultados = list(main = result_temp.main, cor = result_temp.rho, p = result_temp.P, p.text = result_temp.P, method = result_temp.method)

  return(resultados)
}

