

#' Normality Test
#'
#' This function assess if a variable follows the normal distribution. This is done using Shapiro-Wilk's test whenever possible and Lilliefor's correction for Kolmogorov-Smirnoff if Shapiro-Wilk's is not possible.
#'
#' @param variable               vector (numeric if possible) of observations to for wich normality will be assessed
#' @param to.numeric             if the vector is not numeric: do you want to try an automatic conversion?
#' @param decimals               number of decimals for the p value (it will determine the threshold for "<x.xxx1" p value too)
#' @param method                 string with the possible methods: "sw", "lille", "ks" (default: "auto")
#' @param lang                   either "en" for english or "es" for spanish
#' @param show.interpretation    a logical value indicating if you want an interpretation of normality and the specific result you have
#' @param show.theory            a logical value indicating if you want a background explanation of the test
#' @param show.warnings          a logical value indicating wether you want warnings to be shown or not
#'
#' @return It returns list of elements:
#'  * var                       name of the variable tested
#'  * method                    method used to determine normality
#'  * p.value                   exact p value (rounded as per decimals argument)
#'  * p.value.str               STRING value with p value. If p value < 10^-(decimals + 1) then returns "<0.(decimals)1". If decimals = 2 then <0.001
#'  * normal                    logical value indicating if the variable follows a normal distribution or not
#'  * theory                    TEXT theory about normality test
#'  * interpret                 TEXT interpreting the results
#'  * config:
#'     + show.theory            whether or not a theory explanation was solicited
#'     + show.interpretation    whether or not an interpretation was solicited
#'     + lang                   languaje selected for the output
#' @export
#'
#' @examples
norm.test <- function(variable, to.numeric = TRUE, decimals = 2, method = "auto", lang = "en", show.interpretation = FALSE, show.theory = FALSE, show.warnings = TRUE) {
  if (missing(variable)) stop("Variable is missing")
    theory.text <- list()
    theory.text[["es"]] <- paste0(
    "\n\n",
    "---",
    "\n\n**SUPUESTOS DE NORMALIDAD**",
    "\n",
    "\nSe dice que una variable sigue una distribución normal cuando:\n",
    "\n* La media +- 1 distribución estandar incluye al 68% de la muestra",
    "\n* La media +- 2 distribuciones estandar incluye al 95% de la muestra",
    "\n* La media +- 3 distribuciones estandar incluye al 99.7% de la muestra",
    "\n* La simetría está cerca de 0 (<0 cola a la izquierda, >0 cola a la derecha)",
    "\n* La curtosis está cerca de 0 (<0 mesocurtica, >0 platicurtica)",
    "\n",
    "\nPara determinar la normalidad se ha usado tradicionalmente el test de Kolmogorov-Smirnof, pero este test es poco preciso.",
    "\nCuando se comparan tests como Kolmogorov-Smirnof, Shapiro-Wilks, Lilliefors y Anderson-Darling se observa que Shapiro-Wilks es el más potente, mientras que Kolmogorov-Smirnof es el menos potente.",
    "\n",
    "\n",
    "\n**DESCRIPCION DE LA VARIABLE**",
    "\n",
    "\n Si la variable ES normal la descripción se debe hacer mediante media y desviación estandar",
    "\n Si la variable NO es normal la descripción se debe hacer mediante mediana y rango intercuartílico",
    "\n",
    "\n**CITAS**",
    "\n",
    "\n* Razali, NM. and Wah YB, A. (2011). Power comparisons of Shapiro-Wilk, Kolmogorov-Smirnov, Lilliefors and Anderson-Darling test. Journal of Statistical Modeling and Analysis 1(2), 21-33",
    "\n* Mendes, M. and Pala; A. (2003). Type I Error Rate and. Power of Three Normality Tests. Pakistan Journal of Information and Technology 2(2), 135-139. ",
    "\n* Keskin, S. (2006). Comparison of Several Univariate Normality Tests Regarding Type I Error Rate and Power of the Test in Simulation Based Small Sarriples. Journal of Applied Science Research 2(5), pp. 296-300."
  )

  theory.text[["en"]] <- paste0(
    "\n\n",
    "---",
    "\n\n**NORMALITY ASSUMPTIONS**",
    "\n",
    "\nA variable is normal under this assumptions:\n",
    "\n* Mean +- 1 standard desviation inlcudes 68% of the sample",
    "\n* Mean +- 2 standard desviation inlcudes 95% of the sample",
    "\n* Mean +- 3 standard desviation inlcudes 99.7% of the sample",
    "\n* Skewnes is about 0 (<0 is left leaning, >0 is right leaning)",
    "\n* Kurtosis 0 (<0 mesokurtic, >0 platikurtic)",
    "\n",
    "\nOne of the most common methods to determine normality is Kolmogorov-Smirnof, but this test has shown to be very unprecise",
    "\nWhen multiple tests (like Kolmogorov-Smirnof, Shapiro-Wilks, Lilliefors and Anderson-Darling) are compared Shapiro-Wilk's has shown to be most powerful of them and that's why we use it as primary test",
    "\n",
    "\n",
    "\n**DESCRIPTION OF THE VARIABLE**",
    "\n",
    "\nIf the variable DOES follow the normal distribution we should descrive it with mean and standard deviation",
    "\nIf the variable does NOT follow the normal distribution we should descrive it with median and interquanntile range",
    "\n",
    "\n**REFERENCES**",
    "\n",
    "\n* Razali, NM. and Wah YB, A. (2011). Power comparisons of Shapiro-Wilk, Kolmogorov-Smirnov, Lilliefors and Anderson-Darling test. Journal of Statistical Modeling and Analysis 1(2), 21-33",
    "\n* Mendes, M. and Pala; A. (2003). Type I Error Rate and. Power of Three Normality Tests. Pakistan Journal of Information and Technology 2(2), 135-139. ",
    "\n* Keskin, S. (2006). Comparison of Several Univariate Normality Tests Regarding Type I Error Rate and Power of the Test in Simulation Based Small Sarriples. Journal of Applied Science Research 2(5), pp. 296-300."
  )

  warnings.text <- list()
  warnings.text[["en"]] <- c(
                   VAR_NOT_NUM <- "Variable was not numeric, trying conversion",
                   WRONG_LANG <- "Languaje selected is wrong. Use 'en' or 'es'",
                   WRONG_METHOD <- "No valid method was specified ('auto', 'sw', 'ks', 'lillie')",
                   METHOD_NA <- "NA (Not enough observations)",
                   METHOD_NA_EXTENDED <- "Variable has less than 3 observations. Unable to perform normality test"
                   )
  warnings.text[["es"]] <- c(
                   VAR_NOT_NUM <- "El vector no es numérico, intentando conversión",
                   WRONG_LANG <- "Error en selección de idioma. Use 'en' or 'es'",
                   WRONG_METHOD <- "No se ha especificado un método válido ('auto', 'sw', 'ks', 'lillie')",
                   METHOD_NA <- "NA (Faltan observaciones)",
                   METHOD_NA_EXTENDED <- "La variable cuenta con menos de 3 observaciones. No se puede realizar el test de normalidad"
                   )



  if (!is.numeric(variable)) {
    var.data <- tryCatch({
      as.numeric(variable)
    }, warning = function(warn){
      if(show.warnings) warning(warnings.text[[lang]]["VAR_NOT_NUM"])
      return(as.numeric(variable))
    }, error = function(err){
      stop(err)
    })
  } else var.data <- variable

  var.name <- deparse(substitute(variable))
  if (length(grep("$",var.name, fixed=TRUE)) == 1) var.name <- sub(".*\\$","",var.name)

  var.n <- length(var.data) - sum(is.na(var.data))

  if( (method != "auto") & (method != "sw") & (method != "lillie") & (method != "ks") ) {
    if (show.errors) writeLines(warnings.text[[lang]]["WRONG_METHOD"])
    method.final = "auto"
  }
  else if (method == "auto") {
    if((var.n > 3) & (var.n < 5000)) method.final <- "sw"
    else if (var.n > 3) method.final <- "lillie"
    else method.final <- "NO"
  }
  else method.final = method

  if(method.final == "sw") {
    test <- tryCatch({
                shapiro.test(var.data)
              }, warning = function(warn) {
                  if(show.warnings) warning(warn)
                  return(shapiro.test(var.data))
              }, error = function(err) {
                  stop(err)
                }
              )

    var.p <- test$p.value
    var.normal <- ifelse(var.p < 0.05, FALSE, TRUE)
    var.method = "Shapiro-Wilk"
  }
  else if(method.final == "lillie") {
    test <- tryCatch({
      nortest::lillie.test(var.data)
    }, warning = function(warn) {
      if(show.warnings) warning(warn)
      return(nortest::lillie.test(var.data))
    }, error = function(err) {
      stop(err)
    }
    )
    var.p <- test$p.value
    var.normal <- ifelse(var.p < 0.05, FALSE, TRUE)
    var.method = "Kolmogorov-Smirnof (Lilliefor's correction)"
  }
  else if (method.final == "NO"){
    var.p <- NA
    var.normal <- FALSE
    var.method = warnings.text[[lang]]["METHOD_NA"]
    warning(warnings.text[[lang]]["METHOD_NA_EXTENDED"])
  }
  else if (method.final == "ks"){
    test <- tryCatch({
      ks.test(var.data,"pnorm")
    }, warning = function(warn) {
      if(show.warnings) warning(warn)
      return(ks.test(var.data,"pnorm"))
    }, error = function(err) {
      stop(err)
    }
    )
    var.p <- test$p.value
    var.normal <- ifelse(var.p < 0.05, FALSE, TRUE)
    var.method = "Kolmogorov-Smirnof"
  }


  if (var.p < 10^( (decimals+1) *-1 )) var.p.final = paste0("<",10^( (decimals+1) *-1 ))
  else var.p.final = round(var.p, digits = decimals)

  var.p  = round(var.p, digits = decimals)

  if (lang == "es") {
    # warnings.text <- warnings.es
    # theory.text <- theory.es
    interpret.text <- paste0(  "\n\n",
                               "---",
                               "\n**Interpretación** \n\nLa variable ha sido examinada usando el test '",var.method,"' obteniendo un valor p ",ifelse(substr(var.p.final,1,1) == "<",var.p.final, paste("=",var.p.final)), sep = "")
    if (var.p <0.05) interpret.text <- paste0(interpret.text, "\n\nAsumiendo un riesgo alpha de 0.05 podemos afirmar que la variable **NO** sigue una distribución normal (p<0.05)")
    else interpret.text <- paste0(interpret.text, "\n\nAsumiendo un riesgo alpha de 0.05 podemos afirmar que la variable **SI** sigue una distribución normal (p>0.05)")
  }
  else {
    #-- if not spanish assume english
    # warnings.text <- warnings.en
    # theory.text <- theory.en
    interpret.text <- paste0(  "\n\n",
                               "---",
                               "\n**Interpretation** \n\nThe variable of interest have been assessed using '",var.method,"' test. The p value for the test is ",ifelse(substr(var.p.final,1,1) == "<",var.p.final, paste("=",var.p.final)), sep = "")
    if(var.p < 0.05) interpret.text <- paste0(interpret.text, "\n\nAccepting an alpha risk of 0.05 we can say that the varible **does NOT** follow a normal distribution (p<0.05)")
    else interpret.text <- paste0(interpret.text, "\n\nAccepting an alpha risk of 0.05 we can say that the varible **DOES** follow a normal distribution (p>0.05)")

    #-- if not english throw warning but still show english output
    if(lang != "en") warning(warnings.text[[lang]]["WRONG_LANG"])
  }



  r <- list(var = var.name, method=var.method, p.value=var.p, p.value.str = var.p.final, normal=var.normal,
            theory = theory.text[[lang]],
            interpret = interpret.text,
            config = list(show.theory = show.theory, show.interpretation = show.interpretation, lang = lang))
  class(r) <- append(class(r), "udaicR.norm")
  return(r)
}

#' @export
print.udaicR.norm <- function(obj){
  cat("Variable:   ",obj$var," \n")
  cat("Test:       ",obj$method," \n")
  cat("p:          ",obj$p.value.str," \n")
  cat("is.normal:  ",obj$normal," \n")
  if (obj$config$show.interpretation) writeLines(obj$interpret)
  if (obj$config$show.theory) {
    cat(obj$theory)
  }
}



#' Check for normality
#' is.normal() gives a TRUE/FALSE value based on normality test of the variable.
#'
#' @param variable       vector of values (ideally numbers)
#' @param show.warnings  Default FALSE. Whether or not you want to get possible warnings
#'
#' @return logical value (TRUE/FALSE)
#' @export
#'
#' @examples
#'
#' is.normal(c(1,76,3,2,1,23,5,64,234,12,3,43,2324,54,45,223,341,213,2,4323,24,3,54,54,56,76,7,55,3,2323))
#'
is_normal <- function(variable, show.warnings = FALSE, decimals = 2, DEBUG = FALSE){
    norm <- tryCatch({
      norm.test(variable, show.warnings = show.warnings, decimals = decimals)
    }, warning = function(warn) {
      if(show.warnings) warning(warn)
      return(norm.test(variable, show.warnings = show.warnings, decimals = decimals))
    }, error = function(err) {
      print(err)
    })

    var.name <- deparse(substitute(variable))
    if (length(grep("$",var.name, fixed=TRUE)) == 1) var.name <- sub(".*\\$","",var.name)

    if(DEBUG) cat("[DEBUG] is.normal[var.name]:",var.name,"\n")
    result <- norm$normal

    #--- this is my trick to return a logical value and yet make a better print function that includes the vector name passed
    class(result) <- append(class(result), c(var.name, "udaicR.is.normal"))
    return(result)
}

#' @export
print.udaicR.is.normal <- function(obj)
{
  clase <- class(obj)
  result <- clase[length(clase)-1] #--- getting the variable name from the class (we put it there in the is.normal function)
  if (obj == FALSE) cat(result,": FALSE")
  else if (obj == TRUE) writeLines(result,": TRUE")

}

