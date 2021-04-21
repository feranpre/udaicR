.get.call.details <- function(f=NULL){
  origen = 0
  for(num in 1:sys.nframe()){
    origen = num-1
    # print(sys.call(which=num*-1))
    if (is.null(sys.call(which=num*-1))) break
    if (sys.call(which=num*-1) == "eval(ei, envir)") break

  }

  if(missing(f))  ORIGINAL.CALL <- deparse(sys.call(sys.parent(n=origen)), width.cutoff = 500L)
  else ORIGINAL.CALL <- f

  #----- if function call is longer than 500 char will be splited and I don't want it splitted
  if (length(ORIGINAL.CALL) > 1) {
    for (i in 1:length(ORIGINAL.CALL)) {
      if (!exists("TEMP.CALL")) TEMP.CALL <- ORIGINAL.CALL[i]
      else TEMP.CALL <- paste0(TEMP.CALL, ORIGINAL.CALL[i])
    }
    ORIGINAL.CALL <- TEMP.CALL
    rm(list="TEMP.CALL")
  }

  temp.call <- strsplit(ORIGINAL.CALL,"\\(")[[1]]
  FUNC.NAME <- temp.call[1]

  # cat("\n\nFUNCTION -> ", FUNC.NAME,"\n")
  if (length(temp.call) <= 1) return(FUNC.NAME) #---------------------- create function.call object
  temp.args <- stringr::str_extract(ORIGINAL.CALL, '(?<=\\().*(?=\\))')
  # print(ORIGINAL.CALL)
  # temp.args <- strsplit(temp.args,",")[[1]]
  temp.args <- strsplit(temp.args, ",(?![^()]*\\))", perl = T)[[1]]
  TOTAL.ARGS <- length(temp.args)
  # print(temp.args)
  arguments.list <- list()
  unnamed.arguments.list <- list()
  for(arg.num in 1:TOTAL.ARGS){
    arg.text <- temp.args[arg.num]
    # arg.split <- strsplit(temp.args[arg.num],"=")[[1]]
    arg.name <- stringr::str_extract(arg.text, '.*(?==)') # --- cogemos lo que hay ANTES de = si es que hay algo
    arg.value <- stringr::str_extract(arg.text, '(?<==)(?![^\\(]*\\))(.*)') # --- cogemos lo que hay DETRAS de '=' si hay algo y de paso IGNORAMOS lo que haya dentro de '()', espara evitar confundir 'by=c(asdf=asdf,sadf=asdf)'
    arg.value <- gsub("\"", "", arg.value)
    # print(paste("TEXT: ", arg.text))
    # print(paste("NAME: ", arg.name))
    # print(paste("VALUE: ", arg.value))
    #--- if there is a named argument BOTH must be not NA, else there is no =
    if (is.na(arg.name) | is.na(arg.value)) {
      #-- if any of them are NA we have a not-named argument
      # print("ISNA")
      arg.value <- temp.args[arg.num]
      arg.name = "UNNAMED"
      arg.value <- gsub("\"", "", arg.value)
      #if ther is a vector like c("AGE", "HEIGHT")

      if (grepl("c(",arg.value, fixed = TRUE)) {
        arg.value <- stringr::str_extract(arg.value, '(?<=\\().*(?=\\))')
        arg.value <- strsplit(arg.value, ",",perl = T)[[1]]
      }
      #-- quitamos el simbolo de $ y nos aseguramos de que no se repite el nombre
      if(length(arg.value)>1) {
        arg.value.temp <- arg.value
        arg.value <- NULL
        for(a in arg.value.temp) {
          if (grepl("$", a, fixed=TRUE)) {
            if (exists("arg.value")) arg.value <- c(arg.value, stringr::str_extract(a, '(?<=\\$).*'))
            else arg.value <- stringr::str_extract(a, '(?<=\\$).*')
          }
          else {
            if (exists("arg.value")) arg.value <- c(arg.value, a)
            else arg.value <- a
          }

        }
      }
      else {
        if (grepl("$", arg.value, fixed=TRUE)) arg.value <- stringr::str_extract(arg.value, '(?<=\\$).*')
      }

    }
    arg.name <- gsub(" ","",arg.name)
    arguments.list[[arg.name]] <- c(arguments.list[[arg.name]],gsub(" ","",arg.value))

  }
  # print(arguments.list)
  # attr(ORIGINAL.CALL,"ORIGINAL.CALL") <- FUNC.NAME
  attr(ORIGINAL.CALL,"FUN") <- FUNC.NAME
  attr(ORIGINAL.CALL,"TOTAL.ARGS") <- TOTAL.ARGS
  attr(ORIGINAL.CALL, "LIST.ARGS") <- arguments.list
  attr(ORIGINAL.CALL, "LIST.UNNAMED.ARGS") <- arguments.list$UNNAMED
  class(ORIGINAL.CALL) <- "udaicR.function.parse"

  return(ORIGINAL.CALL)
}




forma.datos <- function(..., by = NULL, DEBUG = FALSE, DEBUG.CALL = FALSE) {
  parametros <- list(...)
  CALL_DETAILS = .get.call.details()
  CALL_DETAILS.UNNAMED = attr(CALL_DETAILS, "LIST.UNNAMED.ARGS")

  if (DEBUG.CALL){
    cat("\n ------------------- CALL ----------------------- \n")
    print(CALL_DETAILS)
    cat("\n ------------------------------------------------ \n")
  }

  # TEMP.DATA <- data.frame()
  max.length = 0
  for(p in parametros) {

    if (is.data.frame(p)) temp.length = nrow(p)
    else temp.length = length(p)
    if (temp.length > max.length) max.length <- temp.length
  }
  # p.lengths <- lapply(parametros, length)
  # max.length= max(unlist(p.lengths))
  if(DEBUG) cat("\n\n[forma.datos] max.lengt:", max.length,"\n")

  #=================================================================== FORMING PARAMETERS -> DATA.FRAME
  param.number = 0
  for(p in parametros) {
    param.number = param.number + 1
    #----------------------------------------- parameter IS a data.frame
    if (is.data.frame(p)) {
      if (DEBUG) cat("\n[forma.datos] data.frame","\n")
      if (!exists("RAW.DATA")) RAW.DATA <- p
      else {
        if (nrow(RAW.DATA) == nrow(p)) RAW.DATA <- cbind(RAW.DATA, p)
        else print("ERROR")
      }
    }
    #----------------------------------------- parameter IS NOT a data.frame
    else {
      if (DEBUG) cat("\n[forma.datos] CLASS: ", class(p)," P:",p," LENGTH:", length(p),"\n")
      len.p <- length(p)

      if (len.p == max.length) {
        # must be data
        if (DEBUG) cat("\n[forma.datos] len == max.length\n")
        if(!exists("TEMP.DATA")) TEMP.DATA <- as.data.frame(p)
        else TEMP.DATA <- cbind(TEMP.DATA, p)
        names(TEMP.DATA)[ncol(TEMP.DATA)] <- CALL_DETAILS.UNNAMED[param.number]
      }
      else if (len.p >= 1) {
        # cant be data, must be vars names

        if (DEBUG) cat("\n[forma.datos] len >= 1\n")
        vars <- intersect(p,names(RAW.DATA))

        if(length(vars) != length(p)) cat("\n[forma.datos] WARNING\n")
        if(!exists("TEMP.DATA")) {
          TEMP.DATA <- as.data.frame(RAW.DATA[,p])
          names(TEMP.DATA) <- p
        }
        else {
          names.temp.data <- names(TEMP.DATA)
          TEMP.DATA <- cbind(TEMP.DATA, RAW.DATA[,p])
          names(TEMP.DATA) <- c(names.temp.data, p)
        }
      }
    }
  }

  if(!exists("TEMP.DATA") & exists("RAW.DATA")) TEMP.DATA <- RAW.DATA
  if(!exists("RAW.DATA") & exists("TEMP.DATA")) RAW.DATA <- TEMP.DATA
  #=================================================================== FORMING BY -> DATA.FRAME
  if (!missing("by")) {
    if (DEBUG) cat("\n[forma.datos] BY present\n")
    for(p in by) {
      #----------------------------------------- BY parameter IS a data.frame
      if (is.data.frame(p)) {
        if (DEBUG) cat("\n[forma.datos] by is data.frame","\n")
        if (!exists("BY.DATA")) BY.DATA <- p
        else {
          if (nrow(BY.DATA) == nrow(p)) BY.DATA <- cbind(BY.DATA, p)
          else print("ERROR")
        }
      }
      #----------------------------------------- BY parameter IS NOT a data.frame
      else {
        if (DEBUG) cat("\n[forma.datos] BY -> CLASS: ", class(p)," P:",p," LENGTH:", length(p),"\n")
        len.p <- length(p)

         if (len.p == max.length) {
          # must be data
          if (DEBUG) cat("\n[forma.datos] BY -> len == max.length\n")
          if(!exists("BY.DATA")) BY.DATA <- as.data.frame(p)
          else BY.DATA <- cbind(BY.DATA, p)
        }
        else if (len.p >= 1) {
          # cant be data, must be vars names

          if (DEBUG) cat("\n[forma.datos] BY -> len >= 1\n")
          vars <- intersect(p,names(RAW.DATA))

          if(length(vars) != length(p)) warning(paste("[forma.datos] by variables has/ve been specified but was/were not found in the data:", p))
          if (is.null(vars)) {
            warning(paste("[forma.datos] by variables has/ve been specified but was/were not found:", p))
          }
          else {
            temp.by.data <- as.data.frame(RAW.DATA[,vars])

            if(!exists("BY.DATA")) {
              BY.DATA <- temp.by.data
              names(BY.DATA) <- vars
            }
            else {
              names.temp.data <- names(BY.DATA)
              BY.DATA <- cbind(BY.DATA, temp.by.data)
              names(BY.DATA) <- c(names.temp.data, vars)
            }
          }
        }
      }
    }
  }


  if (DEBUG) {
    if (exists("TEMP.DATA")){
      cat("\n[forma.datos] TEMP_DATA:\n")
      print(TEMP.DATA[1:5,])
      print(names(TEMP.DATA))
    }
    if (exists("BY.DATA")){
      cat("\n[forma.datos] BY_DATA:\n")
      print(BY.DATA[1:5,])
      print(names(BY.DATA))
    }
  }



  if (exists("BY.DATA")) {
    RESULT.DATA <- cbind(TEMP.DATA, BY.DATA)
    class(RESULT.DATA) <- append("udaic.DATA",class(RESULT.DATA))
    attr(RESULT.DATA, "DATA") <- TEMP.DATA
    attr(RESULT.DATA, "HAS.BY") <- TRUE
    attr(RESULT.DATA, "BY.DATA") <- BY.DATA
  }
  else {
    RESULT.DATA <- TEMP.DATA
    class(RESULT.DATA) <- append("udaic.DATA",class(RESULT.DATA))
    attr(RESULT.DATA, "DATA") <- TEMP.DATA
    attr(RESULT.DATA, "HAS.BY") <- FALSE
  }
  attr(RESULT.DATA, "ORIGINAL.CALL") <- CALL_DETAILS

  return(RESULT.DATA)
}


udaic.media <- function(..., by = NULL, decimals = 2, DEBUG = FALSE,
                        show.vars = TRUE, show.by = TRUE, show.groups = TRUE,
                        show.n.valid = TRUE, show.n.missing = TRUE, show.min=TRUE,
                        show.max = TRUE, show.mean = TRUE, show.sd = TRUE,
                        show.median = TRUE, show.IRQ = TRUE,
                        show.p.norm = TRUE, show.p.norm.exact = FALSE, show.nor.test = TRUE,
                        show.is.normal=TRUE, show.interpretation = TRUE, lang = "es", show.global = TRUE){
  library("dplyr")
  library("nortest")

  FORMA.DATOS <- forma.datos(..., by = by, DEBUG = FALSE, DEBUG.CALL = FALSE)
  HAS.BY <- attr(FORMA.DATOS, "HAS.BY")
  DATOS <- attr(FORMA.DATOS, "DATA")
  if (DEBUG) {
    cat("\n[udaic.media] DATOS:\n")
    print(DATOS)
    cat("\n")
  }
  # print(FORMA.DATOS)DATOS
  if (!HAS.BY | show.global == TRUE) {
    for(var in names(DATOS)) {
      var.values <- DATOS %>% pull(var)
      temp.mean <- .udaic.media(var.values, var,decimals = decimals)
      if (is.data.frame(temp.mean)){
        if (!HAS.BY) res <- cbind(var = var, temp.mean)
        else res <- cbind(var = var, by = "-", group = "-", temp.mean)
        if (!exists("result.temp")) result.temp <- res
        else result.temp <- rbind(result.temp, res)
      }
    }
  }


  if (HAS.BY) {
    BY.DATA <- attr(FORMA.DATOS, "BY.DATA")
    if (DEBUG) {
      cat("\n[udaic.media] BY.DATA:\n")
      print(BY.DATA)
      cat("\n")
    }
    for(var in names(DATOS)) {
      if (DEBUG) cat("\n[udaic.media] VAR:",var, "\n")
      for(by.var in names(BY.DATA)) {
        if (DEBUG) cat("\n[udaic.media] BY.VAR:",by.var, "\n")
        by.values <- as.factor(BY.DATA %>% pull(by.var))
        for(by.level in levels(by.values)){
          var.values <- DATOS %>% pull(var)
          var.values <- var.values[by.values == by.level]
          temp.mean <- .udaic.media(var.values, var, decimals = decimals)
          if (is.data.frame(temp.mean)){
            res <- cbind(var = var, by = by.var, group = by.level, temp.mean)
            if (!exists("result.temp")) result.temp <- res
            else result.temp <- rbind(result.temp, res)
          }
        }
      }
    }
  }

  if (!exists("result.temp")) return(NA)
  else {
    if(!show.vars) result.temp$var <- NULL
    if(!show.by) result.temp$by <- NULL
    if(!show.groups) result.temp$groups <- NULL
    if(!show.n.valid) result.temp$n.valid <- NULL
    if(!show.n.missing) result.temp$n.missing <- NULL
    if(!show.min) result.temp$min <- NULL
    if(!show.max) result.temp$max <- NULL
    if(!show.mean) result.temp$mean <- NULL
    if(!show.sd) result.temp$sd <- NULL
    if(!show.median) result.temp$median <- NULL
    if(!show.IRQ) result.temp$IRQ <- NULL
    if(!show.p.norm) result.temp$p.norm <- NULL
    if(!show.p.norm.exact) result.temp$p.norm.exact <- NULL
    if(!show.nor.test) result.temp$nor.test <- NULL
    if(!show.is.normal) result.temp$is.normal <- NULL

    attr(result.temp, "SHOW.INTERPRETATION") <- show.interpretation
    if(show.interpretation) attr(result.temp, "INTERPRETATION") <- .udaic.media.interpretation(result.temp, lang = lang, code = "HELP")
    return(result.temp)
  }
}

.udaic.media.interpretation <- function(x, lang="es", code = ""){
  if (lang == "es") {
    if (code == "HELP") {
      text = "
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
              \n* p.normal: valor p de la prueba de normalidad
              \n* nor.test: prueba usada para la normalidad
              \n  - SW: Shapiro-Wilks
              \n  - Lille (KS): Kolmogorov-Smirnov con la corrección de Lilliefors
              \n* p.normal.exact: valor p de la prueba de normalidad exacto (sin redondeo)
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
    }
  }
  else {
    if (code == "HELP") {
      text ="
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
              \n* p.normal: p value for normality test
              \n* nor.test: test used to check normality
              \n  - SW: Shapiro-Wilks
              \n  - Lille (KS): Lilliefor's correction of Kolmogorov-Smirnov
              \n* p.normal.exact: p value for normality test (without rounding)
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
    }
  }
  if (exists("text")) return(text)
}

.udaic.media <- function(x, x.name, decimals = 2, p.value = 0.05) {
  # require("nortest")

  if (!is.numeric(x)) {
    warning(paste0("[.udaic.media] ERROR - Variable ",x.name," is not numeric"), call. = FALSE)
    return(NA)
  }

  n.valid = length(x) - sum(is.na(x))
  n.missing = sum(is.na(x))
  min = min(x, na.rm = TRUE)
  max = max(x, na.rm = TRUE)
  mean = round(mean(x, na.rm = TRUE), digits = decimals)
  sd = round(sd(x, na.rm = TRUE), digits = decimals)
  median = median(x, na.rm = TRUE)
  IQR = IQR(x, na.rm = TRUE)
  if (n.valid > 3 & n.valid < 5000) {
    p.norm.exact = shapiro.test(x)$p.value
    nor.test = "SW"
  }
  else {
    p.norm.exact = nortest::lillie.test(x)$p.value
    nor.test = "Lillie (KS)"
  }
  is.normal = p.norm.exact > p.value
  small.p <- 10^((decimals+1)*-1)
  if(p.norm.exact <= small.p) p.norm <- paste0(" <",small.p)
  else p.norm <- round(p.norm.exact, digits = decimals+1)
  result <- data.frame("n.valid" = n.valid,
              "n.missing" = n.missing,
              "min" = min,
              "max" = max,
              "mean" = mean,
              "sd" = sd,
              "median" = median,
              "IQR" = IQR,
              "p.norm" = p.norm,
              "p.norm.exact" = p.norm.exact,
              "nor.test" = nor.test,
              "is.normal" =  is.normal)
  class(result) <- append("udaic",class(result))

  return(result)
}



PRUEBAS.UDAIC.MEDIA = FALSE
if (PRUEBAS.UDAIC.MEDIA){
  # udaic.media("AGE", DEBUG=TRUE)
  # udaic.media(data_, "AGE", DEBUG=TRUE)
  # udaic.media(data_, "AGE", "HEIGHT", DEBUG=TRUE)
  udaic.media(data_, by = "SEX", data_$AGE, DEBUG=TRUE)
  udaic.media(data_$AGE)
  c <- udaic.media(data_, by = "SEX", data_$AGE, data_$HEALTH, "BLOND", DEBUG=TRUE)
  udaic.media(data_, by = "SEX", c("AGE", "HEIGHT"), DEBUG=TRUE)
  udaic.media(data_, by = c("SEX","BLOND"), c("AGE", "HEIGHT"), DEBUG=FALSE)
}



#----------------------------------------------------------------------------------- FORMA.DATOS

PRUEBAS.FORMA.DATOS = FALSE
if (PRUEBAS.FORMA.DATOS){
  forma.datos(by = "SEX", "AGE", DEBUG=TRUE)
  forma.datos(data_, by = "SEX", data_$AGE, DEBUG=TRUE)
  c <- forma.datos(data_, by = "SEX", data_$AGE, data_$HEALTH, "BLOND", DEBUG=TRUE)
  forma.datos(data_, by = "SEX", c("AGE", "HEIGHT"), DEBUG=TRUE)
  forma.datos(data_, by = c("SEX","BLOND"), c("AGE", "HEIGHT"), DEBUG=TRUE)
}

#----------------------------------------------------------------------------------- CALLS
prueba.call <- function(..., by=NULL, DEBUG = FALSE, DEBUG.CALL = FALSE) {
  forma.datos(..., by = by, DEBUG = DEBUG, DEBUG.CALL = DEBUG.CALL)
}
PRUEBAS.CALL = FALSE
if (PRUEBAS.CALL){
  prueba.call(by = "SEX", "AGE", DEBUG.CALL=TRUE)
  prueba.call(data_, by = "SEX", data_$AGE, DEBUG.CALL=TRUE, DEBUG=TRUE)
  prueba.call(data_, by = "SEX", data_$AGE, data_$HEALTH, "BLOND", DEBUG.CALL=TRUE, DEBUG = TRUE)
  prueba.call(data_, by = "SEX", c("AGE", "HEIGHT"), DEBUG.CALL=TRUE, DEBUG = TRUE)
  prueba.call(data_, by = c("SEX","BLOND"), c("AGE", "HEIGHT"), DEBUG.CALL=TRUE, DEBUG = TRUE)
  prueba.call(data_, by = c("SEX","BLOND"), "AGE", "HEIGHT", DEBUG.CALL=TRUE, DEBUG = TRUE)
}



