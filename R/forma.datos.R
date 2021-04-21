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

          if(length(vars) != length(p)) cat("\n[forma.datos] WARNING\n")
          if(!exists("BY.DATA")) {
            BY.DATA <- as.data.frame(RAW.DATA[,p])
            names(BY.DATA) <- p
          }
          else {
            names.temp.data <- names(BY.DATA)
            BY.DATA <- cbind(BY.DATA, RAW.DATA[,p])
            names(BY.DATA) <- c(names.temp.data, p)
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
    }
  }

  RAW.DATA <- TEMP.DATA
  attr(RAW.DATA, "DATA") <- TEMP.DATA
  class(RAW.DATA) <- append("udaic.DATA",class(RAW.DATA))
  if (exists("BY.DATA")) {
    RAW.DATA <- cbind(RAW.DATA, BY.DATA)
    attr(RAW.DATA, "HAS.BY") <- TRUE
    attr(RAW.DATA, "BY.DATA") <- BY.DATA
  }
  else attr(RAW.DATA, "HAS.BY") <- FALSE
  attr(RAW.DATA, "ORIGINAL.CALL") <- CALL_DETAILS

  return(RAW.DATA)
}


udaic.media <- function(..., by = NULL, decimals = 2, DEBUG = TRUE) {
  require("dplyr")

  FORMA.DATOS <- forma.datos(..., by = by, DEBUG = TRUE, DEBUG.CALL = FALSE)
  HAS.BY <- attr(FORMA.DATOS, "HAS.BY")
  DATOS <- attr(FORMA.DATOS, "DATA")
  print(FORMA.DATOS)
  if (HAS.BY) {
    BY.DATA <- attr(FORMA.DATOS, "BY.DATA")
    print(BY.DATA)
    for(var in names(DATOS)) {
      cat("\nVAR:",var, "\n")
      for(by.var in names(BY.DATOS)) {
        cat("\nBY.VAR:",by.var, "\n")
        by.values <- as.factor(BY.DATA %>% pull(by.var))
        for(by.level in levels(by.values)){
          var.values <- DATOS %>% pull(var)
          var.values <- var.values[by.values == by.level]
          res <- cbind(var = var, by = by.var, group = by.level, .udaic.media(var.values, decimals = decimals))
          if (!exists("result.temp")) result.temp <- res
          else result.temp <- rbind(result.temp, res)
        }
      }
    }
  }
  else {
    for(var in names(DATOS)) {
      var.values <- DATOS %>% pull(var)
      res <- cbind(var = var,.udaic.media(var.values, decimals = decimals))
      if (!exists("result.temp")) result.temp <- res
      else result.temp <- rbind(result.temp, res)
    }
  }

  if (!exists("result.temp")) return()
  else return(result.temp)
}

.udaic.media <- function(x, decimals = 2, p.value = 0.05) {
  if (!is.numeric(x)) {
    print("[.udaic.media] ERROR - Not numeric")
    return()
  }
  # df <- as.data.frame(x)
  # result <- df %>% summarize(
  #   n.valid = n()-sum(is.na(df[,1])),
  #   n.missing = sum(is.na(df[,1])),
  #   min=round(min(df[,1], na.rm = TRUE),digits = decimals),
  #   max=round(max(df[,1], na.rm = TRUE),digits = decimals),
  #   mean=round(mean(df[,1], na.rm = TRUE), digits = decimals),
  #   sd=round(sd(df[,1], na.rm = TRUE),digits = decimals),
  #   median=round(median(df[,1], na.rm = TRUE),digits = decimals),
  #   IQR=round(IQR(df[,1], na.rm = TRUE),digits = decimals),
  #   norm.test = udaicR::is_normal(df[,1])
  # )
  n.valid = length(x) - sum(is.na(x))
  n.missing = sum(is.na(x))
  min = min(x, na.rm = TRUE)
  max = max(x, na.rm = TRUE)
  mean = round(mean(x, na.rm = TRUE), digits = decimals)
  sd = round(sd(x, na.rm = TRUE), digits = decimals)
  median = median(x, na.rm = TRUE)
  IQR = IQR(x, na.rm = TRUE)
  if (n.valid > 3 & n.valid < 5000) {
    p.norm = round(shapiro.test(x)$p.value, digits = decimals+1)
    nor.test = "SW"
  }
  else {
    p.norm = round(nortest::lillie.test(x)$p.value, digits = decimals+1)
    nor.test = "Lillie (KS)"
  }
  is.normal = p.norm > p.value
  small.p <- 10^((decimals+1)*-1)
  if(p.norm < small.p) p.value <- paste0("<",small.p)

  result <- data.frame("n.valid" = n.valid,
              "n.missing" = n.missing,
              "min" = min,
              "max" = max,
              "mean" = mean,
              "sd" = sd,
              "median" = median,
              "IQR" = IQR,
              "p.norm" = p.norm,
              "nor.test" = nor.test,
              "is.normal" =  is.normal)

  return(result)
}



PRUEBAS.UDAIC.MEDIA = FALSE
if (PRUEBAS.UDAIC.MEDIA){
  # udaic.media("AGE", DEBUG=TRUE)
  # udaic.media(data_, "AGE", DEBUG=TRUE)
  # udaic.media(data_, "AGE", "HEIGHT", DEBUG=TRUE)
  udaic.media(data_, by = "SEX", data_$AGE, DEBUG=TRUE)
  c <- udaic.media(data_, by = "SEX", data_$AGE, data_$HEALTH, "BLOND", DEBUG=TRUE)
  udaic.media(data_, by = "SEX", c("AGE", "HEIGHT"), DEBUG=TRUE)
  udaic.media(data_, by = c("SEX","BLOND"), c("AGE", "HEIGHT"), DEBUG=TRUE)
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



