#=========================
#FORMAS DE LLAMAR
#==========================
# media(DATOS$AGE, DATOS$SEX)
# media(DATOS, "AGE", by = "SEX")
# media(DATOS$AGE, by = DATOS$SEX)
# media(AGE ~ SEX, data = DATOS)
#


# library(udaicR)
data_ <- data.frame(AGE=sample(x = 65:100, size=30, replace = TRUE ),
                    HEIGHT=sample(x = 120:205, size=30, replace = TRUE ),
                    SEX=sample(x = c("Male", "Female"), prob = c(.5,.5), size = 30, replace = TRUE),
                    BLOND=sample(x = c("Yes", "No"), prob = c(.2,.8), size = 30, replace = TRUE),
                    HEALTH=sample(x = c("Bad", "Normal", "Excelent"), prob = c(0.2,.6,.2), size = 30, replace = TRUE)
)
data_ <- rbind(data_, list(34,NA,NA,NA,NA))
data_ <- rbind(data_, list(33,NA,"Male",NA,NA))
data_ <- rbind(data_, list(22,NA,NA,"No",NA))
data_ <- rbind(data_, list(NA,NA,NA,"No","Bad"))
data_$EMPTY <- rep(NA,nrow(data_))
data_$HEALTH <- as.factor(data_$HEALTH)


.log <- function(msg) {

  call.temp <- .parse.call()
  call.fun <- attr(call.temp,"FUN")
  call.fun.text <- paste0("(",call.fun,")")
  if (!is.data.frame(msg) & !is.list(msg) & !is.array(msg) & !is.matrix(msg))  cat("\n",call.fun.text,":",msg)
  else {
    cat("\n",call.fun.text,"\n", sep = "")
    print(msg)
  }

  # cat("\n",mensaje,"\n")
}


# .parse.call <- function(llamada = deparse(sys.calls()[[sys.nframe()-1]])){
.parse.call <- function(f){

  origen = 1
  for(num in 1:sys.nframe()){
    origen = num -1
    if (deparse(sys.call(sys.parent(n=num))) == ".parse.call()" ) break
    # print(deparse(sys.call(sys.parent(n=num))))
  }
  if(missing(f))  ORIGINAL.CALL <- deparse(sys.call(sys.parent(n=origen)))
  else ORIGINAL.CALL <- f
  # print(ORIGINAL.CALL)

  temp.call <- strsplit(ORIGINAL.CALL,"\\(")[[1]]
  FUNC.NAME <- temp.call[1]

  # cat("\n\nFUNCTION -> ", FUNC.NAME,"\n")
  if (length(temp.call) <= 1) return(FUNC.NAME) #---------------------- create function.call object
  temp.args <- stringr::str_extract(ORIGINAL.CALL, '(?<=\\().*(?=\\))')
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
    # print(paste("TEXT: ", arg.text))
    # print(paste("NAME: ", arg.name))
    # print(paste("VALUE: ", arg.value))
    #--- if there is a named argument BOTH must be not NA, else there is no =
    if (is.na(arg.name) | is.na(arg.value)) {
      #-- if any of them are NA we have a not-named argument
      # print("ISNA")
      arg.value <- temp.args[arg.num]
      arg.name = "NA"

      #-- quitamos el simbolo de $ y nos aseguramos de que no se repite el nombre
      arg.no.dollar <- stringr::str_extract(arg.value, '(?<=\\$).*')
      if (arg.no.dollar %in% unnamed.arguments.list) arg.no.dollar <- paste0(arg.no.dollar,".",length(unnamed.arguments.list))
      if (length(unnamed.arguments.list)  == 0) unnamed.arguments.list <- arg.no.dollar
      else unnamed.arguments.list <- c(unnamed.arguments.list, arg.no.dollar)
      arg.no.dollar <- NULL

    }

    arguments.list[[gsub(" ","",arg.name)]] <- c("ARG.NAME" = gsub(" ","",arg.name),
                                   "ARG.VALUE" = gsub(" ","",arg.value))

  }
  # print(arguments.list)
  # attr(ORIGINAL.CALL,"ORIGINAL.CALL") <- FUNC.NAME
  attr(ORIGINAL.CALL,"FUN") <- FUNC.NAME
  attr(ORIGINAL.CALL,"TOTAL.ARGS") <- TOTAL.ARGS
  attr(ORIGINAL.CALL, "LIST.ARGS") <- arguments.list
  attr(ORIGINAL.CALL, "LIST.UNNAMED.ARGS") <- unnamed.arguments.list
  class(ORIGINAL.CALL) <- "udaicR.function.parse"
  return(ORIGINAL.CALL)
}

print.udaicR.function.parse <- function(obj,...) {
  cat("\n Original call:", as.character(obj))
  cat("\n Fun name:", attr(obj,"FUN"))
  cat("\n Total args passed:", attr(obj,"TOTAL.ARGS"))
  cat("\n Argument list:\n")
  print(attr(obj,"LIST.ARGS"))
  cat("\n Unnamed argument list:\n")
  print(attr(obj,"LIST.UNNAMED.ARGS"))
}


get.data <- function(x, ..., by = NULL, data = NULL) {

  DATA.FRAME = FALSE
  BY = FALSE
  OBS.TOTALES = 0

  parametros <- list(...)
  LLAMADA <- .parse.call()
  nombres.parametros <- attr(LLAMADA, "LIST.UNNAMED.ARGS")

  if(is.data.frame(x)) {
    DATA.FRAME = TRUE
    RAW.DATA <- x
  }
  else {
    DATA.FRAME = FALSE
    RAW.DATA <- as.data.frame(x)
    #-- there's a difference between using x as the first parameter
    #   and casting it
    #   get.data(data$variable)   <-- here "x" is NOT in the parameters list
    #   get.data(x=data$variable) <-- here "x" IS in the list
    #
    #   If x was specified we DO NOT need to take any names from the parameter list
    #   because x will NOT be in that list.
    if ("x" %in% names(attr(LLAMADA, "LIST.ARGS")))  names(RAW.DATA) <- attr(LLAMADA, "LIST.ARGS")[["x"]]["ARG.NAME"]
    else {
      names(RAW.DATA) <- nombres.parametros[1]
      nombres.parametros <- nombres.parametros[-1]
    }
  }


  if (!missing(data)) {
    # --- there is a data parameter, this MUST be a data.frame
    if (!is.data.frame(data)) stop(.error.udaicR("get_data", "DATA_NOT_DF"))
    if (exists("RAW.DATA")) {
      if (nrow(RAW.DATA) != nrow(data)) stop(.error.udaicR("get_data","DATA_AND_X_LENGTHS"))
      RAW.DATA <- tryCatch({
          cbind(data, RAW.DATA)
        }, error = function(err){
          print(paste("[ERROR](get_data):",err))
        }
        )
    } else RAW.DATA <- data
  }


  OBS.TOTALES <- nrow(RAW.DATA)

  #---------- QUITAMOS PARAMETROS QUE NO SON DATOS HASTA QUEDARNOS SOLO CON LOS DATOS

  # parametros.data <- parametros[!("by" %in% names(parametros))]
  parametros.data <- parametros

  print(parametros.data)
  # cat("\nPARAM NUM: ", length(parametros.data))
  # ------------- hay parámetros con datos
  if (length(parametros.data) > 0 ) {
    # print(parametros.data[[1]])
    #-- estos se supone que son varaibles numéricas
    temp.error = TRUE
    #--- RAW.DATA always exists?
    x.temp <- data.frame(EMPTY.UDAICR = 1:nrow(RAW.DATA))
    x.temp <- cbind(x.temp,lapply(parametros.data,as.data.frame))
    x.temp$EMPTY.UDAICR <- NULL

    # print(attr(LLAMADA,"LIST.UNNAMED.ARGS"))
    if (ncol(x.temp) == length(attr(LLAMADA,"LIST.UNNAMED.ARGS")[[1]])) names(x.temp) <- attr(LLAMADA,"LIST.UNNAMED.ARGS")[[1]]
    # print(x.temp)

    tryCatch({
      x.temp <- cbind(RAW.DATA, x.temp)
      temp.error = FALSE
    }, warning = function(warn){
      cat("\n[WARNING](get_data): While creating data.frame with ... ",paste(warn))
    }, error = function(err){
      cat("\n[ERROR](get_data): While creating data.frame with ... :",paste(err))
    })
    if (!temp.error) MERGED.DATA <- x.temp
  }

  if (!exists("MERGED.DATA")) MERGED.DATA <- RAW.DATA

  # print(MERGED.DATA)
  # return()

  # --- RAW.DATA is 'data' + x
  # --- MERGED.DATA is RAW.DATA + ...
  # --- FINAL.DATA is MERGED.DATA + by
  #
  #
  #
  #
  #
  #  --- DECISIONES QUE TOMAR ---
  #  Hace falta decidir cómo lo quiero hacer
  #  Si la estructura es DATA.FRAME y luego tenemos:
  #      - Lista de nombres de VARIABLES
  #      - Lista de nombres BY
  #
  #      * Entonces tenemos que METER en el DATA.FRAME las BY y quedarnos los nombres
  #
  #  Si lo queremos separar tendremos un DATA.FRAME de DATOS y OTRO de BY
  if(!missing(by)) {
    # BY.ORIGINAL.CALL <- deparse(substitute(by))
    # print(BY.ORIGINAL.CALL)
    by.value <- by
    # print(RAW.DATA)
    if(!is.numeric(by.value) & DATA.FRAME) {

      # -- if values are names of columns take them
        if (all(by.value %in% names(MERGED.DATA))) {
          by.name <- by.value
          by.value <- as.data.frame(MERGED.DATA[,by.value])
          names(by.value) <- by.name
          MERGED.DATA <- MERGED.DATA[, !(names(MERGED.DATA) %in% by.name)]
        }
      # -- if values are NOT in data.frame and therefore are not variables names
      #    then we try to make a variable out of it
        else if (length(by.value) == OBS.TOTALES) {
          by.value <- as.data.frame(by.value)
          by.name = "NA"
        } else stop(.error.udaicR("get_data", "BY_VAR_NOT_IN_DF"))
    }
    #-- if not name of columns but same lenght as X make it an independent var
    else if (length(by.value) == OBS.TOTALES) {
      by.value <- as.data.frame(by.value)
      by.name = "NA"
    } else stop(.error.udaicR("get_data", "BY_VAR_NOT_IN_DF"))

    class(x) <- append("udaicR_DATA", class(x))
    attr(x,"by.name") <- by.name
    attr(x,"by") <- by.value
  }
  else {
    #-- NO BY
    class(x) <- append("udaicR_DATA", class(x))
    attr(x,"by.name") <- "NA"
    attr(x,"by") <- "NA"
  }

  return(x)
}




prueba <- function(x, ...) {
  get.data(x,...)
}

.error.udaicR <- function(fun, code, lang="es") {
  error.text <- list()
  error.text[["en"]] <- c(
    GENERIC = "generic error",
    DATA_NOT_DF = "'data' parameter was specified but is not a data.frame",
    DATA_AND_X_LENGTHS  = "data sources differ in lengths",
    BY_VAR_NOT_IN_DF = "By variable not in the data.frame",
    BY_LENGTH = "BY variable length is not the same as the data.frame",
    BY_LENGTH_NO_DF = "BY variable length is not the same as the rest"
  )
  error.text[["es"]] <- c(
    GENERIC = "error generico",
    DATA_NOT_DF = "el argumento 'data' ha sido especificado pero no es un data.frame",
    DATA_AND_X_LENGTHS  = "las fuentes de datos tienen longitudes distintas",
    BY_VAR_NOT_IN_DF = "La variable BY no está en el data.frame",
    BY_LENGTH = "La longitud de la variable BY no es la misma que la del data.frame",
    BY_LENGTH_NO_DF = "La longitud de la variable BY no es la misma que la del resto de variables"
  )

  if(!is.na(error.text[[lang]][code])) return(paste0("[ERROR](",fun,"): ",error.text[[lang]][code]))
  else return(paste0("[ERROR](",fun,"): ",error.text[[lang]]["GENERIC"]))
}

v1 <- c("h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h")


print.udaicR_DATA <- function(obj,..) {
  # print(as.data.frame(obj))
  print(attr(obj,"by.name"))
  # print(attr(obj, "by"))

}








prueba(data_, by="SEX")
prueba(data_, by=c("SEX", "HEALTH"))
c <- prueba(data_, variables = c("AGE", "HEIGHT"), by=c("SEX", "HEALTH"))

# # prueba(data_)
# prueba(data_, by=v1)
#
# get.data(x=data_$AGE, data_$AGE)
# get.data(data_$AGE, data_$AGE)
# get.data(data_, by="SEX")
get.data(data_, by=c("SEX", "HEALTH"))
# get.data(data_, by=c("h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h"))
# get.data(data_, by=factor(v1))
# get.data(data_, by=v1)
#
#
# tryCatch({get.data(data_, by=c("SEX","SEX2"))}, error = function(err) {print(err)})
# tryCatch({get.data(data_, by=c(1,0,1,0))}, error = function(err) {print(err)})
# tryCatch({get.data(data_, by=c("h","m","m","h","m","h","h","m"))}, error = function(err) {print(err)})
# get.data(data_, by=c("h","m","m","h","m","h","h","m"))
