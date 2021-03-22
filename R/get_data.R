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


  if(missing(f))  ORIGINAL.CALL <- deparse(sys.call(sys.parent()))
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
    }

    arguments.list[[arg.num]] <- c("ARG.NAME" = gsub(" ","",arg.name),
                                   "ARG.VALUE" = gsub(" ","",arg.value))

  }
  # print(arguments.list)
  # attr(ORIGINAL.CALL,"ORIGINAL.CALL") <- FUNC.NAME
  attr(ORIGINAL.CALL,"FUN") <- FUNC.NAME
  attr(ORIGINAL.CALL,"TOTAL.ARGS") <- TOTAL.ARGS
  attr(ORIGINAL.CALL, "LIST.ARGS") <- arguments.list
  class(ORIGINAL.CALL) <- "udaicR.function.parse"
  return(ORIGINAL.CALL)
}

print.udaicR.function.parse <- function(obj,...) {
  cat("\n Original call:", as.character(obj))
  cat("\n Fun name:", attr(obj,"FUN"))
  cat("\n Total args passed:", attr(obj,"TOTAL.ARGS"))
  cat("\n Argument list:\n")
  print(attr(obj,"LIST.ARGS"))
}


get.data <- function(x, ..., by = NULL) {

  DATA.FRAME = FALSE
  BY = FALSE
  OBS.TOTALES = 0

  parametros <- list(...)
  print(.parse.call())
  return()
# print(parametros)
  LLAMADA <- tryCatch({
                deparse(sys.calls()[[sys.nframe()-1]])
              }, error = function(err){
                return("NA")
              })
  llamada <- .parse.call(LLAMADA)


  return()
  if(is.data.frame(x)) {
    DATA.FRAME = TRUE
    OBS.TOTALES = nrow(x)
  }
  else {
    DATA.FRAME = FALSE
    OBS.TOTALES = length(x)
    x <- as.data.frame(x)
  }

  #---------- QUITAMOS PARAMETROS QUE NO SON DATOS HASTA QUEDARNOS SOLO CON LOS DATOS
  parametros.data <- parametros[!("by" %in% names(parametros))]

  if (length(parametros.data) > 0 ) {
        print(parametros.data[[1]])
      #-- estos se supone que son varaibles numéricas
      temp.error = TRUE
      tryCatch({
        x.temp <- cbind(x, lapply(parametros.data,as.data.frame))
        temp.error = FALSE
      }, warning = function(warn){
        print(warn)
      }, error = function(err){
        print(err)
      })
      if (!temp.error) x <- x.temp
  }

  if(!missing(by)) {
    # BY.ORIGINAL.CALL <- deparse(substitute(by))
    # print(BY.ORIGINAL.CALL)
    by.value <- by
    print(x)
    if(!is.numeric(by.value) & DATA.FRAME) {

      # -- if values are names of columns take them
        if (all(by.value %in% names(x))) {
          by.name <- by.value
          by.value <- as.data.frame(x[,by.value])
          names(by.value) <- by.name
          x <- x[, !(names(x) %in% by.name)]
        }
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
    BY_VAR_NOT_IN_DF = "By variable not in the data.frame",
    BY_LENGTH = "BY variable length is not the same as the data.frame",
    BY_LENGTH_NO_DF = "BY variable length is not the same as the rest"
  )
  error.text[["es"]] <- c(
    GENERIC = "error generico",
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








# prueba(data_, by="SEX")
# # prueba(data_)
# prueba(data_, by=v1)
#
# get.data(data_$AGE, data_$AGE)
# get.data(data_, by="SEX")
# get.data(data_, by=c("SEX", "HEALTH"))
# get.data(data_, by=c("h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h"))
# get.data(data_, by=factor(v1))
# get.data(data_, by=v1)
#
#
# tryCatch({get.data(data_, by=c("SEX","SEX2"))}, error = function(err) {print(err)})
# tryCatch({get.data(data_, by=c(1,0,1,0))}, error = function(err) {print(err)})
tryCatch({get.data(data_, by=c("h","h"))}, error = function(err) {print(err)})
