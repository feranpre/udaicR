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

get.data <- function(x, ..., by = NULL) {

  DATA.FRAME = FALSE
  BY = FALSE
  OBS.TOTALES = 0

  parametros <- list(...)
# print(parametros)

    tryCatch({
      ORIGINAL.CALL <- deparse(sys.calls()[[sys.nframe()-1]])
      temp.call <- strsplit(ORIGINAL.CALL,"\\(")[[1]]
      if (length(temp.call) >= 2) {
        ORIGINAL.CALL <- temp.call[2]
        temp.call <- strsplit(ORIGINAL.CALL,"\\)")[[1]]
        if (length(temp.call) >= 1) {
          ORIGINAL.CALL <- temp.call[1]

          temp.call <- strsplit(ORIGINAL.CALL,",")[[1]]
          for(part in temp.call) {
            part <- gsub('\\s+', '',part) #<-- quitamos espacios en blanco
            if (tolower(substr(part,1,3)) == "by=") print(paste("BY ENCONTRADO --> ", part))
          }
          # print("ORIGINAL --->")
          # print(ORIGINAL.CALL)
        }
      }

      }, error = function(err){
        ORIGINAL.CALL <- "NA"
      })
  #   if( ORIGINAL.CALL != "eval(ei, envir)") {
  #      ORIGINAL.CALL <- gsub('\\s+', '',ORIGINAL.CALL) #-- remove all spaces
  #       print(ORIGINAL.CALL)
  #     if (length(grep("(",ORIGINAL.CALL, fixed=TRUE)) == 1) ORIGINAL.FUN <- sub("\\(.*","", ORIGINAL.CALL)
  #     if (length(grep("by=",ORIGINAL.CALL, fixed=TRUE)) == 1) ORIGINAL.BY <- sub("by=*.","", ORIGINAL.CALL)
  #
  #
  #     # print(ORIGINAL.FUN)
  #     # print(ORIGINAL.BY)
  #
  #   }
  #   else {
  #     ORIGINAL.FUN <- "get.data"
  #     ORIGINAL.BY <- deparse(substitute(by))
  #     print(ORIGINAL.BY)
  #   }
  # # print(ORIGINAL.CALL)

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
prueba(data_, by="SEX")
# prueba(data_)
prueba(data_, by=v1)

get.data(data_$AGE, data_$AGE)
get.data(data_, by="SEX")
get.data(data_, by=c("SEX", "HEALTH"))
get.data(data_, by=c("h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h","m","h","h","m","h"))
get.data(data_, by=factor(v1))
get.data(data_, by=v1)


tryCatch({get.data(data_, by=c("SEX","SEX2"))}, error = function(err) {print(err)})
tryCatch({get.data(data_, by=c(1,0,1,0))}, error = function(err) {print(err)})
tryCatch({get.data(data_, by=c("h","h"))}, error = function(err) {print(err)})
