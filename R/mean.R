



#' =============================================================
#'  TO BE REMOVED: this is here just for compatibility
#' =============================================================
#'
#' Title means
#'
#' My own spin on the mean function, working as I like it
#'
#' @import dplyr
#'
#' @param df              data
#' @param ...             any number of numerical variables
#' @param group_by_col    variable to group the others by
#' @param decimales       number of decimal to show
#' @param show_warnings   show warnings
#' @param show_errors     show errors
#' @param n               show n (without missing values)
#' @param missing         show number of missing values
#' @param min             show min value (or NA)
#' @param max             show max value (or NA)
#' @param mean            show mean value
#' @param sd              show standard deviation
#' @param median          show median
#' @param range           show range
#' @param norm.test       perform Shapiro-Wilks test and show p.value
#' @param col_names       column names for all columns (must set all or none)
#'
#' @return                data.frame with the corresponding stats
#' @export
#'
#' @examples
#'
#' data_ <- data.frame(AGE=sample(x = 65:100, size=30, replace = TRUE ),
#'                     HEIGHT=sample(x = 120:205, size=30, replace = TRUE ),
#'                     SEX=sample(x = c("Male", "Female"), prob = c(.5,.5), size = 30, replace = TRUE),
#'                     BLOND=sample(x = c("Yes", "No"), prob = c(.2,.8), size = 30, replace = TRUE)
#' )
#' data_ <- rbind(data_, list(NA,NA,NA,NA))
#' data_ <- rbind(data_, list(NA,NA,"Male",NA))
#' data_ <- rbind(data_, list(NA,NA,NA,"No"))
#'
#' means(data_, AGE)
#'
#' means(data_, AGE,HEIGHT)
#'
#' mean(data_, AGE, group_by_col=SEX)
#'
means <- function(df, ... , group_by_col = NULL, decimales=2, show_warnings = TRUE, show_errors = FALSE,
                  n=TRUE, missing=TRUE,
                  min=TRUE, max= TRUE, mean=TRUE, sd=TRUE,
                  median=TRUE, range=TRUE, norm.test = TRUE,
                  col_names = c("var","groups","n.valid","n.missing","min","max","mean","sd","median","range","shapiro-wilk")) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  library(dplyr, quietly = TRUE)

  if(show_warnings) .Deprecated("medias")

  vars <- enquos(...)
  result_df <- list()

  for (v in vars) {
    vs <- quo_name(v)
    col_names_temp <- col_names
    v.data <- df %>% select(!! v)
    v.data <- as.data.frame(v.data)[,names(v.data)]


    # --- NO GROUPING
    if (missing("group_by_col")) {
      if ((nrow(df) < 5000) & ((nrow(df) - sum(is.na(v.data))) > 3)) {
        s = tryCatch({
          shapiro.test(v.data)$p.value
        }, error = function(err) {
          if (show_errors) print(paste("\n ERROR udaicR::means ->",err))
          return(NA)
        }
        )

        result_temp <- df %>% summarise(
          n = n()-sum(is.na(.data[[quo_name(v)]])),
          missing = ifelse(all(is.na(!! v)),0,sum(is.na(.data[[quo_name(v)]]))),
          min = ifelse(all(is.na(!! v)),NA,round(min(!! v, na.rm=TRUE), digits=decimales)),
          max = ifelse(all(is.na(!! v)),NA,round(max(!! v, na.rm=TRUE), digits=decimales)),
          mean = ifelse(all(is.na(!! v)),NA,round(mean(!! v, na.rm = TRUE), digits=decimales)),
          sd = ifelse(all(is.na(!! v)),NA,round(sd(!! v, na.rm = TRUE), digits=decimales)),
          median = ifelse(all(is.na(!! v)),NA,round(median(!! v, na.rm = TRUE), digits=decimales)),
          range = ifelse(all(is.na(!! v)),NA,round(quantile(!! v, na.rm = TRUE)[4] - quantile(!! v, na.rm = TRUE)[2], digits=decimales)),
          shapiro = s
        )
      } else {
        s = tryCatch({
          ks.test(v.data, "pnorm", mean=mean(v.data, na.rm=TRUE), sd=sd(v.data, na.rm=TRUE))$p.value
        }, error = function(err) {
          if (show_errors) print(paste("\n ERROR udaicR::means ->",err))
          return(NA)
        }
        )
        result_temp <- df %>% summarise(
          n = n()-sum(is.na(.data[[quo_name(v)]])),
          missing = ifelse(all(!is.na(!! v)),0,sum(is.na(.data[[quo_name(v)]]))),
          min = ifelse(all(is.na(!! v)),NA,round(min(!! v, na.rm=TRUE), digits=decimales)),
          max = ifelse(all(is.na(!! v)),NA,round(max(!! v, na.rm=TRUE), digits=decimales)),
          mean = ifelse(all(is.na(!! v)),NA,round(mean(!! v, na.rm = TRUE), digits=decimales)),
          sd = ifelse(all(is.na(!! v)),NA,round(sd(!! v, na.rm = TRUE), digits=decimales)),
          median = ifelse(all(is.na(!! v)),NA,round(median(!! v, na.rm = TRUE), digits=decimales)),
          range = ifelse(all(is.na(!! v)),NA,round(quantile(!! v, na.rm = TRUE) - quantile(!! v, na.rm = TRUE)[2], digits=decimales)),
          shapiro = s
        )
      }


    }
    else {
      agrupa <- enquos(group_by_col)
      if ((nrow(df) < 5000) & ((nrow(df) - sum(is.na(v.data))) > 3)) {
        s = tryCatch({
          shapiro.test(df[,quo_name(v)])$p.value
        }, error = function(err) {
          if (show_errors) print(paste("\n ERROR udaicR::means ->",err))
          return(NA)
        }
        )
        result_temp <- df %>%
          group_by(!!! agrupa) %>%
          summarise(
            n = n()-sum(is.na(!! v)),
            missing = ifelse(all(!is.na(!! v)),0,sum(is.na(!! v))),
            min = ifelse(all(is.na(!! v)),NA,round(min(!! v, na.rm=TRUE), digits=decimales)),
            max = ifelse(all(is.na(!! v)),NA,round(max(!! v, na.rm=TRUE), digits=decimales)),
            mean = round(mean(!! v, na.rm = TRUE), digits=decimales),
            sd = round(sd(!! v, na.rm = TRUE), digits=decimales),
            median = round(median(!! v, na.rm = TRUE), digits=decimales),
            range = ifelse(all(is.na(!! v)),NA, round(quantile(!! v, na.rm = TRUE)[4] - quantile(!! v, na.rm = TRUE)[2], digits=decimales)),
            shapiro = s
          )
      } else {
        s = tryCatch({
          ks.test(df[,quo_name(v)], "pnorm", mean=mean(df[,quo_name(v)], na.rm=TRUE), sd=sd(df[,quo_name(v)], na.rm=TRUE))$p.value
        }, error = function(err) {
          if (show_errors) print(paste("\n ERROR udaicR::means ->",err))
          return(NA)
        }
        )

        result_temp <- df %>%
          group_by(!!! agrupa) %>%
          summarise(
            n = n()-sum(is.na(!! v)),
            missing = ifelse(all(!is.na(!! v)),0,sum(is.na(!! v))),
            min = ifelse(all(is.na(!! v)),NA,round(min(!! v, na.rm=TRUE), digits=decimales)),
            max = ifelse(all(is.na(!! v)),NA,round(max(!! v, na.rm=TRUE), digits=decimales)),
            mean = ifelse(all(is.na(!! v)),NA,round(mean(!! v, na.rm = TRUE), digits=decimales)),
            sd = ifelse(all(is.na(!! v)),NA,round(sd(!! v, na.rm = TRUE), digits=decimales)),
            median = ifelse(all(is.na(!! v)),NA,round(median(!! v, na.rm = TRUE), digits=decimales)),
            range = ifelse(all(is.na(!! v)),NA, round(quantile(!! v, na.rm = TRUE)[4] - quantile(!! v, na.rm = TRUE)[2], digits=decimales)),
            shapiro = s
          )
      }
    }
    result_temp <- as.data.frame(result_temp)
    # if (debug) print(col_names_temp)

    if (n == FALSE) {
      col_names_temp <- col_names_temp[-3]
      result_temp$n <- NULL
    }
    if (missing == FALSE) {
      col_names_temp <- col_names_temp[-4]
      result_temp$missing <- NULL
    }
    if (min == FALSE) {
      col_names_temp <- col_names_temp[-5]
      result_temp$min <- NULL
    }
    if (max == FALSE) {
      col_names_temp <- col_names_temp[-6]
      result_temp$max <- NULL
    }
    if (mean == FALSE) {
      col_names_temp <- col_names_temp[-7]
      result_temp$mean <- NULL
    }
    if (sd == FALSE) {
      col_names_temp <- col_names_temp[-8]
      result_temp$sd <- NULL
    }
    if (median == FALSE) {
      col_names_temp <- col_names_temp[-9]
      result_temp$median <- NULL
    }
    if (range == FALSE) {
      col_names_temp <- col_names_temp[-10]
      result_temp$range <- NULL
    }
    if (norm.test == FALSE) {
      col_names_temp <- col_names_temp[-11]
      result_temp$shapiro <- NULL
    } else {
      result_temp$shapiro <- c(rep(NA,nrow(result_temp)-1),round(result_temp$shapiro[1],digits = decimales))
      if(!is.na(result_temp$shapiro[1])){
        if (result_temp$shapiro < 10^((decimales+1)*-1)) result_temp$shapiro <- paste("<",as.character(10^((decimales+1)*-1)),"")
        if(nrow(df) > 5000) col_names_temp[11] <- "kolmogorov-smirnov"
      }
    }

    # -- add a column with the name of the var being studied
    result_temp <- cbind(c(quo_name(v),rep("",nrow(result_temp)-1)),result_temp)
    # colnames(result_temp)[1]<- "Var"

    # we take the groups col name the last to make all the rest of the code usable,
    # if we took it before this point all the
    if (missing("group_by_col")) col_names_temp <- col_names_temp[-2]

    colnames(result_temp) <- col_names_temp


    # result_df[[length(result_df)+1]] <- as.data.frame(result_temp)
    if (!exists("result_df")) result_df <- as.data.frame(result_temp)
    else result_df <- rbind(result_df,result_temp)
  }


  if (length(result_df) == 1) result_df <- result_df[[1]]
  return(result_df)
}





#' Title do.means
#'
#' This is the function that does the calculation per se
#'
#' @param variable
#' @param show.errors
#' @param DEBUG
#' @param decimales
#' @param n
#' @param missing
#' @param min
#' @param max
#' @param mean
#' @param sd
#' @param median
#' @param IQR
#' @param norm.test
#'
#' @return
#'
#' @examples
.do.means <- function(variable, show.errors = FALSE, DEBUG = TRUE, decimales=2,
                      n=TRUE, missing=TRUE,
                      min=TRUE, max= TRUE, mean=TRUE, sd=TRUE,
                      median=TRUE, IQR=TRUE, norm.test = TRUE) {

  var.name <- quo_name(enquo(variable))

  # ----- try variable class
  var.class <- tryCatch({
    class(variable)
  }, warning = function(war) {
    return("NA")
  }, error = function(err) {
    if (show.errors) print(paste("ERROR udaicR::means ->",err))
    return("NA")
  }
  )


  # ----- try variable length
  var.length <- tryCatch({
    length(variable)
  }, warning = function(war) {
    return(NA)
  }, error = function(err) {
    if (show.errors) print(paste("ERROR udaicR::means ->",err))
    return(0)
  }
  )


  if (is.data.frame(variable)){
    variable <- variable[,1]
  }
  if(!is.numeric(variable)) {
    return(as.data.frame(VAR="Error, not numeric"))
  }

  var.data <- variable

  total <- data.frame(VAR = var.data) %>% summarise(n = n()-sum(is.na(VAR)))

  result_temp <- as.data.frame(data.frame(VAR = var.data) %>% summarise(var = var.name))
  if(n) result_temp <- cbind(result_temp, data.frame(VAR = var.data) %>% summarise(n = n()-sum(is.na(VAR))))
  if(missing) result_temp <- cbind(result_temp, data.frame(VAR = var.data) %>% summarise(missing = ifelse(all(!is.na(VAR)),0,sum(is.na(VAR)))))
  if(min) result_temp <- cbind(result_temp, data.frame(VAR = var.data) %>% summarise(min = ifelse(all(is.na(VAR)),NA,round(min(VAR, na.rm=TRUE), digits=decimales))))
  if(max) result_temp <- cbind(result_temp, data.frame(VAR = var.data) %>% summarise(max = ifelse(all(is.na(VAR)),NA,round(max(VAR, na.rm=TRUE), digits=decimales))))
  if(mean) result_temp <- cbind(result_temp, data.frame(VAR = var.data) %>% summarise(mean = ifelse(all(is.na(VAR)),NA,round(mean(VAR, na.rm = TRUE), digits=decimales))))
  if(sd) result_temp <- cbind(result_temp, data.frame(VAR = var.data) %>% summarise(sd = ifelse(all(is.na(VAR)),NA,round(sd(VAR, na.rm = TRUE), digits=decimales))))
  if(median) result_temp <- cbind(result_temp, data.frame(VAR = var.data) %>% summarise(median = ifelse(all(is.na(VAR)),NA,round(median(VAR, na.rm = TRUE), digits=decimales))))
  if(IQR) result_temp <- cbind(result_temp, data.frame(VAR = var.data) %>% summarise(IQR = ifelse(all(is.na(VAR)),NA, round(quantile(VAR, na.rm = TRUE)[4] - quantile(VAR, na.rm = TRUE)[2], digits=decimales))))

  if(norm.test) {
    if ((total < 5000) & (total >= 3)) {
      s <- shapiro.test(var.data)$p.value
      if (s < 0.001) s = "<0.001"
      else s = round(s,digits = decimales)
      norm.test.name <- "Shapiro-Wilks"
      s <- paste(s," S-W")
      result_temp <- cbind(result_temp, data.frame(VAR = var.data) %>% summarise(norm.test = s))
    } else if (total >= 4) {
      s <- nortest::lillie.test(var.data)$p.value
      if (s < 0.001) s = "<0.001"
      else s = round(s,digits = decimales)
      norm.test.name <- "K-S (Lilliefors)"
      s <- paste(s," K-S (Lillie)")
      result_temp <- cbind(result_temp, data.frame(VAR = var.data) %>% summarise(norm.test = s))
    } else {
      s <- "Error"
      result_temp <- cbind(result_temp, data.frame(VAR = var.data) %>% summarise(norm.test = s))

    }
  }

  print_ <- function(){
    print("SADFDSAFDSAFDSA")
  }
  return(result_temp)

}
