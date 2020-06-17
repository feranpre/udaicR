#' Title means
#'
#' My own spin on the mean function, working as I like it
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
#' means(df,AGE)
#'
#' means(df,AGE,HEIGHT)
#'
#' mean(df,AGE group_by_col=SEX)
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
  library(dplyr)

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
          range = ifelse(all(is.na(!! v)),NA,round(max(!! v, na.rm = TRUE) - min(!! v, na.rm = TRUE), digits=decimales)),
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
          range = ifelse(all(is.na(!! v)),NA,round(max(!! v, na.rm = TRUE) - min(!! v, na.rm = TRUE), digits=decimales)),
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
                                    range = ifelse(all(is.na(!! v)),NA, round(max(!! v, na.rm = TRUE) - min(!! v, na.rm = TRUE), digits=decimales)),
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
            range = ifelse(all(is.na(!! v)),NA, round(max(!! v, na.rm = TRUE) - min(!! v, na.rm = TRUE), digits=decimales)),
            shapiro = s
          )
      }
    }
    result_temp <- as.data.frame(result_temp)
    if (debug) print(col_names_temp)

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

media <- means
