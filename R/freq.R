#' ================
#'
#' Intervalo  de confianza para proporciones
#' Hacerlo para medias tb
#'
#'
#' ==========================================
#'
#' Confidence Interval = p  +/-  z*(√p(1-p) / n)
#'
#'  where:
#'
#'  p: sample proportion
#'  z: the chosen z-value
#'  n: sample size
#'
#'





#' Title freq
#'
#' The freq function creates a dataframe with the frequencies of the independent values in one or more variables passed as arguments.
#' It can also give the information grouping said variable/s by a different one
#' @import dplyr
# @import rlang
#' @importFrom janitor adorn_totals
#' @param df data
#' @param ...                 variables (no quotes needed)
#' @param group_by_col        column used to group the other variables
#' @param col_names           vector with names for the 4 columns
#' @param col_names_groups    vector with the names of the 2 columns regarding grouping
#' @param decimals            number of decimals to show
#' @param show_warnings       if you want to print any warning that may come up
#' @param total               total values of frequencies and percentajes without groups
#' @param total_by_group      total values of frequencies and percentajes within each group
#' @param sort_by_values      sort results by variable categories
#' @param sort_by_freq        sort results by frequency
#' @param sort_by_percent     sort results by perecentage
#' @param sort_decreasing     make the sorting descending
#' @param debug               show debug information
#' @return list of dataframes (1 per variable)
#' @export
#' @examples
#' \dontrun{
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
#'
#'
#' # -- regular freq
#' freq(data_, SEX)
#'
#' # -- freq sorted by Value
#' freq(data_,SEX, sort_by_values = TRUE, sort_decreasing = FALSE)
#'
#' # -- multiple variables
#' freq(data_, SEX, BLOND)
#'
#' # -- one variable grouped by another
#' freq(data_, SEX, group_by_col = BLOND)
#'
#' # -- one variable grouped by another, without groups totals
#' freq(data_, SEX, group_by_col = BLOND, total_by_group = FALSE)
#'
#' }
freq <- function(df,..., group_by_col = NULL, col_names=c("Variable","Values","n","rel.freq","rel.valid.freq"), col_names_groups=c("grouping.by", "groups","rel.group.freq"),
                 decimals=2, show_warnings = TRUE, total=TRUE, total_by_group = TRUE,
                 sort_by_values=FALSE, sort_by_freq=FALSE, sort_by_percent=FALSE, sort_decreasing = TRUE,
                 debug = FALSE) {
 if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
      call. = FALSE)
 }
  if (!requireNamespace("janitor", quietly = TRUE)) {
    stop("Package \"janitor\" needed for this function to work. Please install it.",
      call. = FALSE)
 }
  if (debug) {print(paste("Column names: ",col_names))}

  if (length(col_names) != 5 ) stop("The number of strings in 'col_names' must be 4")
  if (length(col_names_groups) != 3 ) stop("The number of strings in 'col_names_groups' must be 2")

  # library(dplyr)

  vars <- dplyr::enquos(...)
  result_df <- list()


  if(length(vars) == 0) {
    print(length(vars))
    #----- ONLY DATA HAS BEEN PASSED
    print(class(df))
    if(class(df) != "data.frame") df <- as.data.frame(df)
    vars <- colnames(df)
  }

  for (v in vars) {
    # print(paste("VAR ->", v))
    if (missing("group_by_col")) {
      result_temp <- df %>%
                      group_by(!! v) %>%
                        summarise(n = n()) %>%
                          mutate(rel.freq = round((n/sum(n))*100,decimals))
      result_temp <- as.data.frame(result_temp)


      total_n = sum(result_temp$n[!is.na(result_temp[,1])], na.rm=T)
      result_temp$rel.freq.validos[!is.na(result_temp[,1])] <- round((result_temp$n[!is.na(result_temp[,1])]/total_n)*100,decimals)

      if(debug) print(result_temp)

      if (sort_by_values) result_temp <- result_temp[order(result_temp[,ncol(result_temp)-2], decreasing = sort_decreasing),]
      if (sort_by_freq) result_temp <- result_temp[order(result_temp[,ncol(result_temp)-1], decreasing = sort_decreasing),]
      if (sort_by_percent) result_temp <- result_temp[order(result_temp[,ncol(result_temp)], decreasing = sort_decreasing),]
      rownames(result_temp) <- NULL

      if (total) result_temp <- result_temp %>% janitor::adorn_totals("row")

    }
    else {
      agrupa <- enquos(group_by_col)
      result_temp <- df %>%
        group_by(!!! agrupa, !! v) %>%
        summarise(n = n()) %>%
        mutate(rel.freq.group = round((n/sum(n))*100,decimals))
      result_temp <- as.data.frame(result_temp)
      total_n = sum(result_temp$n, na.rm=T)
      result_temp$rel.freq <- round((result_temp$n/total_n)*100,decimals)
      total_n = sum(result_temp$n[!is.na(result_temp[,1])], na.rm=T)
      result_temp$rel.freq.validos[!is.na(result_temp[,1])] <- round((result_temp$n[!is.na(result_temp[,1])]/total_n)*100,decimals)

      if(debug) print(result_temp)


      if (sort_by_values) result_temp <- result_temp[order(result_temp[,1],result_temp[,ncol(result_temp)-2], decreasing = sort_decreasing),]
      if (sort_by_freq) result_temp <- result_temp[order(result_temp[,1],result_temp[,ncol(result_temp)-1], decreasing = sort_decreasing),]
      if (sort_by_percent) result_temp <- result_temp[order(result_temp[,1],result_temp[,ncol(result_temp)], decreasing = sort_decreasing),]
      rownames(result_temp) <- NULL



      result_temp_original <- result_temp
      if (total_by_group) {

        for( l in levels(as.factor(result_temp_original[,1]))) {
          r <- result_temp_original[result_temp_original[,1]==as.character(l) & !is.na(result_temp_original[,1]),]
          r <- rbind(r,c("Total","-",sum(r$n),sum(r$rel.freq.group),sum(r$rel.freq, na.rm = T),sum(r$rel.freq.validos, na.rm = T)))

          if(!exists("result_total_by_group")) result_total_by_group <- r
          else {
            result_total_by_group <- rbind(result_total_by_group,c("","",NULL,NULL,NULL,NULL))
            result_total_by_group <- rbind(result_total_by_group, r)
          }
          rm(list="r")
        }

        if (sum(is.na(result_temp_original[,1]))>0) {
          r <- result_temp_original[is.na(result_temp_original[,1]),]
          r <- rbind(r,c("Total","-",sum(r$n),sum(r$rel.freq.group),sum(r$rel.freq, na.rm = T),sum(r$rel.freq.validos, na.rm = T)))
          if(!exists("result_total_by_group")) result_total_by_group <- r
          else {
            result_total_by_group <- rbind(result_total_by_group,c("","",NULL,NULL,NULL,NULL))
            result_total_by_group <- rbind(result_total_by_group, r)
          }

        }
        result_temp <- result_total_by_group
      }

      if (total) {
        r <- result_temp_original
        r <- rbind(r,c("Total","-",sum(r$n),NA,sum(r$rel.freq, na.rm = T),sum(r$rel.freq.validos, na.rm = T)))
        result_temp <- rbind(result_temp, r[nrow(r),])
      }

      # if(debug) print(result_temp)
    }

    #-- add Variable name Var
    var_column = c(quo_name(v),rep("",nrow(result_temp)-1))
    result_temp = mutate(result_temp,Variable=var_column)
    #-- take Variable_column to the front
    result_temp <- result_temp %>% select(Variable, everything())


    if (!missing("group_by_col")) {
      # -- if there is a grouping var we need to put it on front
      result_temp <- result_temp %>% select(!!! agrupa, everything())
      #-- add Group variable as Group_var
      var_column = c(sub("~","",agrupa),rep("",nrow(result_temp)-1))
      result_temp = mutate(result_temp,Group_var=var_column)
      #-- take Variable_column to the front
      result_temp <- result_temp %>% select(Group_var, everything())
      #-- Change col names
      colnames(result_temp) <- c(col_names_groups[c(1,2)], col_names[1:3], col_names_groups[3], col_names[-c(1:3)])

    } else {
      colnames(result_temp) <- col_names
    }

    result_df[[length(result_df)+1]] <- as.data.frame(result_temp)
  }

  if (length(result_df) == 1) result_df <- result_df[[1]]
  return(result_df)
}
