require("dplyr")

tab <- function(df,x,y, col_percent = TRUE, row_percent = FALSE, show_totals = TRUE,
                chi = TRUE, decimals = 2){
 if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
      call. = FALSE)
 }
 x <- enquo(x)
 y <- enquo(y)

 d <- df %>% select(!! x,!! y)
 # print(d)
 t1 <- table(d[,1], d[,2])
 t_row_prop <- round(prop.table(t1, margin = 1)*100, digits = decimals) #.. row
 t_row_prop <- as.data.frame.matrix(t_row_prop) %>% mutate(row_totals = rowSums(.))
 t_col_prop <- round(prop.table(t1, margin = 2)*100, digits = decimals) #.. col
 # t_col_prop <- as.data.frame.matrix(t_col_prop) %>% mutate(col_totals = colSums(.))

 col_per_level = 0
 if (col_percent) col_per_level = col_per_level +1
 if (row_percent) col_per_level = col_per_level +1

 for (cn in 1:ncol(t1)) {
   if (!exists("result_t")) {
     result_t <- t1[,cn]
     result_t <- as.data.frame(result_t)
   }
   else result_t <- cbind(result_t,t1[,cn])

   col_name_num = cn+(col_per_level*(cn-1))
   # print(paste(levels(as.factor(d[,2]))[cn],"n",sep = "_"))
   colnames(result_t)[col_name_num] <- paste(colnames(t1)[cn],"n",sep = "_")
   if (col_percent) {
     result_t <- cbind(result_t,t_col_prop[,cn])
     col_name_num = col_name_num + 1
     colnames(result_t)[col_name_num] <- paste(colnames(t1)[cn],"col%",sep = "_")
   }
   if (row_percent) {
     result_t <- cbind(result_t,t_row_prop[,cn])
     col_name_num = col_name_num + 1
     colnames(result_t)[col_name_num] <- paste(colnames(t1)[cn],"row%",sep = "_")
   }
 }


 #-- add Variable name Var
 var_column = c(paste(quo_name(x),levels(as.factor(d[,1])),sep = "_"),rep("",nrow(result_t)- length(levels(as.factor(d[,1])))))
 result_t = mutate(result_t,Variable=var_column)
 #-- take Variable_column to the front
 result_t <- result_t %>% select(Variable, everything())


 if (show_totals){
   if (row_percent) {
     result_t <- cbind(result_t, t_row_prop[,ncol(t_row_prop)])
     colnames(result_t)[ncol(result_t)] <- "row_totals"
   }

   if (col_percent) {
     result_t <- result_t %>% adorn_totals("row")
     # rownames(result_t)[nrow(result_t)] <- "col_totals"
     if (row_percent) {
       # right now the cols are:
       # Variable + n % row% col% <-- per level so...
       for (i in 1:ncol(t1)){
         col <- 1 + (i*(col_per_level+1))
         result_t[nrow(result_t),col] <- NA
       }

     }
   }
 }

 if (chi){
   chis <- suppressWarnings(chisq.test(t1))
   chis_under_5 <- sum(chis$expected < 5)
   result_t <- rbind(result_t, rep("",ncol(result_t)))
   result_t <- rbind(result_t,c("Num of cells with expected <5",chis_under_5,rep("",ncol(result_t)-2)))
   if (chis_under_5 > 0) result_t <- rbind(result_t,c("Fisher's exact test: ",round(fisher.test(t1)$p.value, digits = decimals),rep("",ncol(result_t)-2)))
   else result_t <- rbind(result_t,c("Pearson's chi square test: ",round(chis$p.value, digits = decimals),rep("",ncol(result_t)-2)))
 }


 rownames(result_t)<-NULL
 colnames(result_t)[1] <- paste("Variable", quo_name(y), sep = "_")


 return(result_t)
}






#' Title freq
#'
#' The freq function creates a dataframe with the frequencies of the independent values in one or more variables passed as arguments.
#' It can also give the information grouping said variable/s by a different one
#'
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
#'
#' @return list of dataframes (1 per variable)
#' @export
#'
#' @examples
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
#'
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

  vars <- enquos(...)
  result_df <- list()

  for (v in vars) {

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

      if (total) result_temp <- result_temp %>% adorn_totals("row")

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
          # r <- r %>% adorn_totals("row", na.rm=T)
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
