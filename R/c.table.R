#' Title c.table (contingency table)
#'
#' @param df            dataframe to be used
#' @param x             variable that will be on the columns
#' @param y             variable that will be on the rows
#' @param col_percent   T/F show percentage by columns
#' @param row_percent   T/F show percentage by rows
#' @param show_totals   T/F show totals
#' @param chi           T/F perform proportions comparison (chi.square or fisher, as needed)
#' @param decimals      number of decimal digits
#'
#' @return
#' @export
#'
#' @examples
c_table <- function(df,x,y, col_percent = TRUE, row_percent = FALSE, show_totals = TRUE,
                chi = TRUE, decimals = 2, debug = FALSE) {
 if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
      call. = FALSE)
 }
 if (!requireNamespace("janitor", quietly = TRUE)) {
      stop("Package \"janitor\" needed for this function to work. Please install it.",
           call. = FALSE)
 }


 library(dplyr)
 library(janitor)

 x <- enquo(x)
 y <- enquo(y)

 if(debug) print("Preparando tablas")
 d <- as.data.frame(df %>% select(!! x,!! y))

 if (debug) {
    print(d)
    print("VAR-1:")
    print(d[,1])
    print("VAR-2:")
    print(d[,2])
 }

 t1 <- table(d[,1], d[,2])
 t_row_prop <- round(prop.table(t1, margin = 1)*100, digits = decimals) #.. row
 t_row_prop <- as.data.frame.matrix(t_row_prop) %>% mutate(row_totals = rowSums(.))
 t_col_prop <- round(prop.table(t1, margin = 2)*100, digits = decimals) #.. col
 # t_col_prop <- as.data.frame.matrix(t_col_prop) %>% mutate(col_totals = colSums(.))

 if (debug){
    print("TABLES:")
    print(t1)
    print("-----------")
    print(t_row_prop)
    print("-----------")
    print(t_col_prop)
 }

 col_per_level = 0
 if (col_percent) col_per_level = col_per_level +1
 if (row_percent) col_per_level = col_per_level +1

 if(debug) print("PRE loop")

 for (cn in 1:ncol(t1)) {
   if (!exists("result_t")) {
     result_t <- t1[,cn]
     result_t <- as.data.frame(result_t)
   }
   else result_t <- cbind(result_t,t1[,cn])

   col_name_num = cn+(col_per_level*(cn-1))

   if (debug) print(paste(colnames(t1)[cn],"n",sep = "_"))

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

