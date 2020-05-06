#' Title means
#'
#' My own spin on the mean function, working as I like it
#'
#' @param df              data
#' @param ...             any number of numerical variables
#' @param group_by_col    variable to group the others by
#' @param decimales       number of decimal to show
#' @param show_warnings   show warnings
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
means <- function(df, ... , group_by_col = NULL, decimales=2, show_warnings = TRUE, n=TRUE, missing=TRUE,
                       min=TRUE, max= TRUE, mean=TRUE, sd=TRUE,
                       median=TRUE, range=TRUE, norm.test = TRUE,
                       col_names = c("var","groups","n.valid","n.missing","min","max","mean","sd","median","range","shapiro")) {
  vars <- enquos(...)
  result_df <- list()

  for (v in vars) {
    vs <- quo_name(v)

    # --- NO GROUPING
    if (missing("group_by_col")) {
      # result_temp <- df[quo_name(v)] %>% summarise(
      result_temp <- df %>% summarise(
                                    n = n()-sum(is.na(.data[[quo_name(v)]])),
                                    missing = sum(is.na(.data[[quo_name(v)]])),
                                    min = round(min(!! v, na.rm=TRUE), digits=decimales),
                                    max = round(max(!! v, na.rm=TRUE), digits=decimales),
                                    mean = round(mean(!! v, na.rm = TRUE), digits=decimales),
                                    sd = round(sd(!! v, na.rm = TRUE), digits=decimales),
                                    median = round(median(!! v, na.rm = TRUE), digits=decimales),
                                    range = round(max(!! v, na.rm = TRUE) - min(!! v, na.rm = TRUE), digits=decimales),
                                    shapiro = shapiro.test(!! v)$p.value
                                  )
    }
    else {
      agrupa <- enquos(group_by_col)
      s.test <- shapiro.test(df[,quo_name(v)])$p.value
      result_temp <- df %>%
                        group_by(!!! agrupa) %>%
                              summarise(
                                  n = n()-sum(is.na(.data[[quo_name(v)]])),
                                  missing = sum(is.na(.data[[quo_name(v)]])),
                                  min = ifelse(all(is.na(!! v)),NA,round(min(!! v, na.rm=TRUE), digits=decimales)),
                                  max = ifelse(all(is.na(!! v)),NA,round(max(!! v, na.rm=TRUE), digits=decimales)),
                                  mean = round(mean(!! v, na.rm = TRUE), digits=decimales),
                                  sd = round(sd(!! v, na.rm = TRUE), digits=decimales),
                                  median = round(median(!! v, na.rm = TRUE), digits=decimales),
                                  range = ifelse(all(is.na(!! v)),NA, round(max(!! v, na.rm = TRUE) - min(!! v, na.rm = TRUE), digits=decimales)),
                                  shapiro = s.test
                                )
    }
    result_temp <- as.data.frame(result_temp)

    if (n == FALSE) {
      col_names <- col_names[-3]
      result_temp$n <- NULL
    }
    if (missing == FALSE) {
      col_names <- col_names[-4]
      result_temp$missing <- NULL
    }
    if (min == FALSE) {
      col_names <- col_names[-5]
      result_temp$min <- NULL
    }
    if (max == FALSE) {
      col_names <- col_names[-6]
      result_temp$max <- NULL
    }
    if (mean == FALSE) {
      col_names <- col_names[-7]
      result_temp$mean <- NULL
    }
    if (sd == FALSE) {
      col_names <- col_names[-8]
      result_temp$sd <- NULL
    }
    if (median == FALSE) {
      col_names <- col_names[-9]
      result_temp$median <- NULL
    }
    if (range == FALSE) {
      col_names <- col_names[-10]
      result_temp$range <- NULL
    }
    if (norm.test == FALSE) {
      col_names <- col_names[-11]
      result_temp$shapiro <- NULL
    }
    else result_temp$shapiro <- c(rep(NA,nrow(result_temp)-1),result_temp$shapiro[1])

    # -- add a column with the name of the var being studied
    result_temp <- cbind(c(quo_name(v),rep("",nrow(result_temp)-1)),result_temp)
    # colnames(result_temp)[1]<- "Var"

    # we take the groups col name the last to make all the rest of the code usable,
    # if we took it before this point all the
    if (missing("group_by_col")) col_names <- col_names[-2]

    colnames(result_temp) <- col_names

    if (exists("result_df")) result_df <- rbind(result_df,result_temp)
    else result_df <- result_temp
  }
  return(result_df)
}

media <- means
