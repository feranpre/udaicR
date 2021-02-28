



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
#' means(data_, AGE)
#'
#' means(data_, AGE,HEIGHT)
#'
#' mean(data_, AGE, group_by_col=SEX)
#'
#' }
#'
means <- function(df, ... , group_by_col = NULL, decimales=2, show_warnings = TRUE, show_errors = FALSE,
                  n=TRUE, missing=TRUE,
                  min=TRUE, max= TRUE, mean=TRUE, sd=TRUE,
                  median=TRUE, range=TRUE, norm.test = TRUE,
                  col_names = c("var","groups","n.valid","n.missing","min","max","mean","sd","median","range","shapiro-wilk"),
                  lang = "en", DEBUG = FALSE, show.help = FALSE) {

  # if (!requireNamespace("dplyr", quietly = TRUE)) {
  #   stop("Package \"dplyr\" needed for this function to work. Please install it.",
  #        call. = FALSE)
  # }
  # library(dplyr, quietly = TRUE)

  if(show_warnings) .Deprecated("medias")

  vars <- enquos(...)
  result_df <- list()

  var.names <- sapply(vars, quo_name)
  # print(var.names)
  by.name <- NA
  if(!missing(group_by_col)) by.name = quo_name(enquos(group_by_col)[[1]])
  return(media(df, variables=var.names, by = by.name, decimals=decimales,
               show_warnings = show_warnings, show_errors = show_errors,
               n=n, missing=missing,
               min=min, max= max, mean=mean, sd=sd,
               median=median, IQR=range, norm.test = norm.test, col_names=col_names, lang=lang, DEBUG = DEBUG ))
}
