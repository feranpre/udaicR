#' riemann.sum
#'
#' This function calculates the area under the curve for a density function.
#'
#' @import dplyr
#'
#' @param df
#' @param ...               variables to be studied (no quotes)
#' @param group_by_col      name of the variable for the grouping (if there is one)
#' @param inc.value         split the studied variable in increments of x
#' @param min.value         min value of x for witch the riemann.sum will estimate the area under the curve
#' @param max.value         max value of x for witch the riemann.sum will estimate the area under the curve
#' @param debug             debuggin options on or off
#'
#' @return                  dataframe with values
#' @export
#'
#' @examples
riemann_sum <- function(df,...,group_by_col=NULL,inc.value=NULL, min.value=NULL, max.value=NULL, debug=FALSE){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  vars <- enquos(...)

  for(v in vars){
    if(debug)print(paste("Var->",quo_name(v)))

    if (missing("group_by_col")){
      if(debug) print("No groups selected")
      x <- df[,quo_name(v)]
      if(class(x) == "difftime") x <- as.numeric(x)
      if (missing("min.value")) temp.min = min(x,na.rm = TRUE)
      else temp.min = min.value

      if (missing("max.value")) temp.max = max(x,na.rm = TRUE)
      else temp.max = max.value

      if (missing("inc.value")) temp.increment = max(x,na.rm = TRUE) - temp.min
      else temp.increment = inc.value

      result.df <- riemann.sum.basic(x, min.value = temp.min, max.value = temp.max, inc.value = temp.increment)
      if(debug) print(result.df)

    } else {
      if(debug) print("Groups selected")
      agrupa <- enquos(group_by_col)
      for (v.grupo in agrupa) {

        var.grupo <- df[,quo_name(v.grupo)]
        for (grupo in levels(var.grupo)){
          x <- df[var.grupo == grupo, quo_name(v)]
          if (missing("min.value")) temp.min = min(x,na.rm = TRUE)
          else temp.min = min.value

          if (missing("max.value")) temp.max = max(x,na.rm = TRUE)
          else temp.max = max.value

          if (missing("inc.value")) temp.increment = max(x,na.rm = TRUE) - temp.min
          else temp.increment = inc.value

          result.df.temp <- riemann.sum.basic(x, min.value = temp.min, max.value = temp.max, inc.value = temp.increment)

          names.old <- names(result.df.temp)
          result.df.temp <- cbind(c(grupo,rep("",nrow(result.df.temp)-1)),result.df.temp)
          names(result.df.temp) <- c(quo_name(v.grupo), names.old)

          if(debug){
            print(paste0("Grupo->",grupo))
            print(result.df.temp)
          }
          if(!exists("result.df")) result.df <- result.df.temp
          else result.df <- rbind(result.df, result.df.temp)
        }
        if (debug) print(result.df)
      }
    }
    names.old <- names(result.df)
    result.df <- cbind(c(quo_name(v),rep("",nrow(result.df)-1)),result.df)
    names(result.df) <- c("Var",names.old)

    if(!exists("result.df.final")) result.df.final <- result.df
    else result.df.final <- rbind(result.df.final, result.df)
    rm("result.df")
  }
  if (exists("result.df.final")) return(result.df.final)
}

riemann.sum.basic <- function(x,inc.value=NULL, min.value = NULL, max.value = NULL){
  d.temp <- density(x, na.rm = T)
  xx <- d.temp$x
  dx <- xx[2L] - xx[1L]
  yy <- d.temp$y
  C <- sum(yy)*dx

  while(min.value < max.value){
    t.max = min.value + inc.value
    if (t.max > max.value) t.max = max.value
    p.unscaled <- sum(yy[xx>=min.value & xx<=t.max]) * dx
    p.scaled <- p.unscaled / C
    # print(paste0(min.value,"-",t.max))
    # print(p.scaled)
    # print("---------------------------------")
    if (!exists("result.df")) result.df <- data.frame(min = min.value, max = t.max, p.scaled = p.scaled )
    else result.df <- rbind(result.df, c(min.value, t.max, p.scaled))
    min.value = t.max
  }
  # print(result.df)
  if (exists("result.df")) return(result.df)
}
