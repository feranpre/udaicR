#
# f <- AGE ~ HEIGHT + SEX
# regresion(f, data_)

.new_regresion <- function(dependent, independent, data) {
  obj <- list(dependent = dependent,
              independent = independent,
              data = data)
  class(obj) <- append(class(obj), "udaicR.regression")
  return(obj)
}


regresion <- function(formula, data) {
  if (missing(formula)) print("no formula")
  if (missing(data)) print("no data")

  .validate_args(formula, data)
  model <- lm(formula = formula, data = data)

  factores <- attr(model$terms,"term.labels")
  vars <- as.character(attr(model$terms, "variables"))[-1]


  cat("\nfactores -->", factores)
  cat("\nvars ->", vars)

  for(v in vars) {
    # v <- as.character(v)
    if (!(v %in% factores)) {
      #.. if not one of the factors then is a dependent
      if (!exists("dependent")) dependent <- v
      else dependent <- c(dependent, v)
    }
  }

  cat("\nDEP -->", dependent)
  cat("\nFACT ->", factores)

  .supuesto1_linearidad(dependent = dependent, factores = factores, data = data_)

}

.validate_args <- function(formula, data) {
  if(formula[1] != "`~`()") stop("not formula")
  else print("formula")

  if(!is.data.frame(data))  stop("not data.frame")

  return(TRUE)
}
