

#' @importFrom cars, leveneTest
comp.mean <- function(formula = NULL, data = NULL, variables = NULL, by = NULL, result.mean = NULL, decimals = 2,
                      show.desc = TRUE,
                      DEBUG = FALSE, show.warnings = FALSE, lang = "en") {

  if(!missing(formula)) UseMethod("comp.mean", formula)

  UseMethod("comp.mean",data)

}


comp.mean.formula <- function(formula, ...) {
  print(formula)
}



comp.mean.data.frame <- function(data, variables, by, result.mean = NULL, decimals = 2,
                                 show.desc = TRUE,
                                 DEBUG = FALSE, show.warnings = FALSE, lang = "en") {

  data.validate <- .media.validate.data(data, variables = variables, by = by, lang = lang)
  data.final <- data.validate[["data"]]
  by.name <- data.validate[["by"]]
  variables.names <- data.validate[["variables"]]

  if(missing(result.mean)) result.mean <- media(data = data.final, variables = variables.names, by = by.name, decimals = decimals, show_warnings = show_warnings)

  # by.name <- attr(result.mean,"by")
  by.levels <- attr(result.mean,"by.levels")
  by.num.levels <- attr(result.mean,"by.num.levels")
  by.var <- data[,by.name]

  #----------- now we have the means
  raw.mean <- attr(result.mean,"raw.result")


  if(DEBUG) cat("\n(comp.media) \n by.name: ", by.name, "\n by.levels:", by.levels, "\n by.num.levels:", by.num.levels,"\n\n")

  # print(raw.mean)

  for(var in names(data.final)) {
    if(var == by.name) next
    temp.data <- data.final[,var]
    if(!is.numeric(temp.data)) temp.data <- as.numeric(temp.data)
    if(sum(is.na(temp.data)) == length(temp.data)) next
    normal.cats <- sum(raw.mean$norm.test[raw.mean$var == var])
    total.cats <- length(raw.mean$norm.test[raw.mean$var == var])
    var.normal <- ifelse(normal.cats == total.cats, TRUE, FALSE)

    if (DEBUG) cat("\nNum NORMAL cats:", normal.cats, " All normal cats:", var.normal,"\n\n")
    #--- var has all categories normal
    if (total.cats < 2) stop("TOTAL.CAT < 2, no comparison possible")
    else if (total.cats == 2 ) result.temp <- comp.mean.2.groups(temp.data, by = by.var, normal = var.normal, decimals = decimals)
    else if (total.cats > 2 ) result.temp <- comp.mean.many.groups(temp.data, by = by.var, normal = var.normal, decimals = decimals)

    if (exists("result.temp")) result.temp <- cbind(var = var, result.temp)
    if(!exists("results")) results <- result.temp
    else results <- rbind(results, result.temp)
    rm(list="result.temp")
  }

  class(results) <- append("udaicR_mean_comp", class(results))


  attr(results, "desc") <- result.mean
  attr(results, "show.desc") <- show.desc

  return(results)

}








comp.mean.many.groups <- function(data, by=NULL, normal=FALSE, DEBUG = FALSE, decimals = 2) {
  if (normal) {

  }
  else {

  }


}







comp.mean.2.groups <- function(data, by=NULL, normal=FALSE, DEBUG = FALSE, decimals = 2) {

  if (normal) {
    #--- t.student
    var.test.p <- car::leveneTest(data, group = as.factor(by))$`Pr(>F)`[1]
    homocedasticity <- ifelse(var.test.p < 0.05, FALSE, TRUE)

    temp <- t.test(data ~ by, equal.var = homocedasticity)
    result <- data.frame(
      method = "Welch t-test",
      levene.p = round(var.test.p, digits = decimals),
      stat.value = round(temp$statistic, digits = decimals),
      mean.diff = round(temp$estimate[1] - temp$estimate[2], digits = decimals),
      mean.diff.ci.low = round(temp$conf.int[1], digits = decimals),
      mean.diff.ci.up = round(temp$conf.int[2], digits = decimals),
      p.value = round(temp$p.value, digits = decimals)



    )
  } else {
    temp <- tryCatch({
                        wilcox.test(data ~ by, conf.int = TRUE, correct = TRUE, exact = TRUE)
                      }, warning = function(warn) {
                        if (warn$message == "cannot compute exact p-value with ties") {
                          #----------------------------------------------------------------- there are ties, aborting wilcoxon
                          if(DEBUG) cat("\n(media.compare) 2 GROUPS - NOT NORMAL - there are ties. VAR ->", v)
                          return(wilcox.test(data ~ by, conf.int = TRUE, correct = TRUE, exact = FALSE))
                        }
                        else return(wilcox.test(data ~ by, conf.int = TRUE, correct = TRUE, exact = FALSE))
                      }, error = function(err) {
                        stop(err)
                      }
      )
    result <- data.frame(
      method = "Wilcox-Mann-Whitney",
      levene.p = NA,
      stat.value = round(temp$statistic, digits = decimals),
      mean.diff = round(temp$estimate, digits = decimals),
      mean.diff.ci.low = round(temp$conf.int[1], digits = decimals),
      mean.diff.ci.up = round(temp$conf.int[2], digits = decimals),
      p.value = round(temp$p.value, digits = decimals)
    )
      # OJO!!!!!!
      # difference in location no es la diferencia de medianas sino la mediana de las diferencias!!!!

      # - Incluir bootstrap
      # https://data.library.virginia.edu/the-wilcoxon-rank-sum-test/
      #Hogg, R.V. and Tanis, E.A., Probability and Statistical Inference, 7th Ed, Prentice Hall, 2006.
      #R Core Team (2016). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
  }

  if (exists("result")){
    small.p <- 10^(-1*(decimals + 1))
    if (!is.na(result$levene.p) & (result$levene.p < small.p)) result$levene.p <- paste0("<",small.p)
    if (result$p.value < small.p) result$p.value <- paste0("<",small.p)


    return(result)
  }
  else stop("NO RESULT")
}




print.udaicR_mean_comp <- function(obj, ...) {
  if ("knitr" %in% rownames(installed.packages())){
    if (attr(obj,"show.desc")) print(knitr::kable(attr(obj,"desc")))
    print(knitr::kable(as.data.frame(obj)))
  } else {
    print(as.data.frame(obj))
  }

}
