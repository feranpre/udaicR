

#' @importFrom cars, leveneTest
comp.media <- function(data, variables, by, result.mean = NULL, decimals = 2, DEBUG = FALSE, show.warnings = FALSE, lang = "en") {

  data.validate <- .media.validate.data(data, variables, by, lang = lang)
  data.final <- data.validate[["data"]]
  by.name <- data.validate[["by"]]
  variables.names <- data.validate[["variables"]]

  if(missing(result.mean)) result.mean <- media(data = data.final, variables = variables.names, by = by.name, decimals = decimals, show_warnings = show_warnings)

  by.name <- attr(result.mean,"by")
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
    var.normal <- ifelse(normal.cats == length(raw.mean$norm.test[raw.mean$var == var]), TRUE, FALSE)

    if (DEBUG) cat("\nNum NORMAL cats:", normal.cats, " All normal cats:", var.normal,"\n\n")
    #--- var has all categories normal
    result.temp <- comp.media.2.groups(temp.data, by = by.var, normal = var.normal)
    result.temp <- cbind(var = var, result.temp)
    if(!exists("results")) results <- result.temp
    else results <- rbind(results, result.temp)


  }
  return(results)

}


comp.media.2.groups <- function(data, by=NULL, normal=FALSE, DEBUG = FALSE) {

  if (normal) {
    #--- t.student
    var.test.p <- car::leveneTest(data, group = as.factor(by))$`Pr(>F)`[1]
    homocedasticity <- ifelse(var.test.p < 0.05, FALSE, TRUE)

    temp <- t.test(data ~ by, equal.var = homocedasticity)
    result <- data.frame(
      method = "Welch t-test",
      levene.p = var.test.p,
      stat.value = temp$statistic,
      p.value = temp$p.value,
      mean.diff = temp$estimate[1] - temp$estimate[2],
      mean.diff.ci.up = temp$conf.int[2],
      mean.diff.ci.low = temp$conf.int[1]
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
      stat.value = temp$statistic,
      p.value = temp$p.value,
      mean.diff = temp$estimate,
      mean.diff.ci.up = temp$conf.int[2],
      mean.diff.ci.low = temp$conf.int[1]
    )
      # OJO!!!!!!
      # difference in location no es la diferencia de medianas sino la mediana de las diferencias!!!!

      # - Incluir bootstrap
      # https://data.library.virginia.edu/the-wilcoxon-rank-sum-test/
      #Hogg, R.V. and Tanis, E.A., Probability and Statistical Inference, 7th Ed, Prentice Hall, 2006.
      #R Core Team (2016). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

  }

  return(result)
}


#
#   result.mean <- result.mean[result.mean$var != "---",]
#   if (DEBUG) cat("[DEBUG] (media.compare) by:", by.name, "\n")
#   if (DEBUG) cat("[DEBUG] (media.compare) result.mean:", paste(result.mean), "\n")
#   levels.list <- levels(as.factor(data[,by.name]))
#
#   by.values <- data[,by]
#
#   for (v in names(data)){
#
#     var.values <- data[,v]
#     if((is.numeric(var.values)) & (v != by)) {
#       if(DEBUG) cat("\n(media.compare) VAR: ", v)
#       #if there are as many TRUE as groups, then var.normal = TRUE, else FALSE
#       var.normal <- ifelse(sum(result.mean[result.mean$var == v, "norm.test"]) == nrow(result.mean[result.mean$var == v,]), TRUE, FALSE)
#       # var.normal <- udaicR::is_normal(var.values)
#       if(DEBUG) cat("\n(media.compare) VAR: ", v, " --NORMAL:", var.normal)
#
#       #---------------------------------------- menos de dos
#       if (length(levels.list) < 2) {
#         if(show.warnigns) cat(.media.error.text(lang,"COMP_LESS_2_GROUPS"))
#       }
#       #--------------------------------------------------------------------- START --------- dos grupos
#       else if (length(levels.list) == 2) {
#         #---------------------- START ---- if var is normal -------------
#         if(var.normal) {
#           # if(DEBUG) cat("\n\n(media.compare) Var is normal, comparing\n\n")
#           #--- t.student
#           var.test.p <- car::leveneTest(var.values, group = as.factor(by.values))$`Pr(>F)`[1]
#           homocedasticity <- ifelse(var.test.p < 0.05, FALSE, TRUE)
#
#           temp <- t.test(var.values ~ by.values, equal.var = homocedasticity)
#           method = "Welch t-test"
#           stat.value = temp$statistic
#           p.value = temp$p.value
#           mean.diff = temp$estimate[1] - temp$estimate[2]
#           mean.diff.ci.up = temp$conf.int[2]
#           mean.diff.ci.low = temp$conf.int[1]
#         }
#         #---------------------- END ---- if var is normal -------------
#         #---------------------- START -- if var is NOT NORMAL
#         else {
#           temp <- tryCatch({
#             wilcox.test(var.values ~ by.values, conf.int = TRUE, correct = TRUE, exact = TRUE)
#           }, warning = function(warn) {
#             if (warn$message == "cannot compute exact p-value with ties") {
#               #----------------------------------------------------------------- there are ties, aborting wilcoxon
#               if(DEBUG) cat("\n(media.compare) 2 GROUPS - NOT NORMAL - there are ties. VAR ->", v)
#               return(wilcox.test(var.values ~ by.values, conf.int = TRUE, correct = TRUE, exact = FALSE))
#             }
#             else return(wilcox.test(var.values ~ by.values, conf.int = TRUE, correct = TRUE, exact = FALSE))
#           }, error = function(err) {
#             stop(err)
#           }
#           )
#
#           method = "Wilcox-Mann-Whitney"
#           var.test.p = NA
#           stat.value = temp$statistic
#           p.value = temp$p.value
#           mean.diff = temp$estimate
#           mean.diff.ci.up = temp$conf.int[2]
#           mean.diff.ci.low = temp$conf.int[1]
#           # OJO!!!!!!
#           # difference in location no es la diferencia de medianas sino la mediana de las diferencias!!!!
#
#           # - Incluir bootstrap
#           # https://data.library.virginia.edu/the-wilcoxon-rank-sum-test/
#           #Hogg, R.V. and Tanis, E.A., Probability and Statistical Inference, 7th Ed, Prentice Hall, 2006.
#           #R Core Team (2016). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
#
#         }
#         #---------------------- END -- if var is NOT NORMAL
#
#
#         #--- forming results for 2 groups
#         result.temp <- data.frame(var = v, levene.p = var.test.p, method = method,
#                                   stat = stat.value, p = p.value, mean.diff = mean.diff,
#                                   CI.low = mean.diff.ci.low, CI.high = mean.diff.ci.up)
#
#       }
#       #--------------------------------------------------------------------- END --------- dos grupos
#       #--------------------------------------------------------------------- START ------- mas de dos grupos
#       else if (length(levels.list) > 2) {
#       }
#       #--------------------------------------------------------------------- END ------- mas de dos grupos
#
#
#       if(exists("result.temp")){
#         # if (DEBUG) print(result.temp)
#         if(!exists("result.final")) result.final <- as.data.frame(result.temp)
#         else result.final <- rbind(result.final, result.temp)
#         rm(list=c("result.temp", "temp", "var.test.p", "method", "stat.value", "p.value", "mean.diff", "mean.diff.ci.low", "mean.diff.ci.up"))
#         if(DEBUG) cat("\n(media.compare) Clearing variables\n")
#       }
#
#     } #--- end if 'by' is valid
#   } #--- end variables in data.frame loop
#
#   class(result.final) <- append("udaicR_mean_comp", class(result.final))
#   return(result.final)
#
# }



print.udaicR_mean_comp <- function(obj, ...) {
  if ("knitr" %in% rownames(installed.packages()))
    print(knitr::kable(as.data.frame( obj )))
  else
    print(as.data.frame(obj))
}
