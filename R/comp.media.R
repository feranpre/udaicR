

#'
#'
#' @param formula
#'
#' @param data
#' @param variables
#' @param by
#' @param result.mean
#' @param decimals
#' @param show.desc
#' @param DEBUG
#' @param show.warnings
#' @param lang
#'
#' @importFrom car  leveneTest
#' @importFrom PMCMRplus  gamesHowellTest kwAllPairsConoverTest
#'
#' @return
#'
#' @export
#'
# comp.mean <- function(formula = NULL, data = NULL, variables = NULL, by = NULL, result.mean = NULL, decimals = 2,
#                       show.desc = TRUE,
#                       DEBUG = FALSE, show.warnings = FALSE, lang = "en") {
#
#   if(!missing(formula)) UseMethod("comp.mean", formula)
#
#   UseMethod("comp.mean",data)
#
# }
#
#
# comp.mean.formula <- function(formula, ...) {
#   print(formula)
# }


#' @export
comp.mean <- function(data, variables, by, result.mean = NULL, decimals = 2,
                                 show.desc = TRUE,
                                 DEBUG = FALSE, show.warnings = FALSE, lang = "en") {

  data.validate <- .media.validate.data(data, variables = variables, by = by, lang = lang)
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
    total.cats <- length(raw.mean$norm.test[raw.mean$var == var])
    var.normal <- ifelse(normal.cats == total.cats, TRUE, FALSE)

    temp.data.frame = as.data.frame(temp.data)
    names(temp.data.frame) <- var
    if (DEBUG) cat("\nNum NORMAL cats:", normal.cats, " All normal cats:", var.normal,"\n\n")
    #--- var has all categories normal
    if (total.cats < 2) stop("TOTAL.CAT < 2, no comparison possible")
    else if (total.cats == 2 ) result.temp <- comp.mean.2.groups(temp.data, by = by.var, normal = var.normal, decimals = decimals)
    else if (total.cats > 2 ) result.temp <- comp.mean.many.groups(temp.data.frame, by = by.var, by.name = by.name, normal = var.normal, decimals = decimals)

    # print(result.temp)
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






#' @importFrom car  leveneTest
#' @importFrom PMCMRplus  gamesHowellTest kwAllPairsConoverTest
comp.mean.many.groups <- function(data, formula = NULL, by=NULL, by.name = NULL, normal=FALSE, DEBUG = FALSE, decimals = 2) {
  if (!require("PMCMRplus", quietly = TRUE)) {
    stop(.comp.mean.error.text(lang,"PKG.PMCMRplus"), call. = FALSE)
  }
  if (!require("car", quietly = TRUE)) {
    stop(.comp.mean.error.text(lang,"PKG.CAR"), call. = FALSE)
  }


  data <- cbind(data, by)
  names(data)[2] <- by.name
  small.p <- 10^((decimals+1)*-1)
  if(missing(formula))  formula <- formula(paste(names(data)[1],"~",by.name))

  if (normal) {

    # res.temp <- aov(as.formula(paste(names(data)[1],"~",by.name)), data = data)
    res.temp <- aov(formula, data = data)
    res.temp.sum <- summary(res.temp)
    n.missing <- length(res.temp$na.action)
    num.ranks <- res.temp$rank
    # cat("\n(comp.mean.many.groups) Ranks:", num.ranks,"\n")
    # df.res <- res.temp$df.residual

    p.value <- res.temp.sum[[1]]$`Pr(>F)`[1]
    p.text <- ifelse(p.value < small.p, paste("<", small.p), round(p.value, digits = decimals+1))
    p.value <- round(p.value, digits = decimals+1)

    f.value <- round(res.temp.sum[[1]]$`F value`[1], digits = decimals)

    var.test.p <- car::leveneTest(data[,1], group = as.factor(data[,2]))$`Pr(>F)`[1]
    homocedasticity <- ifelse(var.test.p < 0.05, FALSE, TRUE)
    var.test.p <- round(var.test.p, digits = decimals+1)


    if(homocedasticity == T){
      result <- as.data.frame(TukeyHSD(res.temp)[[1]])
      result$diff <- round(result$diff, digits = decimals)
      result$lwr <- round(result$lwr, digits = decimals)
      result$upr <- round(result$upr, digits = decimals)

      result$`p adj` <- ifelse(result$`p adj` < small.p, paste("<", small.p), round(result$`p adj`, digits = decimals+1))

      result <- cbind(stat.value = c(rep(NA,nrow(result))), result)
      result <- cbind(levene.p = c(rep(NA,nrow(result))), result)
      result <- cbind(categories = rownames(result), result)
      result <- rbind(c("ANOVA",var.test.p, f.value,NA,NA, NA,p.value), result)
      rownames(result) <- NULL
      names(result) <- c("categories","levene.p","stat.value","mean.diff","ci.low","ci.up","p.value")
      result$method <- c("Tukey HDS", rep(NA, nrow(result)-1))
    }
    else {
      gh <- PMCMRplus::gamesHowellTest(res.temp)
      for(x in dimnames(gh$p.value)[[1]]){
        for(y in dimnames(gh$p.value)[[2]]){
          if(x==y) next
          temp.group <- paste(x,"-",y)
          temp.p.value <- gh$p.value[x,y]
          temp.stat.value <- gh$statistic[x,y]

          temp.res <- data.frame(stat.value = temp.stat.value, mean.diff = NA, ci.low = NA, ci.up = NA, p.value = temp.p.value)
          rownames(temp.res) <- temp.group
          if(!exists("result")) result <- temp.res
          else result <- rbind(temp.res, result)
        }
      }
      result <- cbind(levene.p = c(rep(NA,nrow(result))), result)
      result <- cbind(categories = rownames(result), result)
      result$p.value <- ifelse(result$p.value < small.p, paste("<", small.p), round(result$p.value, digits = decimals+1))
      result <- rbind(c("ANOVA",var.test.p, f.value,p.value), result)
      result$method <- c("Games-Howell", rep(NA, nrow(result)-1))
      rownames(result) <- NULL
    }

    # GRAFICO --> https://statsandr.com/blog/anova-in-r/#post-hoc-test
    # Significacion de post-hoc: https://www.statology.org/anova-post-hoc-tests/

  }
  else {
    res.temp <- kruskal.test(formula, data = data)
    res.temp.sum <- summary(res.temp)

    p.value <- res.temp$p.value
    p.text <- ifelse(p.value < small.p, paste("<", small.p), round(p.value, digits = decimals+1))

    chi.value <- round(res.temp$statistic, digits = decimals)

    gh <- kwAllPairsConoverTest(formula, data= data)
    # gh <- kwAllPairsDunnTest(formula, data= data)
    for(x in dimnames(gh$p.value)[[1]]){
      for(y in dimnames(gh$p.value)[[2]]){
        if(x==y) next
        temp.group <- paste(x,"-",y)
        temp.p.value <- gh$p.value[x,y]
        temp.stat.value <- round(gh$statistic[x,y], digits = decimals)

        temp.res <- data.frame(levene.p = NA, stat.value = temp.stat.value, mean.diff = NA, ci.low = NA, ci.up = NA, p.value = temp.p.value)
        rownames(temp.res) <- temp.group
        if(!exists("result")) result <- temp.res
        else result <- rbind(temp.res, result)
      }
    }


    # print(result)
    # result <- cbind(levene.p = c(rep(NA,nrow(result))), result)
    result <- cbind(categories = rownames(result), result)
    result$p.value <- ifelse(result$p.value < small.p, paste("<", small.p), round(result$p.value, digits = decimals+1))
    result <- rbind(c("Kruskal-Wallis", NA, chi.value, NA, NA, NA, p.text), result)
    result$method <- c("Conover Test", rep(NA, nrow(result)-1))
    rownames(result) <- NULL


  }
  return(result)

}







comp.mean.2.groups <- function(data, by=NULL, normal=FALSE, DEBUG = FALSE, decimals = 2, lang = "en") {
  # data <- as.data.frame(data)

  if (!require("car", quietly = TRUE)) {
    stop(.comp.mean.error.text(lang,"PKG.CAR"), call. = FALSE)
  }


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
      ci.low = round(temp$conf.int[1], digits = decimals),
      ci.up = round(temp$conf.int[2], digits = decimals),
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
      ci.low = round(temp$conf.int[1], digits = decimals),
      ci.up = round(temp$conf.int[2], digits = decimals),
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



#' @export
print.udaicR_mean_comp <- function(obj, ...) {
  # print(paste("COMP",attr(obj,"show.desc")))
  if ("knitr" %in% rownames(installed.packages())){
    if (attr(obj,"show.desc")) {
      # print("DESCRIPTIVOS")
      print(knitr::kable(attr(obj,"desc")))
      }
    print(knitr::kable(as.data.frame(obj)))
  } else {
    print(as.data.frame(obj))
  }

}



.comp.mean.error.text <- function(lang = "en", code = "") {
  error.text <- list()
  error.text[["en"]] <- c(
    GENERIC = "generic error in udaicR::comp.mean",
    PKG.PMCMRplus = "package PMCMRplus, not installed, please install using 'install.packages('PMCMRplus')'",
    PKG.CAR = "package car, not installed, please install using 'install.packages('car')'"

  )
  error.text[["es"]] <- c(
    GENERIC = "error generico en la función udaicR::comp.mean",
    PKG.PMCMRplus = "package PMCMRplus, not installed, please install using 'install.packages('PMCMRplus')'",
    PKG.CAR = "package car, not installed, please install using 'install.packages('car')'"
  )

  if(!is.na(error.text[[lang]][code])) return(error.text[[lang]][code])
  else return(error.text[[lang]]["GENERIC"])
}

