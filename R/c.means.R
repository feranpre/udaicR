

c.means <- function(df,x,group, decimals = 2, alternative = "two.sided",
                    debug = FALSE, show.test.method = TRUE,
                    conf.level = 0.95, show.warnings = TRUE, is.normal = TRUE) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!exists("conf.level")) {
    if (show.warnings) warning("conf.level is empty, using 0.95 default value")
    conf.level = 0.95
  } else {
    if (conf.level >= 1) {
      if (show.warnings) warning("conf.level is 1, using 0.95 default value")
      conf.level = 0.95
    }
    else if (conf.level <= 0) {
      if (show.warnings) warning("conf.level is 0, using 0.95 default value")
      conf.level = 0.95
    }
  }

  if ((!exists("decimals")) | (is.null(decimals)) ){
    if (show.warnings) warning("decimals is empty, using 2 default value")
    decimals = 2
  }

  if (!exists("alternative")) {
    if (show.warnings) warning("alternative hypothesis is empty, using 'two.sided' default value")
    alternative = "two.sided"
  } else if ((alternative != "two.sided") & (alternative != "greater") & (alternative != "less")) {
    if (show.warnings) warning('alternative parameter was used but with incorrect option. Options available are: "two.sided","greater","less". Defaulting to "two.sided"')
    alternative = "two.sided"
  }


  x <- rlang::sym(rlang::as_label(rlang::enquo(x)))
  group <- rlang::sym(rlang::as_label(rlang::enquo(group)))

  df <- df[!is.na(df[,quo_name(group)]),]

  # names(df) <- c("x","group")
  # c.group <- class(df[,quo_name(group)])

  # if (class(c.group) != "factor"){
  #   if(debug) print(paste("Group is not a factor. Trying conversion.(",c.group,")",sep = ""))
  #   df[,quo_name(group)] <- as.factor(df[,quo_name(group)])
  # }
  #
  if(is.factor(df[,quo_name(group)])) niveles <- levels(df[,quo_name(group)])
  else niveles <- levels(as.factor(df[,quo_name(group)][[1]]))
  if (length(niveles) != 2){
    print(paste("ERROR: Levels of group variable not equal to 2. Aborting. Levels:",niveles, " --> ", length(niveles)))
    return(NULL)
  }

  exp1 <- rlang::expr(!! x ~ !! group)
  df2 <- df[,c(quo_name(x),quo_name(group))] %>% group_split(!! group)

  if (debug) print(df2)


  levene <- var.test(eval(exp1), data =df)
  if (levene$p.value < 10^((decimals+1)*-1)) levene$p.value <- paste("<",as.character(10^((decimals+1)*-1)),"")
  else levene$p.value <- round(levene$p.value, digits = decimals)

  varianzas.iguales = ifelse(levene$p.value<0.05,TRUE,FALSE)

  if (debug) print(paste("LEVENE's p:",levene))


  if (is.normal) {
    temp <- t.test(formula = eval(exp1), data = df, alternative = alternative, var.equal = varianzas.iguales, conf.level = conf.level)
    medias <- round(temp$estimate,digits = decimals)
  }
  else {
    temp <- wilcox.test(formula = eval(exp1), data = df, alternative = alternative, var.equal = varianzas.iguales, conf.level = conf.level)
    medias <- c("","")
  }

  if (debug){
    print(temp)
    print(paste("MEANS ->",c("",medias)))
  }

  temp.p <- temp$p.value
  if (temp.p < 10^((decimals+1)*-1)) temp.p <- paste("<",as.character(10^((decimals+1)*-1)),"")
  else temp.p <- round(temp.p, digits = decimals)


  result.df <- data.frame(groups = c("",niveles),
                          mean.var = c(quo_name(x),"",""),
                          n = c("", nrow(df2[[1]][,quo_name(x)]),nrow(df2[[2]][,quo_name(x)])),
                          homocedasticity.p = c(ifelse(show.test.method,levene$method,""),"",levene$p.value),
                          t = c(ifelse(show.test.method,temp$method,""),"",round(temp$statistic,digits = decimals)),
                          p = c("","",temp.p),
                          means = c("",medias),
                          IC95.low = c("",round(t.test(df2[[1]][,quo_name(x)], conf.level = conf.level)$conf.int[1],digits = decimals),
                                          round(t.test(df2[[2]][,quo_name(x)], conf.level = conf.level)$conf.int[1],digits = decimals)
                                      ),
                          IC95.high = c("",round(t.test(df2[[1]][,quo_name(x)])$conf.int[2],digits = decimals),
                                           round(t.test(df2[[2]][,quo_name(x)])$conf.int[2],digits = decimals)
                                        )
                        )
  names(result.df)[8] <- paste("IC",conf.level*100,".low", sep = "")
  names(result.df)[9] <- paste("IC",conf.level*100,".high", sep = "")
  row.names(result.df) <- NULL
  if (!is.normal) {
    result.df$means <- NULL
    names(result.df)[names(result.df) == "t"] <- "W"
  }
  return(result.df)

}
