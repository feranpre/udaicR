

c.means <- function(df,x,group,decimals = 2, alternative = "two.sided", debug = FALSE, decimales = 2, show.test.method = TRUE){
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  x <- rlang::sym(rlang::as_label(rlang::enquo(x)))
  group <- rlang::sym(rlang::as_label(rlang::enquo(group)))

  df <- df[!is.na(df[,quo_name(group)]),]

  # names(df) <- c("x","group")
  c.group <- class(df[,quo_name(group)])

  if (class(c.group) != "factor"){
    if(debug) print("Group is not a factor. Trying conversion.")
    df[,quo_name(group)] <- as.factor(df[,quo_name(group)])
  }

  if (length(levels(df[,quo_name(group)])) != 2){
    warning("Levels of group variable not equal to 2. Aborting")
    return(NULL)
  }

  exp1 <- rlang::expr(!! x ~ !! group)
  df2 <- df[,c(quo_name(x),quo_name(group))] %>% group_split(!! group)

  levene <- var.test(eval(exp1), data =df)
  if (levene$p.value < 10^((decimales+1)*-1)) levene$p.value <- paste("<",as.character(10^((decimales+1)*-1)),"")
  else levene$p.value <- round(levene$p.value, digits = decimales)

  varianzas.iguales = ifelse(levene$p.value<0.05,TRUE,FALSE)

  if (debug) print(paste("LEVENE's p:",levene))

  if ((alternative != "two.sided") & (alternative != "greater") & (alternative != "less")) {
    warning('alternative parameter was used but with incorrect option. Options available are: "two.sided","greater","less". Defaulting to "two.sided"')
    alternative = "two.sided"
  }

  temp <- t.test(formula = eval(exp1), data = df, alternative = alternative, var.equal = varianzas.iguales)

  temp.p <- temp$p.value
  if (temp.p < 10^((decimales+1)*-1)) temp.p <- paste("<",as.character(10^((decimales+1)*-1)),"")
  else temp.p <- round(temp.p, digits = decimales)


  result.df <- data.frame(groups = c("",levels(df[,quo_name(group)])),
                          mean.var = c(quo_name(x),"",""),
                          homocedasticity.p = c(ifelse(show.test.method,levene$method,""),"",levene$p.value),
                          t = c(ifelse(show.test.method,temp$method,""),"",round(temp$statistic,digits = decimales)),
                          p = c("","",temp.p),
                          means = c("",round(temp$estimate,digits = decimales)),
                          IC95.low = c("",round(t.test(df2[[1]][,quo_name(x)])$conf.int[1],digits = decimales),
                                          round(t.test(df2[[2]][,quo_name(x)])$conf.int[1],digits = decimales)
                                      ),
                          IC95.high = c("",round(t.test(df2[[1]][,quo_name(x)])$conf.int[2],digits = decimales),
                                           round(t.test(df2[[2]][,quo_name(x)])$conf.int[2],digits = decimales)
                                        )
                        )
  row.names(result.df) <- NULL
  return(result.df)

}
