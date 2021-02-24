correlationS4 <- function(data,..., decimals = 2,
                        show.errors = TRUE, show.warnings = TRUE, DEBUG = FALSE,
                        show.main.matrix = TRUE, show.cor.matrix = FALSE, show.p.matrix = FALSE, show.method.matrix = FALSE,
                        method = "auto", show.theory = FALSE, show.interpretation = FALSE, lang = "en"){

  lang <- arg_match(lang, values = c("en","es"))
  method <- arg_match(method, values = c("auto", "sw", "lillie", "ks"))

  new("udaicR.correlation")

}
