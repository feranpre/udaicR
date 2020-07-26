library(tidyselect)

media <- function(df, ... , digits=2,
                  n=TRUE, missing=TRUE, min=TRUE, max=TRUE, mean=TRUE, sd=TRUE,
                  q1=TRUE, median=TRUE, q3=TRUE, iq_range=TRUE) {
   if (!missing("...")) {
     # if (length({{group_var}})>1) errorCondition("Only 1 group_var allowed")
     # else df <- df %>% group_by({{group_var}})
     df <- df %>% group_by(...)
   }



   result.df <- df %>%
                    group_by(...) %>%
                        select(where(is.numeric)) %>%
                          # var_summary(.data, digits = digits)
                          summarise(
                                      across(names(.data),
                                           ~ mean(.,na.rm=TRUE)
                                          )
                                    )

   return(result.df)
}

var_summary <- function(df, digits){
  vars <- names(df)
  suppressWarnings(
    result <- as.data.frame(
                          df %>%
                            dplyr::summarise(
                              across(vars,
                                n   = n(),
                                min = ifelse(min({{var}}, na.rm = TRUE) == Inf, NA, round(min({{var}}, na.rm = TRUE), digits=digits)),
                                max = ifelse(max({{var}}, na.rm = TRUE) == -Inf,NA, round(max({{var}}, na.rm = TRUE), digits=digits)),
                                mean = ifelse(is.na(mean({{var}}, na.rm = TRUE)), NA, round(mean({{var}}, na.rm = TRUE), digits=digits)),
                                sd = ifelse(is.na(sd({{var}}, na.rm = TRUE)), NA, round(sd({{var}}, na.rm = TRUE), digits=digits)),
                                median = ifelse(is.na(median({{var}}, na.rm = TRUE)), NA, median({{var}}, na.rm = TRUE)),
                                q1 = quantile({{var}},0.25, na.rm = TRUE),
                                q3 = quantile({{var}},0.75, na.rm = TRUE),
                                iq_range = quantile({{var}},0.75, na.rm = TRUE) - quantile({{var}},0.25, na.rm = TRUE)
                                ),
                              .groups = "keep"
                            )
      )
    )
}
