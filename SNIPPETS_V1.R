rm(list = ls())

data_ <- data.frame(AGE=sample(x = 65:100, size=30, replace = TRUE ),
                    HEIGHT=sample(x = 120:205, size=30, replace = TRUE ),
                    SEX=sample(x = c("Male", "Female"), prob = c(.5,.5), size = 30, replace = TRUE),
                    BLOND=sample(x = c("Yes", "No"), prob = c(.2,.8), size = 30, replace = TRUE)
)
data_ <- rbind(data_, list(34,NA,NA,NA))
data_ <- rbind(data_, list(33,NA,"Male",NA))
data_ <- rbind(data_, list(22,NA,NA,"No"))
data_$EMPTY <- rep(NA,nrow(data_))

#' # PUNTO DE CORTE DE UNA VARIABLE
#'
#+ , echo = TRUE, message = FALSE, warning = FALSE, eval =TRUE, width = 25, height = 25, unit = "cm"

data_.temp <- data_[!is.na(data_$AGE) & !is.na(data_$BLOND), ]
(roc <- Epi::ROC(test=data_.temp$AGE, stat=as.numeric(data_.temp$BLOND=="Yes"), MX=TRUE))
opt <- names(which.max(rowSums(roc$res[, c("sens", "spec")])))
print(paste("Punto de corte optimo para AGE en funcion de BLOND:",opt))
