rm(list = ls())





# library(udaicR)
data_ <- data.frame(AGE=sample(x = 65:100, size=30, replace = TRUE ),
                    HEIGHT=sample(x = 120:205, size=30, replace = TRUE ),
                    SEX=sample(x = c("Male", "Female"), prob = c(.5,.5), size = 30, replace = TRUE),
                    BLOND=sample(x = c("Yes", "No"), prob = c(.2,.8), size = 30, replace = TRUE),
                    HEALTH=sample(x = c("Bad", "Normal", "Excelent"), prob = c(0.2,.6,.2), size = 30, replace = TRUE)
)
data_ <- rbind(data_, list(34,NA,NA,NA,NA))
data_ <- rbind(data_, list(33,NA,"Male",NA,NA))
data_ <- rbind(data_, list(22,NA,NA,"No",NA))
data_ <- rbind(data_, list(NA,NA,NA,"No","Bad"))
data_$EMPTY <- rep(NA,nrow(data_))
data_$HEALTH <- as.factor(data_$HEALTH)


udaicR::comp.mean(data_, c("HEIGHT"), by= "HEALTH", show.desc = T)
udaicR::media(data_$AGE, by = data_$SEX)
media(data_$AGE, by = data_$SEX)

load("DATOS.RData")

t.test(data_$AGE ~ data_$SEX, equal.var = FALSE)
t.test(data_$AGE ~ data_$SEX, equal.var = TRUE)
c <-car::leveneTest(data_$AGE, group= data_$SEX)
c$`Pr(>F)`



media(DATOS$IMC)
media(DATOS, variables = c("IMC","EDAD"), by="SEXO", DEBUG = T)
media(DATOS, variables = c("IMC","EDAD"), DEBUG = T)
#====================================================
#
#  ---- is.normal ----
#
#====================================================
is.normal(data_$AGE)
is.normal(12:92)

c <- is.normal(data_$AGE)

print(class(is.normal(data_$AGE)))
if (is.normal(data_$HEIGHT)) {
  print("T")
} else {
  print("F")
  }

norm.test(data_$AGE, show.interpretation = TRUE, lang = "es")
norm.test(data_$AGE, show.interpretation = TRUE, lang = "es", method="lillie")
norm.test(data_$HEIGHT, show.interpretation = TRUE, lang = "es")

norm.test(data_$AGE, show.interpretation = TRUE)
norm.test(data_$HEIGHT, show.interpretation = TRUE)

norm.test(data_$AGE, show.theory = TRUE, lang = "es")
n <- norm.test(40:70)


#====================================================
#
#  ---- MEDIA ----
#
#====================================================

summarise_if(data_, is.numeric, list(~ mean(.x,na.rm=T),
                                     ~ sd(.x,na.rm=T),
                                     ~ median(.x,na.rm=T),
                                     ~ IQR(.x,na.rm=T)
                                     ),
             )

data_ %>% select(AGE)
psych::describe(data_ %>% select(AGE))

data_ %>% select(AGE) %>% summarise(mean = mean(.data[[1]],na.rm=T),
                                    sd =  sd(.x[[1]],na.rm=T),
                                    median =  median(.x[[1]],na.rm=T),
                                    IQR = IQR(.data[[1]],na.rm=T)

                                             )



data_ %>% select(AGE,SEX) %>%
  group_by(SEX) %>% summarize(
  n = n()-sum(is.na(data_[,1])),
  missing = sum(is.na(data_[,1])),
  min=min(data_[,1], na.rm = TRUE),
  max=max(data_[,1], na.rm = TRUE),
  mean=round(mean(data_[,1], na.rm = TRUE), digits = 2),
  sd=round(sd(data_[,1], na.rm = TRUE),digits = 2),
  median=median(data_[,1], na.rm = TRUE),
  IQR=IQR(data_[,1], na.rm = TRUE),
  normal=round(shapiro.test(data_[,1])$p.value, digits = 3)
)


media(data_$AGE)
media(data_$AGE, data_$HEIGHT)
media(data_, AGE)
media(data_, "AGE")

media(data_, AGE, HEIGHT)
media(data_, "AGE", by="SEX")

media(data_, AGE,HEIGHT, by="SEX")
media(data_, AGE,HEIGHT, by=SEX)
media(data_, AGE,HEIGHT, by=SEX)


media(AGE, data = data_, mean=F)
media(AGE, data = data_, mean=F, by="SEX")
media("DATOS",data = data_)



#====================================================
#
#  ---- FREQUENCIES ----
#
#====================================================


# -- regular freq
udaicR::freq(data_,SEX)


# -- freq sorted by Value
freq(data_,SEX, sort_by_values = TRUE, sort_decreasing = FALSE)

# -- multiple variables
udaicR::freq(data_, SEX, BLOND)

# -- one variable grouped by another
freq(data_, SEX, group_by_col = BLOND)

# -- one variable grouped by another, without groups totals
freq(data_, SEX, group_by_col = BLOND, total_by_group = FALSE)


#====================================================
#
#  ---- MEANS ----
#
#====================================================

# ---  basic mean
means(data_,HEIGHT)
means(data_,HEIGHT, show_warnings = FALSE)
media(data_, HEIGHT, group_var = SEX)
media(data_, SEX)


mtcars %>% group_by(cyl) %>% summarise(where(is.numeric))

# ---  multiple means
means(data_, AGE, HEIGHT, range=FALSE)

# --- means with grouping
means(data_, AGE, HEIGHT, group_by_col = SEX)

# --- esto funciona también con media
media(data_, HEIGHT)


means(data_, EMPTY, group_by_col = SEX)

#========================================================
#
#      ---- c.table -----
#
#=========================================================
c_table(data_,SEX,BLOND, debug=TRUE)
knitr::kable(c_table(data_,SEX,BLOND))



#========================================================
#
#      ---- c.means -----
#
#=========================================================
c_means(data_,HEIGHT,BLOND)




#========================================================
#
#      ---- correlation -----
#
#=========================================================
correlation(data_$AGE, data_$HEIGHT, lang="es")
correlation(data_, AGE, HEIGHT)
correlation(data_, "AGE", "HEIGHT")
correlation(data_[,c("AGE", "HEIGHT")], show.warnings = F, method = "pearson")
udaicR::correlation(data_, show.warnings = T, method = "pearson")

correlation()

data("mtcars")
(c <- correlation(mtcars))
c
c$theory.text

