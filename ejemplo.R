rm(list = ls())

library(dplyr)
data_ <- data.frame(AGE=sample(x = 65:100, size=30, replace = TRUE ),
                     HEIGHT=sample(x = 120:205, size=30, replace = TRUE ),
                     SEX=sample(x = c("Male", "Female"), prob = c(.5,.5), size = 30, replace = TRUE),
                     BLOND=sample(x = c("Yes", "No"), prob = c(.2,.8), size = 30, replace = TRUE)
)
data_ <- rbind(data_, list(34,NA,NA,NA))
data_ <- rbind(data_, list(33,NA,"Male",NA))
data_ <- rbind(data_, list(22,NA,NA,"No"))
data_$EMPTY <- rep(NA,nrow(data_))



#====================================================
#
#  ---- FREQUENCIES ----
#
#====================================================


# -- regular freq
freq(data_, SEX)

# -- freq sorted by Value
freq(data_,SEX, sort_by_values = TRUE, sort_decreasing = FALSE)

# -- multiple variables
freq(data_, SEX, BLOND)

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

# ---  multiple means
means(data_, AGE, HEIGHT)

# --- means with grouping
means(data_, AGE, HEIGHT, group_by_col = SEX)

# --- esto funciona también con media
media(data_, HEIGHT)


(r <- tab(data_,SEX,BLOND,row_percent = F, show_totals = T))
kable(r)

means(data_, EMPTY, group_by_col = SEX)

#========================================================
#
#      ---- c.table -----
#
#=========================================================
c.table(data_,SEX,BLOND)
