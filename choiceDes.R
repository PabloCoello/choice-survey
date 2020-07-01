if(!require(choiceDes)){install.packages("choiceDes");library(choiceDes)}
levs1 <- c(3,3,3)
des <- dcm.design(levs1, 3, 3, 20)
des
