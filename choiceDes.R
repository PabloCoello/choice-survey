if(!require(choiceDes)){install.packages("choiceDes");library(choiceDes)}
setwd("~/GitHub/choice-survey")


levs1 <- c(3,3,3)
nb = 1 # Número de bloques
sets = 10 # Numero de choice sets
alts = 3 # Numero de alternativas en cada choice set


des <- dcm.design(levs1, nb, sets, alts,fname='res.txt', 20)
des
eff <- dcm.design.effcy(des$effects$design)
eff
