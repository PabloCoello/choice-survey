if(!require(idefix)){install.packages("idefix");library(idefix)}
if(!require(mlogit)){install.packages("mlogit");library(mlogit)}
if(!require(rjson)){install.packages("rjson");library(rjson)}


setwd(system("pwd", intern = T) )
conf <- fromJSON(file='./conf.json')

data <- LoadData(data.dir = conf[["path_to_storage"]], type = "num")
des <- as.matrix(data[, 3:8], ncol = 6)
y <- data[, 9]

logit_data <-
  Datatrans(
    pkg = "mlogit",
    des = des,
    y = y,
    n.alts = n.alts,
    n.sets = n.sets,
    n.resp = 1,
    bin = TRUE
  )
summary(mlogit(Choice ~ Var12 + Var13 + Var22 + Var23 + Var32 + Var33, data =
                 logit_data))