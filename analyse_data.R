if(!require(idefix)){install.packages("idefix");library(idefix)}
if(!require(mlogit)){install.packages("mlogit");library(mlogit)}
if(!require(rjson)){install.packages("rjson");library(rjson)}


gen_formula <- function(data) {
  string <- c('Choice ~ ')
  for (name in names(data)) {
    if (grepl('Var', name)) {
      string <- paste(string, name, sep = ' + ')
    }
  }
  formula <- as.formula(string)
  return(formula)
}

setwd(system("pwd", intern = T))
conf <- fromJSON(file = './conf.json')
data <- LoadData(data.dir = conf[["path_to_storage"]], type = "num")

des <- as.matrix(data[, 3:(ncol(data) - 1)])
y <- data[, ncol(data)]

logit_data <-
  Datatrans(
    pkg = "mlogit",
    des = des,
    y = y,
    n.alts = conf[["design_conf"]][['n.alts']],
    n.sets = conf[["survey_conf"]][['n.sets_survey']],
    n.resp = max(data['ID']),
    bin = TRUE
  )
formula <- gen_formula(data)
summary(mlogit(formula, data = logit_data))


