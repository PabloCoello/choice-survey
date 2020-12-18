suppressMessages(library(idefix))
suppressMessages(library(rjson))
suppressMessages(library(mlogit))

set_nochoice <- function(conf){
  #' Set n.alts acording to no.choice option.

  if (conf[['design_conf']][['no.choice']]) {
    conf[['design_conf']][['n.alts']] <- conf[['design_conf']][['n.alts']] + 1
  }
  return(conf)
}


gen_formula <- function(data) {
  #' Builds as.formula object for estimation.
  
  string <- c('Choice ~ ')
  for (name in names(data)) {
    if (grepl('Var', name)) {
      string <- paste(string, name, sep = ' + ')
    }
    if (grepl('.cte', name)) {
      string <- paste(string, name, sep = ' + ')
    }
  }
  formula <- as.formula(string)
  return(formula)
}


format_data <- function(data, conf){
  #' Format data for Rchoice package.
  
  des <- as.matrix(data[, 3:(ncol(data) - 1)])
  y <- data[, ncol(data)]
  
  data <-
    Datatrans(
      pkg = "mlogit",
      des = des,
      y = y,
      n.alts = conf[["design_conf"]][['n.alts']],
      n.sets = conf[["survey_conf"]][['n.sets_survey']],
      n.resp = max(data['ID']),
      #no.choice = conf[["design_conf"]][['no.choice']],
      bin = TRUE
    )
  return(data)
}


# Set conf and environment:
setwd(system("pwd", intern = T))
conf <- fromJSON(file = './conf/conf.json')
conf <- set_nochoice(conf)
data <- LoadData(data.dir = conf[["path_to_storage"]], type = "num")

# Get data:
formula <- gen_formula(data)
data <- format_data(data, conf)

# Perform estimation:
est <-
  mlogit(formula,
          data = data,
          method = "nr",
          rpar = "n")

# Show results:
summary(est)



