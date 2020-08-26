suppressMessages(library(idefix))
suppressMessages(library(rjson))
suppressMessages(library(Rchoice))


gen_formula <- function(data) {
  #' Builds as.formula object for estimation.
  
  string <- c('Choice ~ ')
  for (name in names(data)) {
    if (grepl('Var', name)) {
      string <- paste(string, name, sep = ' + ')
    }
  }
  formula <- as.formula(string)
  return(formula)
}


format_data <- function(data){
  #' Format data for Rchoice package.
  
  des <- as.matrix(data[, 3:(ncol(data) - 1)])
  y <- data[, ncol(data)]
  
  data <-
    Datatrans(
      pkg = "Rchoice",
      des = des,
      y = y,
      n.alts = conf[["design_conf"]][['n.alts']],
      n.sets = conf[["survey_conf"]][['n.sets_survey']],
      n.resp = max(data['ID']),
      bin = TRUE
    )
  return(data)
}


# Set conf and environment:
setwd(system("pwd", intern = T))
conf <- fromJSON(file = './conf/conf.json')
data <- LoadData(data.dir = conf[["path_to_storage"]], type = "num")

# Get data:
formula <- gen_formula(data)
data <- format_data(data)

# Perform estimation:
est <-
  Rchoice(formula,
          data = data,
          family = binomial(conf[['analysis_conf']][['model']]))

# Show results:
summary(est)



