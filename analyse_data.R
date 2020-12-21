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

idx_format_data <- function(data, conf){
  data$resp <- as.logical(data$resp)
  names(data)[1]<- 'id'

  nalt <- conf[['design_conf']][['n.alts']]
  nvar <- length(conf[['design_conf']][['mu']])
  variables <- names(data)[4:(4+nvar-1)]

  toret <- data.frame()
  alt <- 0
  row <- 1
  resp <- c()
  for(i in 1:nrow(data)){
    alt <- alt + 1
    for(variable in variables){
      toret[row, paste(variable, alt, sep='_')] <- data[i, variable]
    }
    resp <- c(resp, data[i, 'resp'])
    if(alt == nalt){
      alt <- 0
      toret[row, 'Choice'] <- which(resp)
      toret[row, 'id'] <- data[i, 'id']
      row <- row + 1
      resp <- c()
    }
  }
  
  toret$chid <- 1:nrow(toret)

  toret <- dfidx(
    toret,
    idx = list(c("chid", "id")),
    choice = "Choice",
    varying = 1:24,
    sep = "_"
  )
  return(toret)
}


# Set conf and environment:
setwd(system("pwd", intern = T))
conf <- fromJSON(file = './conf/conf.json')
conf <- set_nochoice(conf)
data <- LoadData(data.dir = conf[["path_to_storage"]], type = "num")
data <- idx_format_data(data, conf)
# Get data:
formula <- gen_formula(data)
data <- format_data(data, conf)

# Perform estimation:
est <-
  mlogit(
    Choice ~ alt4.cte + Var1 + Var2 + Var32 + Var33 + Var4 | 0,
    toret,
    rpar = c(
      Var1 = 'n',
      Var2 = 'n',
      Var32 = 'u',
      Var33 = 'u',
      Var4 = 'n'
    ),
    R = 100,
    halton = NA,
    panel = TRUE
  )
summary(choi.mxl)



xchoi.up<- update(choi.mxl, correlation = TRUE)
summary(choi.up)

cor.mlogit(choi.up)
lrtest(choi.mxl, choi.up)
waldtest(choi.up, correlation = FALSE)
scoretest(choi.mxl, correlation = TRUE)