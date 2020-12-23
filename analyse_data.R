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

#df <- data$Var4
#df <- data[which(data$resp == 1), 'Var4']
#unique(df)
#for(val in unique(df)){
#  print(paste('valor:', val, 'reps:', sum(df==val), sep= ' '))
#}



names(data)[4:9]<-c("noChoice", "econVulnerability", "socialVulnerability", 'envVulnerability2',
                    'envVulnerability3', 'coste')

data <- idx_format_data(data, conf)

data$econCoste <- data$econVulnerability*data$coste
data$socialCoste <- data$socialVulnerability*data$coste
data$env2Coste <- data$envVulnerability2*data$coste
data$env3Coste <- data$envVulnerability3*data$coste
data$noChoiceCoste <- data$noChoice*data$coste



# Perform estimation:
# https://www.rdocumentation.org/packages/mlogit/versions/1.1-1/topics/mlogit
# Mixed Logit:
# https://cran.r-project.org/web/packages/mlogit/vignettes/c5.mxl.html
# https://cran.r-project.org/web/packages/mlogit/vignettes/e3mxlogit.html
# Multinomial Probit:
# https://cran.r-project.org/web/packages/mlogit/vignettes/e4mprobit.html
#

est <-
  mlogit(
    Choice ~ noChoice + econVulnerability + socialVulnerability + envVulnerability2 + envVulnerability3 + coste + econCoste | -1,
    data,
    rpar = c(
      econVulnerability = 'n',
      socialVulnerability = 'n',
      envVulnerability2 = 'n',
      envVulnerability3 = 'n',
      coste = 'n'
    ),
    R = 100,
    halton = NA,
    panel = TRUE
  )
summary(est)

est.up<- update(est, correlation = TRUE)
summary(est.up)

cor.mlogit(est.up)
lrtest(est, est.up)
waldtest(est.up, correlation = FALSE)
scoretest(est, correlation = TRUE)
