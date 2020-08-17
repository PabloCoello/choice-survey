if(!require(idefix)){install.packages("idefix");library(idefix)}
if(!require(bayesm)){install.packages("bayesm");library(bayesm)}
if(!require(mlogit)){install.packages("mlogit");library(mlogit)}
if(!require(rsconnect)){install.packages("rsconnect");library(rsconnect)}
if(!require(parallel)){install.packages("parallel");library(parallel)}
if(!require(doSNOW)){install.packages("doSNOW");library(doSNOW)}
if(!require(rjson)){install.packages("rjson");library(rjson)}

cores <- detectCores(all.tests = FALSE, logical = TRUE)
cluster_name <- makeCluster(cores, type = "SOCK")
memory.limit(9999999999)
#rsconnect::setAccountInfo(name='ecoiuris',
#                          token='45F8C61BCA58B3EC9C85948E570C423B',
#                          secret='6nEgZw5xO1FwGLJXlB5M719M6ZeC8eK7RWmGR0yc')
#rsconnect::deployApp('path/to/your/app')
#Parameters

gen_lvls <- function(conf){
  lvls <- c()
  for(i in 1:length(conf[["attributes"]])){
    lvls <- c(lvls, length(conf[[paste('labels',i,sep='')]]))
  } 
  return(lvls)
}

gen_sigma <- function(conf){
  if(is.null(conf[["sigma"]])){
    sigma <- diag(length(conf[['mu']]))
  }else{
    sigma <- conf[['sigma']]
  }
  return(sigma)
}

generate_design <- function(conf){
  design[['lvls']] <- gen_lvls(conf)
  design[['sigma']] <- gen_sigma(conf)

  design[['cs']] <- Profiles(lvls = design[['lvls']], coding = conf[['code']])
  design[['M']] <- MASS::mvrnorm(n = 500, mu = conf[['mu']], Sigma = design[['sigma']])
  design[['D']] <- Modfed(
    cand.set = design[['cs']],
    n.sets = conf[['n.sets_design']],
    n.alts = conf[['n.alts']],
    alt.cte = conf[['alt.cte']],
    par.draws = design[['M']]
  )
  print(paste("Error of the model: ", D$error))
  return(design)
}


setwd(system("pwd", intern = T) )
conf <- fromJSON(file='./conf.json')
design <- generate_design(conf)
save(design, 'design_name.RData')


#DD <- Decode(des=xdes,n.alts = n.alts, lvl.names = labels, coding = code)
#DD


# Adaptive design
truepref <- c(0.3, 0.4, 0.3, 0.5, 0.3, 0.6, 0.7, 0.8, -0.2, -0.3, -0.4, -0.5)

y.sim <- RespondMNL(par = truepref, des = xdes, n.alts=n.alts)
y.sim

draws <- ImpsampMNL(n.draws = 100, prior.mean = mu, prior.covar = sigma,
                    des = xdes, n.alts = n.alts, y = y.sim)
draws


set <- SeqMOD(des=xdes, cand.set = cs, n.alts = n.alts,
              par.draws = draws$sample, prior.covar = sigma,
              weights= draws$weights, parallel = TRUE, reduce=TRUE)

set <- SeqCEA(des=xdes, lvls=c(3,3,5,5), coding = code, n.alts = n.alts,
              par.draws = draws$sample, prior.covar = sigma,
              weights= draws$weights, parallel = TRUE, reduce=TRUE)
set
# Survey

xdes

conf[["survey_conf"]]['b.text']
SurveyApp (
  des = xdes,
  n.total = conf[['n.sets_survey']],
  alts = conf[['alternatives']],
  atts = conf[['attributes']],
  lvl.names = labels,
  coding = conf[['code']],
  buttons.text = conf[["survey_conf"]]['b.text'],
  intro.text = conf[["survey_conf"]]['i.text'],
  end.text = conf[["survey_conf"]]['e.text'],
  prior.mean = conf[['mu']],
  prior.covar = sigma,
  cand.set = cs,
  n.draws = 50,
  data.dir = dataDir
)


# Data analysis

data <- LoadData(data.dir = dataDir, type = "num")
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
