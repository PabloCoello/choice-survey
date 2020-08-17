if(!require(idefix)){install.packages("idefix");library(idefix)}
if(!require(parallel)){install.packages("parallel");library(parallel)}
if(!require(doSNOW)){install.packages("doSNOW");library(doSNOW)}
if(!require(rjson)){install.packages("rjson");library(rjson)}

cores <- detectCores(all.tests = FALSE, logical = TRUE)
cluster_name <- makeCluster(cores, type = "SOCK")

setwd(system("pwd", intern = T) )
conf <- fromJSON(file='./conf.json')

load(paste('./Designs/', conf['design_name'], '.RData', sep='')


SurveyApp (
  des = design[['D']]$xdes,
  n.total = conf[['n.sets_survey']],
  alts = conf[['alternatives']],
  atts = conf[['attributes']],
  lvl.names = design[['labels']],
  coding = conf[['code']],
  buttons.text = conf[["survey_conf"]]['b.text'],
  intro.text = conf[["survey_conf"]]['i.text'],
  end.text = conf[["survey_conf"]]['e.text'],
  prior.mean = conf[['mu']],
  prior.covar = design[['sigma']],
  cand.set = design[['cs']],
  n.draws = 50,
  data.dir = conf[['path_to_storage']]
)