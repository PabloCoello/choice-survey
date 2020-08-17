if(!require(idefix)){install.packages("idefix");library(idefix)}
if(!require(parallel)){install.packages("parallel");library(parallel)}
if(!require(doSNOW)){install.packages("doSNOW");library(doSNOW)}
if(!require(rjson)){install.packages("rjson");library(rjson)}

cores <- detectCores(all.tests = FALSE, logical = TRUE)
cluster_name <- makeCluster(cores, type = "SOCK")

setwd(system("pwd", intern = T) )
conf <- fromJSON(file='./conf.json')

design <- readRDS(paste('./Designs/', conf[['design_conf']][['design_name']], '.rds', sep=''))

SurveyApp (
  des = design[['D']]$design,
  n.total = conf[['survey_conf']][['n.sets_survey']],
  alts = conf[["design_conf"]][['alternatives']],
  atts = conf[["design_conf"]][['attributes']],
  lvl.names = design[['labels']],
  coding = conf[["design_conf"]][['att_code']],
  buttons.text = conf[["survey_conf"]]['b.text'],
  intro.text = conf[["survey_conf"]]['i.text'],
  end.text = conf[["survey_conf"]]['e.text'],
  prior.mean = conf[["design_conf"]][['mu']],
  prior.covar = design[['sigma']],
  cand.set = design[['cs']],
  n.draws = 50,
  data.dir = conf[['path_to_storage']],
  parallel = TRUE, 
  reduce = TRUE
)