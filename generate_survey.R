if(!require(idefix)){install.packages("idefix");library(idefix)}
if(!require(bayesm)){install.packages("bayesm");library(bayesm)}
if(!require(mlogit)){install.packages("mlogit");library(mlogit)}

dataDir <-
  "C:/Users/pablo/OneDrive - Universidade de Santiago de Compostela/surveys"
setwd(dataDir)
load("adaptive_design_12.RData")


SurveyApp (
  des = xdes,
  n.total = n.sets,
  alts = alternatives,
  atts = attributes,
  lvl.names = labels,
  coding = code,
  buttons.text = b.text,
  intro.text = i.text,
  end.text = e.text,
  prior.mean = mu,
  prior.covar = sigma,
  cand.set = cs,
  n.draws = 100,
  data.dir = dataDir
)