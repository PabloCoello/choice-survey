if(!require(idefix)){install.packages("idefix");library(idefix)}
if(!require(parallel)){install.packages("parallel");library(parallel)}
if(!require(doSNOW)){install.packages("doSNOW");library(doSNOW)}
if(!require(rjson)){install.packages("rjson");library(rjson)}


gen_lvls <- function(conf) {
  lvls <- c()
  for (i in 1:length(conf[["attributes"]])) {
    lvls <- c(lvls, length(conf[[paste('labels', i, sep = '')]]))
  }
  return(lvls)
}

gen_sigma <- function(conf) {
  if (is.null(conf[["sigma"]])) {
    sigma <- diag(length(conf[['mu']]))
  } else{
    sigma <- conf[['sigma']]
  }
  return(sigma)
}

gen_labels <- function(conf) {
  labels <- list()
  for (i in 1:length(conf[['attributes']])) {
    labels[[i]] <- conf[[paste('labels', i, sep = '')]]
  }
  return(labels)
}

generate_design <- function(conf) {
  design <- list()
  design[['lvls']] <- gen_lvls(conf)
  design[['sigma']] <- gen_sigma(conf)
  design[['labels']] <- gen_labels(conf)
  
  design[['cs']] <-
    Profiles(lvls = design[['lvls']], coding = conf[['att_code']])
  design[['M']] <-
    MASS::mvrnorm(n = 500, mu = conf[['mu']], Sigma = design[['sigma']])
  design[['D']] <- Modfed(
    cand.set = design[['cs']],
    n.sets = conf[['n.sets_design']],
    n.alts = conf[['n.alts']],
    alt.cte = conf[['alt.cte']],
    par.draws = design[['M']]
  )
  return(design)
}

cores <- detectCores(all.tests = FALSE, logical = TRUE)
cluster_name <- makeCluster(cores, type = "SOCK")

setwd(system("pwd", intern = T))
conf <- fromJSON(file = './conf.json')
design <- generate_design(conf = conf[["design_conf"]])
saveRDS(design, paste('./Designs/',
                      conf[["design_conf"]][['design_name']],
                      '.rds',
                      sep = ''))
stopCluster(cluster_name)