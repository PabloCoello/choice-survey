suppressMessages(library(idefix))
suppressMessages(library(parallel))
suppressMessages(library(doSNOW))
suppressMessages(library(rjson))


gen_lvls <- function(conf) {
  #' Return array with number of levels per attribute.
  
  lvls <- c()
  for (i in 1:length(conf[["attributes"]])) {
    lvls <- c(lvls, length(conf[[paste('labels', i, sep = '')]]))
  }
  return(lvls)
}


gen_sigma <- function(conf) {
  #' Generates sigma if it is not pre especified.
  
  if (is.null(conf[["sigma"]])) {
    sigma <- diag(length(conf[['mu']]))
  } else{
    sigma <- conf[['sigma']]
  }
  return(sigma)
}


gen_labels <- function(conf) {
  #' Returns an array of labels.
  
  labels <- list()
  for (i in 1:length(conf[['attributes']])) {
    labels[[i]] <- conf[[paste('labels', i, sep = '')]]
  }
  return(labels)
}


generate_design <- function(conf) {
  #' Generates design by passign arguments to Modfed() function from
  #' Idefix package.
  
  design <- list()
  design[['lvls']] <- gen_lvls(conf)
  design[['sigma']] <- gen_sigma(conf)
  design[['labels']] <- gen_labels(conf)
  
  design[['cs']] <-
    Profiles(lvls = design[['lvls']], coding = conf[['att_code']])
  draws <-
    MASS::mvrnorm(n = 500, mu = conf[['mu']], Sigma = design[['sigma']])
  
  if(conf[['no.choice']]){
    design[['M']] <- list(draws[,1:sum(conf[['alt.cte']])], draws[,sum(conf[['alt.cte']])]:length(mu))
  }else{
    design[['M']] <- draws
  }
  

  design[['D']] <- Modfed(
    cand.set = design[['cs']],
    n.sets = conf[['n.sets_design']],
    n.alts = conf[['n.alts']],
    alt.cte = conf[['alt.cte']],
    par.draws = design[['M']],
    no.choice = conf[['no.choice']]
  )
  return(design)
}


# Set environment:
cores <- detectCores(all.tests = FALSE, logical = TRUE)
cluster_name <- makeCluster(cores, type = "SOCK")
setwd(system("pwd", intern = T))
conf <- fromJSON(file = './conf/conf.json')

# Generate design:
design <- generate_design(conf = conf[["design_conf"]])

# Save design data:
saveRDS(design, paste('./Designs/',
                      conf[["design_conf"]][['design_name']],
                      '.rds',
                      sep = ''))
stopCluster(cluster_name) # Stop multithreading setup