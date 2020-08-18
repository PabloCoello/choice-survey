if(!require(idefix)){install.packages("idefix");library(idefix)}
if(!require(parallel)){install.packages("parallel");library(parallel)}
if(!require(doSNOW)){install.packages("doSNOW");library(doSNOW)}
if(!require(rjson)){install.packages("rjson");library(rjson)}

get_answer <- function(conf){
  n.alts <- conf[['design_conf']][['n.alts']]
  cat(paste("Choose alternative [1-", n.alts, "]: ", sep=''))
  answer <- readLines("stdin", n=1)
  toret <- c()
  for(i in 1:n.alts){
    if(i != answer){
      toret[i] <- 0
    }else{
      toret[i] <- 1
    }
  }
  return(toret)
}

set_rownames <- function(set, i){
    names <- c()
    for(n in 1:nrow(set$set)){
      names[n] <- paste('set', i, '.alt', n , sep='')
    }
    rownames(set$set) <- names
    return(set)
}

cores <- detectCores(all.tests = FALSE, logical = TRUE)
cluster_name <- makeCluster(cores, type = "SOCK")

setwd(system("pwd", intern = T) )
conf <- fromJSON(file='./conf.json')
design <- readRDS(paste('./Designs/', conf[['design_conf']][['design_name']], '.rds', sep=''))


i <- 0
des <- design[['D']]$design
resp<-c()
while(i<conf[['survey_conf']][['n.sets_survey']]){
  i <- i + 1
  if(i <= conf[["design_conf"]][['n.sets_design']]){
    print(des[grep(paste('set', i, '.alt', sep=''),rownames(des)),])
    resp <- c(resp, get_answer(conf))
  }else{
    draws <- ImpsampMNL(n.draws = 100, prior.mean = conf[["design_conf"]][['mu']], prior.covar =design[['sigma']],
                    des = des, n.alts = conf[['design_conf']][['n.alts']], y = resp)
    set <- SeqCEA(des=des, lvls=design[['lvls']], coding = conf[["design_conf"]][['att_code']],n.alts = conf[['design_conf']][['n.alts']],par.draws = draws$sample,prior.covar = design[['sigma']],weights= draws$weights, parallel = TRUE, reduce=TRUE)
    set <- set_rownames(set, i)

    print(set)
    resp <- c(resp, get_answer(conf))
    des <- rbind(des, set$set)
  }
}

result <- cbind(des, resp)
write.table(
  result,
  paste(conf[['path_to_storage']], 
        str(as.numeric(Sys.time())),
        '_num_data.txt'),
  append = FALSE,
  sep = " ",
  dec = ".",
  row.names = TRUE,
  col.names = TRUE
)

stopCluster(cluster_name)