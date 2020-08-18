suppressMessages(library(idefix))
suppressMessages(library(parallel))
suppressMessages(library(doSNOW))
suppressMessages(library(rjson))
suppressMessages(library(utf8))


get_answer <- function(conf) {
  n.alts <- conf[['design_conf']][['n.alts']]
  cat(paste("Choose alternative [1-", n.alts, "]: ", sep = ''))
  answer <- readLines("stdin", n = 1)
  toret <- c()
  for (i in 1:n.alts) {
    if (i != answer) {
      toret[i] <- 0
    } else{
      toret[i] <- 1
    }
  }
  return(toret)
}


set_rownames <- function(set, i) {
  names <- c()
  for (n in 1:nrow(set$set)) {
    names[n] <- paste('set', i, '.alt', n , sep = '')
  }
  rownames(set$set) <- names
  return(set)
}


format_set_print <- function(des, i, design, conf) {
  set <- des[grep(paste('set', i, '.alt', sep = ''), rownames(des)), ]
  dec <-
    Decode(
      des = set,
      n.alts = conf[['design_conf']][['n.alts']],
      lvl.names = design[['labels']],
      coding = conf[["design_conf"]][['att_code']]
    )
  design <- t(dec$design)
  rownames(design) <- conf[['design_conf']][['attributes']]
  colnames(design) <- conf[['design_conf']][['alternatives']]
  utf8_print(knitr::kable(design, 'simple', align = "lccrr", escape=TRUE), char=100)
}


cores <- detectCores(all.tests = FALSE, logical = TRUE)
cluster_name <- makeCluster(cores, type = "SOCK")

setwd(system("pwd", intern = T))
conf <- fromJSON(file = './conf.json')
design <- readRDS(paste('./Designs/',
                        conf[['design_conf']][['design_name']],
                        '.rds',
                        sep = ''))


i <- 0
des <- design[['D']]$design
resp <- c()
while (i < conf[['survey_conf']][['n.sets_survey']]) {
  i <- i + 1
  if (i <= conf[["design_conf"]][['n.sets_design']]) {
    format_set_print(des, i, design, conf)
    resp <- c(resp, get_answer(conf))
  } else{
    draws <-
      ImpsampMNL(
        n.draws = 100,
        prior.mean = conf[["design_conf"]][['mu']],
        prior.covar = design[['sigma']],
        des = des,
        n.alts = conf[['design_conf']][['n.alts']],
        y = resp
      )
    set <-
      SeqCEA(
        des = des,
        lvls = design[['lvls']],
        coding = conf[["design_conf"]][['att_code']],
        n.alts = conf[['design_conf']][['n.alts']],
        par.draws = draws$sample,
        prior.covar = design[['sigma']],
        weights = draws$weights,
        parallel = TRUE,
        reduce = TRUE
      )
    set <- set_rownames(set, i)
    des <- rbind(des, set$set)
    format_set_print(des, i, design, conf)
    
    resp <- c(resp, get_answer(conf))
  }
}

result <- cbind(des, resp)

setwd(conf[['path_to_storage']])
write.table(
  as.data.frame(result),
  sprintf("%s_num_data.txt", as.integer(Sys.time())),
  append = FALSE,
  sep = "\t",
  quote = FALSE,
  row.names = TRUE,
  col.names = NA
)

stopCluster(cluster_name)