suppressMessages(library(idefix))
suppressMessages(library(rjson))


set_nochoice <- function(conf) {
  #' Set n.alts acording to no.choice option.
  
  if (conf[['design_conf']][['no.choice']]) {
    conf[['design_conf']][['n.alts']] <-
      conf[['design_conf']][['n.alts']] + 1
  }
  return(conf)
}

setwd(system("pwd", intern = T))
conf <- fromJSON(file = './conf/conf.json')

# Read design:
design <- readRDS(paste('./Designs/',
                        conf[['design_conf']][['design_name']],
                        '.rds',
                        sep = ''))
conf <- set_nochoice(conf)

dec <- Decode(
  des = design[['D']]$design,
  n.alts = conf[['design_conf']][['n.alts']],
  lvl.names = design[['labels']],
  coding = conf[["design_conf"]][['att_code']],
  alt.cte = conf[['design_conf']][['alt.cte']],
  c.lvls = conf[["design_conf"]][['con.lvls']],
  no.choice = conf[['design_conf']][['n.alts']]
)

cat(dec$design)
cat(design$D$error)
cat(design$D$inf.error)
