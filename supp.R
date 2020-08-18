
SurveyApp (
  des = NULL,
#  des = design[['D']]$design,
  n.total = conf[['survey_conf']][['n.sets_survey']],
  alts = conf[["design_conf"]][['alternatives']],
  atts = conf[["design_conf"]][['attributes']],
  lvl.names = design[['labels']],
  coding = conf[["design_conf"]][['att_code']],
  buttons.text = conf[["survey_conf"]][['b.text']],
  intro.text = conf[["survey_conf"]][['i.text']],
  end.text = conf[["survey_conf"]][['e.text']],
  prior.mean = conf[["design_conf"]][['mu']],
  prior.covar = design[['sigma']],
#  cand.set = design[['cs']],
  n.draws = 50,
#  data.dir = conf[['path_to_storage']],
  parallel = TRUE, 
  reduce = TRUE)


#SurveyApp(des = NULL, n.total = n.sets, alts = alternatives,
#          atts = attributes, lvl.names = labels, coding = code, 
#          buttons.text = b.text, intro.text = i.text, end.text = e.text, 
#          prior.mean = p.mean, prior.covar = p.var, cand.set = cand, 
#          lower = low, upper = up, n.draws = 50)
# If CEA algorithm is desired, cand.set argument is not needed
#SurveyApp(des = NULL, n.total = n.sets, alts = alternatives,
#         atts = attributes, lvl.names = labels, coding = code, 
#         buttons.text = b.text, intro.text = i.text, end.text = e.text, 
#         prior.mean = p.mean, prior.covar = p.var, 
#         lower = low, upper = up, n.draws = 50)



truepref <- c(0.3, 0.4, 0.3, 0.5, 0.3, 0.6, 0.7, 0.8, -0.2, -0.3, -0.4, -0.5)

y.sim <- RespondMNL(par = truepref, des = design[['D']]$design, n.alts=conf[['design_conf']][['n.alts']])


draws <- ImpsampMNL(n.draws = 100, prior.mean = conf[["design_conf"]][['mu']], prior.covar =design[['sigma']],
                    des = design[['D']]$design, n.alts = conf[['design_conf']][['n.alts']], y = y.sim)


#set <- SeqMOD(des=design[['D']]$design, cand.set = design[['cs']], n.alts = conf[['design_conf']][['n.alts']],
#              par.draws = draws$sample, prior.covar = design[['sigma']],
#              weights= draws$weights, parallel = TRUE, reduce=TRUE)



#set <- SeqCEA(des=design[['D']]$design, lvls=design[['lvls']], coding = conf[["design_conf"]][['att_code']],n.alts = conf[['design_conf']][['n.alts']],par.draws = draws$sample,prior.covar = design[['sigma']],weights= draws$weights, parallel = TRUE, reduce=TRUE)


#set

install.packages("googleVis")
library(googleVis)
plot(gvisTable(data.frame(set)))
summary(table_one, title = "Gapminder Data")

library(knitr)
knitr::kable(set)