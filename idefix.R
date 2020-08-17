if(!require(idefix)){install.packages("idefix");library(idefix)}
if(!require(bayesm)){install.packages("bayesm");library(bayesm)}
if(!require(mlogit)){install.packages("mlogit");library(mlogit)}
if(!require(rsconnect)){install.packages("rsconnect");library(rsconnect)}
if(!require(parallel)){install.packages("parallel");library(parallel)}
if(!require(doSNOW)){install.packages("doSNOW");library(doSNOW)}


cores <- detectCores(all.tests = FALSE, logical = TRUE)
cluster_name <- makeCluster(cores, type = "SOCK")
#rsconnect::setAccountInfo(name='ecoiuris',
#                          token='45F8C61BCA58B3EC9C85948E570C423B',
#                          secret='6nEgZw5xO1FwGLJXlB5M719M6ZeC8eK7RWmGR0yc')
#rsconnect::deployApp('path/to/your/app')
#Parameters

dataDir <-
  "C:/Users/pablo/OneDrive - Universidade de Santiago de Compostela/surveys"
setwd(dataDir)
load(file="adaptive_design_12.RData")

code <- c("D", "D", "D", "D")
alternatives <- c("Alternativa A", "Alternativa B", "Alternativa C")
attributes <- c("Vulnerabilidad económica", 
                "Vulnerabilidad social",
                "Vulnerabilidad medioambiental", 
                "Coste del tratamiento")
n.sets <-12
n.alts <-3
alt.cte <- NULL

mu <- c(0.4, 1, 0.2, 0.4, 0.3, 0.4, 0.5, 0.7, -0.1, -0.2, -0.3, -0.4)
sigma <- diag(length(mu))

labels <- vector(mode = "list", length(attributes))
labels[[1]] <- c("0€/ha", "1.000€/ha", "2.000€/ha")
labels[[2]] <- c("Baja", "Media", "Alta")
labels[[3]] <- c("No significativa", "Baja", "Media", "Alta", "Muy alta")
labels[[4]] <- c("2.000€/ha", "4.000€/ha", "6.000€/ha", "8.000€/ha", "10.000€/ha")
b.text <- "Please choose the alternative you prefer"
i.text <- "Welcome, here are some instructions ... good luck!"
e.text <- "Thanks for taking the survey"

# Design

cs <- Profiles(lvls = c(3, 3, 5, 5), coding = code)

M <- MASS::mvrnorm(n = 500, mu = mu, Sigma = sigma)
D <- Modfed(
  cand.set = cs,
  n.sets = n.sets,
  n.alts = n.alts,
  alt.cte = alt.cte,
  par.draws = M
)
D$error
xdes <- D$design

DD <- Decode(des=xdes,n.alts = n.alts, lvl.names = labels, coding = code)
DD

# Adaptive design
truepref <- c(0.3, 0.4, 0.3, 0.5, 0.3, 0.6, 0.7, 0.8, -0.2, -0.3, -0.4, -0.5)

y.sim <- RespondMNL(par = truepref, des = xdes, n.alts=n.alts)
y.sim

draws <- ImpsampMNL(n.draws = 100, prior.mean = mu, prior.covar = sigma,
                    des = xdes, n.alts = n.alts, y = y.sim)
draws

memory.limit(9999999999)
set <- SeqMOD(des=xdes, cand.set = cs, n.alts = n.alts,
              par.draws = draws$sample, prior.covar = sigma,
              weights= draws$weights, parallel = TRUE, reduce=TRUE)

set <- SeqCEA(des=xdes, lvls=c(3,3,5,5), coding = code, n.alts = n.alts,
              par.draws = draws$sample, prior.covar = sigma,
              weights= draws$weights, parallel = TRUE, reduce=TRUE)
set
# Survey

xdes


SurveyApp (
  des = xdes,
  n.total = 16,
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
