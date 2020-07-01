if(!require(idefix)){install.packages("idefix");library(idefix)}
if(!require(bayesm)){install.packages("bayesm");library(bayesm)}
if(!require(mlogit)){install.packages("mlogit");library(mlogit)}

#Parameters
dataDir <-
  "C:/Users/pablo/OneDrive - Universidade de Santiago de Compostela/surveys"
setwd(dataDir)

code <- c("D", "D", "D")
alternatives <- c("Alt A", "Alt B", "Alt C")
attributes <- c("Economic", "Social", "Environmental")
n.sets <- 8
n.alts <-3
alt.cte <- c(0, 0, 0)

mu <- c(-0.4,-1,0.2,1, 0.4, 1)
sigma <- diag(length(mu))

labels <- vector(mode = "list", length(attributes))
labels[[1]] <- c("$10", "$5", "$1")
labels[[2]] <- c("Buena", "Normal", "Mala")
labels[[3]] <- c("Buena", "Normal", "Mala")
b.text <- "Please choose the alternative you prefer"
i.text <- "Welcome, here are some instructions ... good luck!"
e.text <- "Thanks for taking the survey"

# Design

cs <- Profiles(lvls = c(3, 3, 3), coding = code)

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


# Survey

xdes


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
