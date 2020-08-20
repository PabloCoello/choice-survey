library(readxl)
library(idefix)
library(rjson)
library(readODS)
library(Rchoice)

setwd("~/Escritorio/analisis encuestas google forms")


get_design <- function(path){
  design <- read_ods(path)
  rownames(design) <- design[,1]
  design[,1] <- NULL
  design <- as.matrix(design)
  return(design)
}

format_nochoice <- function(data){
  data <- data[!grepl('.alt4',rownames(data)),!colnames(data)=='no.choice.cte']
  return(data)
}

format_df <- function(df){
  df <- df[,grep('¿En qué alternativa preferiría aplicar', colnames(df))]
  for(col in 1:ncol(df)){
    colnames(df)[col] <- substr(colnames(df)[col], start=1, stop = 1)
    if(is.na(as.numeric(colnames(df[,col])))){
      df[,col]<-NULL
    }
  }
  return(df)
}

encode_df <- function(df){
  narows <- c()
  for(row in 1:nrow(df)){
    for(col in 1:ncol(df)){
      if(is.na(df[row,col])){
        df[row,col] = as.character(4)
        narows <- c(narows, row)
      }else{
        if(df[row,col]=='Alternativa A'){
          df[row,col] = as.character(1)
        }
        if(df[row,col]=='Alternativa B'){
          df[row,col] = as.character(2)
        }
        if(df[row,col]=='Alternativa C'){
          df[row,col] = as.character(3)
        }
        if(df[row,col]=='Ninguna de las anteriores'){
          df[row,col] = as.character(4)
        }
      }
    }
  }
  df <- df[-narows,]
  return(df)
}

get_answer <- function(n, n.alts) {
  answer <- n
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

des = get_design(path='design.ods')
df <- read_excel("Actuaciones post-incendio (respuestas).xlsx")
df <- format_df(df)
df <- encode_df(df)
df

for(respondent in 1:nrow(df)){
  resp <- c()
  for(question in 1:ncol(df)){
    resp <- c(resp, get_answer(df[respondent,question], n.alts=4))
  }
  ind <- array(respondent, dim=nrow(des))
  toret <- cbind(ind, des, resp)
  if(respondent==1){
    matrix <- toret
  }else{
    matrix <- rbind(matrix, toret)  
  }
}
data <- matrix

des <- as.matrix(data[, 2:(ncol(data) - 1)])
y <- data[, ncol(data)]

rchoice_data <-
  Datatrans(
    pkg = "Rchoice",
    des = des,
    y = y,
    n.alts = 4,
    n.sets = 8,
    n.resp = nrow(df),
    bin = TRUE
  )


formula <- as.formula('Choice ~ no.choice.cte + Var12 + Var13 + Var22 + Var23 + Var32 + Var33 + Var34 + Var35 + Var42 + Var43 + Var44 + Var45')
est <- Rchoice(formula, data = rchoice_data, family = binomial("logit"))
summary(est)
plot(est)
