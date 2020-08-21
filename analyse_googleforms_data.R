library(readxl)
library(idefix)
library(rjson)
library(Rchoice)


get_design <- function(path){
  design <- read_excel(path)
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

setwd(system("pwd", intern = T))
conf <- fromJSON(file = './conf/conf.json')
forms_conf <- fromJSON(file = './conf/google_forms_conf.json')

des <- get_design(path=forms_conf[['path_to_design']])
df <- read_excel(forms_conf[['path_to_file']])
df <- format_df(df)
df <- encode_df(df)

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


formula <- as.formula(form_conf[['formula']])
est <- Rchoice(formula, data = rchoice_data, family = binomial(form_conf[['model']]))
summary(est)

###############################################
#Multicriteria Saaty analysis
###############################################

format_multi_df <- function(df){
  criterios <- df[,grep(c('¿Qué criterio'), colnames(df))]
  grados <- df[,grep(c('Grado de importancia'), colnames(df))]
  for(col in 1:ncol(criterios)){
    colnames(criterios)[col] <- substr(colnames(criterios)[col], start=1, stop = 1)
    colnames(grados)[col] <- substr(colnames(grados)[col], start=1, stop = 1)
    #if(is.na(as.numeric(colnames(df[,col])))){
    #  criterios[,col]<-NULL
    #  grados[,col]<-NULL
    #}
  }

  return(df)
}