suppressMessages(library(readxl))
suppressMessages(library(rjson))
suppressMessages(library(stringr))

format_multi_df <- function(df){
  criterios <- df[,grep(c('¿Qué criterio'), colnames(df))]
  grados <- df[,grep(c('Grado de importancia'), colnames(df))]
  
  for(col in 1:ncol(criterios)){
    colnames(criterios)[col] <- substr(colnames(criterios)[col], start=1, stop = 1)
    colnames(grados)[col] <- substr(colnames(grados)[col], start=1, stop = 1)
  }
  
  for(col in 1:length(colnames(criterios))){
    colnames(criterios)[col] <- multi_conf[['pares']][as.character(col)]
    colnames(grados)[col] <- multi_conf[['pares']][as.character(col)]
  }
  
  criterios.na <- complete.cases(criterios)
  criterios <- criterios[criterios.na,]
  grados <- grados[criterios.na,]
  
  grados.na <- complete.cases(grados)
  criterios <- criterios[grados.na,]
  grados <- grados[grados.na,]
  return(list(criterios, grados))
}

get_att <- function(list){
  att <- unique(as.vector(as.matrix(list[[1]])))
  att <- att[!grepl('Igual de importantes', att)]
  return(att)
}

extract_weights <- function(list){
  return()
}


get_matrix <- function(list, att, row){

  matrix <- matrix(nrow = length(att), ncol = length(att))
  rownames(matrix) <- att
  colnames(matrix) <- att
  
  criteria_names <- colnames(list[[1]])
  criterios <- list[[1]]
  grados <- list[[2]]
  
  for(i in att){
    for(j in att){
      if(i==j){
        matrix[i,j] <- 1
      }else{
        rule <- as.character(criterios[row,criteria_names[grepl(i,criteria_names) & grepl(j,criteria_names)]])
        grad <- as.numeric(grados[row,criteria_names[grepl(i,criteria_names) & grepl(j,criteria_names)]])
        if(rule == i){
          matrix[i,j] <- grad
          matrix[j,i] <- 1/grad
        }
        if(rule == j){
          matrix[i,j] <- 1/grad
          matrix[j,i] <- grad
        }
      }
    }
  }
  return(matrix)
}


setwd(system("pwd", intern = T))
conf <- fromJSON(file = './conf/conf.json')
multi_conf <- fromJSON(file = './conf/multicriteria_conf.json')

df <- read_excel(multi_conf[['path_to_file']])
list <- format_multi_df(df)
att <- get_att(list)


for(respondent in 1:nrow(list[[1]])){
  matrix <- get_matrix(list, att, row=respondent)
}
