library(readxl)

format_multi_df <- function(df){
  criterios <- df[,grep(c('¿Qué criterio'), colnames(df))]
  grados <- df[,grep(c('Grado de importancia'), colnames(df))]
  for(col in 1:ncol(criterios)){
    colnames(criterios)[col] <- substr(colnames(criterios)[col], start=1, stop = 1)
    colnames(grados)[col] <- substr(colnames(grados)[col], start=1, stop = 1)
  }
  
  criterios.na <- complete.cases(criterios)
  criterios <- criterios[criterios.na,]
  grados <- grados[criterios.na,]
  
  grados.na <- complete.cases(grados)
  criterios <- criterios[grados.na,]
  grados <- grados[grados.na,]
  return(list(criterios, grados))
}

extract_weights <- function(list){
  return()
}


setwd(system("pwd", intern = T))
conf <- fromJSON(file = './conf/conf.json')
multi_conf <- fromJSON(file = './conf/multicriteria_conf.json')


df <- read_excel(multi_conf[['path_to_file']])
list <- format_multi_df(df)
