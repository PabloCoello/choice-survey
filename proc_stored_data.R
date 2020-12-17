suppressMessages(library(idefix))
suppressMessages(library(rjson))
suppressMessages(library(Rchoice))
suppressMessages(library(readr))
suppressMessages(library(stringr))

proc_data <- function(filename){
  res <- read_csv(paste(filename,'.csv',sep=''))
  y <- fromJSON(file = paste(filename,'.json',sep=''))[['answers']]
  if(length(y)==nrow(res)){
    res['resp'] = y
    names(res)[1]='X'
    return(res)
  }
}

setwd(system("pwd", intern = T))
conf <- fromJSON(file = './conf/conf.json')
setwd(conf[['path_to_storage']])

files <- Sys.glob("*.csv")

if(length(files) > 0){
  for(file in files){
    proc_file <- str_split(file, ".c", 2)[[1]][1]
    data <- proc_data(proc_file)
    if(length(data)>0){
      write.table(
        as.data.frame(data),
        paste(proc_file,'_num_data.txt',sep=''),
        append = FALSE,
        sep = "\t",
        quote = FALSE,
        row.names = TRUE,
        col.names = NA
      )
    }
  }
}
