suppressMessages(library(readxl))
suppressMessages(library(rjson))
suppressMessages(library(stringr))


format_multi_df <- function(df, multi_conf) {
  #' Format raw df from google forms.
  #'
  #' Note that the questions are hardcoded into the function.
  
  criterios <-
    df[, grep(c('¿Qué criterio'), colnames(df))] # Get first part of the question
  grados <-
    df[, grep(c('Grado de importancia'), colnames(df))] # Get second part of the question
  
  # Rename both parts with the criteria of the question (from conf)
  for (col in 1:length(colnames(criterios))) {
    colnames(criterios)[col] <- multi_conf[['pares']][as.character(col)]
    colnames(grados)[col] <- multi_conf[['pares']][as.character(col)]
  }
  
  # Remove na rows from criterios in criterios and grados
  criterios.na <- complete.cases(criterios)
  criterios <- criterios[criterios.na, ]
  grados <- grados[criterios.na, ]
  
  # Remove na rows from grados in criterios and grados
  grados.na <- complete.cases(grados)
  criterios <- criterios[grados.na, ]
  grados <- grados[grados.na, ]
  
  return(list(criterios, grados))
}


get_att <- function(list) {
  #' Return the complete list of attributes.
  
  att <- unique(as.vector(as.matrix(list[[1]])))
  att <- att[!grepl('Igual de importantes', att)]
  return(att)
}


get_matrix <- function(list, att, row) {
  #' First step of the extraction of multicriteria weights, returns matrix of
  #' scores based on saaty scale.
  
  # Define matrix:
  matrix <- matrix(nrow = length(att), ncol = length(att))
  rownames(matrix) <- att
  colnames(matrix) <- att
  
  # Load components:
  criteria_names <- colnames(list[[1]]) # Vector of criteria names
  criterios <- list[[1]] # criterios, from format_multi_df()
  grados <- list[[2]] # grados, from format_multi_df()
  
  for (i in att) {
    for (j in att) {
      if (i == j) {
        matrix[i, j] <- 1 # Diagonal of the matrix has to be = 1
      } else{
        # if not in de diagonal:
        rule <- # extract the answer in question 1
          as.character(criterios[row, criteria_names[grepl(i, criteria_names) &
                                                       grepl(j, criteria_names)]])
        grad <-
          # extract the value in question 2 for given answer in question 1
          as.numeric(grados[row, criteria_names[grepl(i, criteria_names) &
                                                  grepl(j, criteria_names)]])
        # Depending on the answer in question one the allocation of the values
        # in the matrix varies
        if (rule == i) {
          matrix[i, j] <- grad
          matrix[j, i] <- 1 / grad
        } else if (rule == j) {
          matrix[i, j] <- 1 / grad
          matrix[j, i] <- grad
        } else{
          matrix[i, j] <- 1
          matrix[j, i] <- 1
        }
      }
    }
  }
  return(matrix)
}


normalise_matrix <- function(matrix) {
  #' Normalise matrix given by get_matrix()
  
  normalised_matrix <- matrix
  for (j in 1:ncol(matrix)) {
    for (i in 1:nrow(matrix)) {
      normalised_matrix[i, j] <- matrix[i, j] / sum(matrix[, j])
    }
  }
  return(normalised_matrix)
}


extract_weights <- function(normalised_matrix) {
  #' Return multicriteria  weights as mean of normalised matrix values.
   
  toret <- list()
  for (row in 1:nrow(normalised_matrix)) {
    toret[[rownames(normalised_matrix)[row]]] <-
      mean(normalised_matrix[row, ])
  }
  return(toret)
}


# Set environment and conf:
setwd(system("pwd", intern = T))
conf <- fromJSON(file = './conf/conf.json')
multi_conf <- fromJSON(file = './conf/multicriteria_conf.json')

# Preprocess data:
df <- read_excel(multi_conf[['path_to_file']])
list <- format_multi_df(df, multi_conf)
att <- get_att(list)

# Calculate weights:
weights <- list()
for (respondent in 1:nrow(list[[1]])) {
  matrix <- get_matrix(list, att, row = respondent)
  matrix <- normalise_matrix(matrix)
  weights_value <- extract_weights(normalised_matrix = matrix)
  for (attribute in att) {
    weights[[attribute]] <-
      c(weights[[attribute]], weights_value[[attribute]])
  }
}

for (attribute in att) {
  weights[[attribute]] <- mean(weights[[attribute]])
}

# Show results:
print(weights)
