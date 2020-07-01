if(!require(choiceDes)){install.packages("choiceDes");library(choiceDes)}
setwd("~/GitHub/choice-survey")

# Set params
levs1 <- c(3,3,3)

alternatives <- c("Alt A", "Alt B", "Alt C")
atributes <- c("Economic", "Social", "Environmental")
labels <- vector(mode = "list", length(attributes))
labels[[1]] <- c("$10", "$5", "$1")
labels[[2]] <- c("Buena", "Normal", "Mala")
labels[[3]] <- c("Buena", "Normal", "Mala")

nb = 1 # Número de bloques
sets = 10 # Numero de choice sets
alts = 3 # Numero de alternativas en cada choice set


des <- dcm.design(levs1, nb, sets, alts,fname='res.txt', 20)
res <- des$levels
eff <- dcm.design.effcy(des$effects$design)

rename <-
  function(res,
           alternatives,
           atributes,
           labels,
           sets,
           alts,
           nb) {
    alternative.counter <- 1
    for (row in 1:nrow(res)) {
      res[row, "alternative"] <- alternatives[alternative.counter]
      alternative.counter <- alternative.counter + 1
      if (alternative.counter == (alts + 1)) {
        alternative.counter <- 1
      }
      
      for (col in 4:(3 + alts)) {
        res[, col] <- as.character(res[, col])
        res[row, col] <-
          as.character(labels[[(col - 3)]][as.numeric(res[row, col])])
      }
    }
    names(res)[4:(3 + alts)] <- atributes
    return(res)
  }

res <- rename(res, alternatives, atributes, labels, sets, alts, nb)
res
eff
