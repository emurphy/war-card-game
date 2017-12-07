df <- read.csv("markov_matrix.csv")
drops <- c("X")
options(scipen = 999) # disable scientific notation
M <- as.matrix(df[ , !(names(df) %in% drops)])

# predict the probability of a hand winning based on number of cards
winProbability <- function(M, numCards) {
    # create a vector for the starting position
    v <- numeric(53)
    v[numCards + 1] = 1
    
    # multiply by the Markov Matrix to get the probability of winning
    Mprob <- M %*% v
    return (Mprob[53])
}

M2 <- M %*% M
sum(M[,1])
M2[1,1]
v <- numeric(53)
v[1] = 1
Mprob <- M2 %*% v
Mprob[1,1]
Mprob[26,1]
M4 <- M2 %*% M2
M8 <- M4 %*% M4
winProbability(26)
winProbability(1)
winProbability(51)
winProbability(52)