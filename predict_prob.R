df <- read.csv("markov_matrix.csv")
drops <- c("X")
options(scipen = 999) # disable scientific notation
M1 <- as.matrix(df[ , !(names(df) %in% drops)])

# predict the probability of a hand with c number of cards winning after n plays
winProbability <- function(M, numPlays, numCards) {
    # create a vector for the starting position
    v <- numeric(53)
    v[numCards + 1] = 1
    
    # multiply by the Markov Matrix to get the probability of winning
    for (i in 1:numPlays) {
        M <- M %*% M
    }
    Mprob <- M %*% v
    return (Mprob[53])
}

M2 <- M1 %*% M1
sum(M[,1])
M2[1,1]
v1 <- numeric(53)
v1[1] <- 1
Mprob <- M2 %*% v
Mprob[1,1]
Mprob[26,1]
M4 <- M2 %*% M2
M8 <- M4 %*% M4
v26 <- numeric(53)
v26[27] <- 1
M8 %*% v26
M64 <- M8 %*% M8 %*% M8 %*% M8
M64 %*% v26
winProbability(M, 10, 26)
winProbability(M, 100, 26)
winProbability(M, 1000, 26)
winProbability(M, 1, 1)
winProbability(M, 10, 1)
winProbability(M, 1000, 1)
winProbability(M, 1, 51)
winProbability(M, 10, 51)
winProbability(M, 100, 51)
