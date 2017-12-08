df <- read.csv("markov_matrix.csv")
drops <- c("X")
options(scipen = 999) # disable scientific notation
M1 <- as.matrix(df[ , !(names(df) %in% drops)])

# predict the probability of a hand with x number of cards winning after n plays
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

# probability of coming back from way behind
winProbability(M1, 30, 2)

# probability of winning when way ahead
winProbability(M1, 2, 50)
winProbability(M1, 10, 50)
winProbability(M1, 100, 50)

# less extreme
winProbability(M1, 30, 13)
winProbability(M1, 10, 39)
