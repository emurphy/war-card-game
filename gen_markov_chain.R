library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)

# plot number of cards in hand over the course of games
deck_sizes <- read.csv("deckSizes100random.csv")
games <- read.csv("games100random.csv")
gamePlays = merge(deck_sizes, games, by = 'id')
gamePlays$cards = gamePlays$p1Cards
gamePlays$Game = gamePlays$id

wins <- gamePlays[gamePlays$result=='W',]
losses <- gamePlays[gamePlays$result=='L',]
ties <- gamePlays[gamePlays$result=='T',]
p1 <- ggplot(data=wins, aes(x=play, y=cards)) + geom_line(aes(color=Game), size=0.3) +
    ggtitle(paste("Plays per Game in", length(unique(wins[,'id'])), "Wins")) + 
    xlab('Number of Plays') + ylab("Cards in Hand")
p2 <- ggplot(data=losses, aes(x=play, y=cards)) + geom_line(aes(color=Game), size=0.3) +
    ggtitle(paste("Plays per Game in", length(unique(losses[,'id'])), "Losses")) + 
    xlab('Number of Plays') + ylab("Cards in Hand")
grob <- arrangeGrob(p1, p2, ncol=1)
ggsave('hand_sizes.png', grob, device='png')

# generate Markov matrix
# for probabilities, this is the denominator -- frequency of each hand size
handCountFreq <- plyr::count(gamePlays, vars='cards')

# the numerator is trickier -- need to count how many times a hand size follows any given hand size
gamePlays$nextCards <- lead(gamePlays$cards, 1)
counts <- ddply(gamePlays, .(gamePlays$cards, gamePlays$nextCards))
names(counts) <- c("cards", "nextCards", "Freq")
# The probability of transitioning from column p to column q
probTransit <- function(p, q) {
    # Columns p, q correspond to card counts p-1, q-1
    x <- p - 1; y <- q -1
    numerator <- counts[counts$cards == x & counts$nextCards == y,]
    denominator <- handCountFreq[handCountFreq$cards == x,]$freq
    prob <- (nrow(numerator) / denominator)
    if (is.na(prob) || is.nan(prob)) {
        prob <- 0
    }
    return (prob)
}

M <- matrix(0L, nrow=53, ncol=53)
# Fill the transitions to 0 and 52 cards outside the main loop, since the 
# transitions are only one-way
for (j in 2:52) {
    M[1,j] <- probTransit(j, 1)
    M[53,j] <- probTransit(j, 53)
}
# When have 0 or 52 cards, probability of keeping that many is 1 
M[1,1] = 1.0 
M[53,53] = 1.0 

# populate the matrix
# value in cell i,j is the likelihood of moving from i to j
for (i in 2:52) {
    for (j in 2:52) {
        if (i == j) {
            # we resolve ties within a play, so the number of cards always changes 
            # between plays (i.e. probability of staying on a card count is 0)
            M[i,j] <- 0.0
        } else {
            M[i,j] <- probTransit(i, j)
        }       
    }
}

write.csv(M, "markov_matrix.csv")

