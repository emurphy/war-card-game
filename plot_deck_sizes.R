library(ggplot2)
library(gridExtra)
library(plyr)
#library(dplyr)

# plot number of cards in hand over the course of games
deck_sizes <- read.csv("deckSizes5000random.csv")
games <- read.csv("games5000random.csv")
merged = merge(deck_sizes, games, by = 'id')
merged$cards = merged$p1Cards
wins <- merged[merged$result=='W',]
losses <- merged[merged$result=='L',]
ties <- merged[merged$result=='T',]
#ggplot(data=merged, aes(x=play, y=p1Cards, group=result)) + geom_line(aes(color=result))
p1 <- ggplot(data=wins, aes(x=play, y=cards)) + geom_line(color='#009E73', size=0.3) + ggtitle(paste(length(unique(wins[,'id'])), "Wins"))
p2 <- ggplot(data=losses, aes(x=play, y=cards)) + geom_line(color='#D55E00', size=0.3) + ggtitle(paste(length(unique(losses[,'id'])), "Losses"))
p3 <- ggplot(data=ties, aes(x=play, y=cards)) + geom_line(color='#F0E442', size=0.3) + ggtitle(paste(length(unique(ties[,'id'])), "Ties"))
grob <- arrangeGrob(p1, p2, p3, ncol=1)
ggsave('hand_sizes.png', grob, device='png')

# explore the ties as a sequence approaching the limit of 26
ties$diff <- abs(ties$p1Cards - 26)
p <- ggplot(data=ties, aes(x=play, y=diff)) + geom_line(color='#CC79A7') + ggtitle('Ties converge to 26 cards')
ggsave('tie_convergence.png', p, device='png')

# generate Markov matrix
# for probabilities, this is the denominator -- frequency of each hand size
handCountFreq <- plyr::count(merged, vars='cards')

# the numerator is trickier -- need to count how many times a hand size follows any given hand size
merged$nextCards <- lead(merged$cards, 1)
counts <- ddply(merged, .(merged$cards, merged$nextCards))
names(counts) <- c("cards", "nextCards", "Freq")
M <- matrix(0L, nrow=53, ncol=53)
M[53,53] = 1.0
# populate the matrix
# value in cell i,j is the likelihood of moving from i to j
for (i in 1:52) {
    for (j in 1:53) {
        if (i == j) {
            if (i == 1) {
                M[i,j] <- 1.0
            } else {
                # we resolve ties within a play, so the number
                # of cards always changes between plays
                M[i,j] <- 0.0
            }
        } else {
            numerator <- counts[counts$cards == i & counts$nextCards == j,]
            denominator <- handCountFreq[handCountFreq$cards == i,]$freq
            #print(paste(i, j, "numerator:", numerator, "denominator:", denominator))
            M[i,j] <-  nrow(numerator) / denominator
        }       
    }
}

write.csv(M, "markov_matrix.csv")