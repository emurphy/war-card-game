library(ggplot2)
library(gridExtra)
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
