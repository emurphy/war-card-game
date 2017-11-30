library(ggplot2)
#library(gridExtra)
# plot number of cards in hand over the course of games
deck_sizes <- read.csv("deckSizes100random.csv")
games <- read.csv("games100random.csv")
merged = merge(deck_sizes, games, by = 'id')
merged$cards = merged$p1Cards
wins <- merged[merged$result=='W',]
losses <- merged[merged$result=='L',]
ties <- merged[merged$result=='T',]
#ggplot(data=merged, aes(x=play, y=p1Cards, group=result)) + geom_line(aes(color=result))
p1 <- ggplot(data=wins, aes(x=play, y=cards)) + geom_line(color='#009E73', size=0.3) + ggtitle("Wins")
p2 <- ggplot(data=losses, aes(x=play, y=cards)) + geom_line(color='#D55E00', size=0.3) + ggtitle("Losses")
p3 <- ggplot(data=ties, aes(x=play, y=cards)) + geom_line(color='#F0E442', size=0.3) + ggtitle("Ties")
grob <- arrangeGrob(p1, p2, p3, ncol=1)
ggsave('deck_sizes.png', grob, device='png')
