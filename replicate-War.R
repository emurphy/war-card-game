# Code for replicating War card Game
# from https://www.r-bloggers.com/simulated-war/
# and http://www.premiersoccerstats.com/wordpress/?p=825&utm_source=rss&utm_medium=rss&utm_campaign=simulated-war-2

# Andrew Clark April 01 2012

library(sets)

# make simulation replicable
set.seed(1068)

games <- data.frame(id=numeric(), strength=numeric(),strength_pct=numeric(), aces=numeric(),faces=numeric(),deuces=numeric(),plays=numeric(),infinite=logical(),result=character())
deckSizes <- data.frame(id=numeric(),play=numeric(),p1Cards=numeric())
i <- 1
game_count <- 100
for (i in 1:game_count) {
    # create a regular deck.
    # All suits are equivalent so there will be four of each number
    deck <- rep(2:14,4)
    deck_strength <- sum(deck)
    
    assign("p1", sample(deck,26, replace=FALSE))
    
    diffs <- gset_difference(as.gset(deck), as.gset(p1))
    # create vector
    p2 <- rep(unlist(diffs), times=gset_memberships(diffs))
    # this produces the right cards but in order so randomize
    assign("p2", sample(p2,26, replace=FALSE))
    p1
    # [1]  3  3  6 14 10 13  3 10 12  5 11  8  2  5  8  4 10  5  8  4
    # [21]  8  7  7  7  9 14
    p2
    # [1] 12  6  9  5  9  9 13  4  2 14  4 13 13  6  6 11  7 11 12  2
    # [21] 10  3  2 11 12 14
    p1Cards <- length(p1)
    strength <- sum(p1) # 196 total of players is always 416 so p2 has stronger hand
    strength_pct <- strength / deck_strength
    aces <- sum(p1==14) # 2
    faces <- sum(p1 >= 10 && p1 <=13)
    deuces <- sum(p1==2) # 1
    result <- "N"

    game <- data.frame(id=i, strength=strength,strength_pct=strength_pct,aces=aces,faces=faces,deuces=deuces,result=result, plays=0, infinite=FALSE,stringsAsFactors=FALSE)
    
    draw <- c(p1[1],p2[1])
    
    booty <- c()
    
    repeat { # for each match of cards
        game$plays <- game$plays + 1
        if (p1[1]>p2[1]) {
            if (p1[1]==14&p2[1]==2){ #  ace(14) vs a 2
                p2 <- c(p2[-1],draw)
                p1 <- p1[-1]
            } else {
                p1 <- c(p1[-1],draw)
                p2 <- p2[-1]
            }
        } else if (p2[1]>p1[1]) {
            if (p2[1]==14&p1[1]==2){
                p1 <- c(p1[-1],draw)
                p2 <- p2[-1]
            } else {
                p2 <- c(p2[-1],draw)
                p1 <- p1[-1]
            }
        } else {
            while (p1[1]==p2[1]) {
                
                # need at least 5 cards to play game
                if (length(p1)<5|length(p2)<5) {
                    break
                }
                # displayed card plus next three from each player
                booty <- c(booty,p1[1],p1[2],p1[3],p1[4],p2[1],p2[2],p2[3],p2[4])
                
                #  remove these cards from the p1,p2 so that new p1[1] is next shown
                p1 <- p1[-(1:4)]
                p2 <- p2[-(1:4)]
            }
            
            draw <- c(p1[1],p2[1])
            
            if (p1[1]>p2[1]) {
                p1 <- c(p1[-1],booty,draw)
                p2 <- p2[-1]
            } else {
                p2 <- c(p2[-1],booty,draw)
                p1 <- p1[-1]
            }
        }
        #  battle over
        
        # keep running total of deck size
        p1Cards <- c(p1Cards,length(p1))
        
        # test for game over
        if(length(p1)==52|length(p1)==0){
            break
        }
        
        limit = length(deck) / 2
        # avoid infinite loop with an approach deriving from the "limit of a sequence" definition
        # > For all epsilon, there exists an N such that for all n > N, |s_n - s| < epsilon
        # In this case, if in the last 26 plays the number of cards are within 1
        # of the limit of 26 (half the deck), the game has converged to the limit (a tie)
        # and will last forever if we let it.
        if (length(p1Cards) > limit && all(tail(p1Cards,limit) %in% ((limit-1):(limit+1)) )) {
            print(paste("Infinite game:", game$id, ", plays:", game$plays))
            game$infinite=TRUE
            break
        }
        else if (length(p1Cards) > 5000) {
            print(paste("Infinite game not caught by limit, game:", game$id, ", plays:", game$plays))
            game$infinite=TRUE
            break
        }
        # reset for next iteration
        booty <- c()
        draw <- c(p1[1],p2[1])
        
    }
    # war over
    
    
    if (length(p1) == 0) {
        #p1Cards <-   -(p1Cards-52)
        game$result = "L"
    } else if (length(p1) == 52) {
        game$result = "W"
    } else if (game$infinite) {
        game$result = "T"
    } else {
        stop(paste("Unclear result with p1 cards: ", length(p1)))
    }
    
    games <- rbind(games,game)
    deckSize <- data.frame(id=i,play=1:length(p1Cards),p1Cards=p1Cards)
    deckSizes <- rbind(deckSizes,deckSize)
    
}

# save for later analysis
write.csv(games,paste("games", game_count, "random.csv", sep=''))
write.csv(deckSizes,paste("deckSizes", game_count, "random.csv", sep=''))
