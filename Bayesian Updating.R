library(ggplot2)
library(lattice)

#############################
# CONDITIONAL PROBABILITIES #
#############################

#simulate likelihood of U.S. winning, losing, or tie conditional on North Korea being WEAKER

	#define underlying distributions such that North Korea's is limited to weapons of level 1 to 5
	us_range<-1:10
	nk_range<-1:5

	#simulate 1000 draws from each country's distribution, and determine outcome of each pair of draws
	N<-1000
	n=1
	results<-data.frame(result=0)
	repeat{
		us_sample<-sample(us_range,1,replace=TRUE) #draw from US distribution #**POINT: Uses for-loops and a random number generator
		nk_sample<-sample(nk_range,1,replace=TRUE) #draw from NK distribution
		result<-ifelse(us_sample>nk_sample,1,ifelse(us_sample<nk_sample,2,3)) #if US wins, loses, or ties
		results<-rbind(results, result)
		
		n<-n+1
		if(n>N) break
		}
	table(results$result) #output table of results
	#probabilities of U.S. winning, losing, or tie are 0.2, 0.7, 0.1 respectively

#simulate likelihood of U.S. winning, losing, or tie conditional on North Korea being STRONGER

	#define underlying distributions such that North Korea's weapons range between level 6 to 10
	us_range<-1:10
	nk_range<-6:10

	#simulate 1000 draws from each country's distribution, and determine outcome of each pair of draws
	N<-1000
	n=1
	results<-data.frame(result=0)
	repeat{
		us_sample<-sample(us_range,1,replace=TRUE) #draw from US distribution
		nk_sample<-sample(nk_range,1,replace=TRUE) #draw from NK distribution
		result<-ifelse(us_sample>nk_sample,1,ifelse(us_sample<nk_sample,2,3)) #if US wins, loses, or ties
		results<-rbind(results, result)
		
		n<-n+1
		if(n>N) break
		}
	table(results$result) #output table of results
	#probabilities of U.S. losing, winning, or tie are 0.7, 0.2, 0.1 respectively

#**POINT: uses a permutation test or other statistical test (Bayesian Updating)

#######################################
# SINGLE BAYESIAN UPDATING SIMULATION #
#######################################
	
	priors <- data.frame(n=0, p=0)

	#define how probability p is updated depending on the outcome of each interaction
	f1<-function(p) 0.7*p/(0.7*p+0.2*(1-p)) #if outcome is that U.S. wins
	f2<-function(p) 0.2*p/(0.2*p+0.7*(1-p)) #if outcome is that U.S. loses
	f3<-function(p) 0.1*p/(0.1*p+0.1*(1-p)) #if outcome is that U.S. ties

	#initialize prior as p=0.5 (equally likely for NK to have weaker or stronger arsenal than U.S.)
	p<-0.5 

	#simulate 50 repeated interactions between U.S. and NK
	N<-50
	n=1
	repeat {
		set.seed(423)
		result<-sample(c(1,2,3), 1, replace=TRUE, c(0.7,0.2,0.1)) #draw outcome from underlying true distribution where U.S. is stronger
		if (result==1) {f<-f1 
			} else if (result==2) {f<-f2
			} else if (result==3) {f<-f3 
			}

		p<-f(p) #update probability
		priors<-rbind(priors, c(n,p))
	
		n<-n+1
		if (n>N) break
	}
	plot(priors$n, priors$p) #plot sequence of updated probabilities


##########################################
# MULTIPLE BAYESIAN UPDATING SIMULATIONS #
##########################################
	
#define multiple underlying distributions ranging between NK being a lot weaker, the same as, or a lot stronger than the US

#given US distribution of 1 to 10, define 11 different distributions for NK:
## weaker than the US: 1-5, 1-6, 1-7, 1-8, 1-9
## same as the US: 1-10
## stronger than the US: 2-10, 3-10, 4-10, 5-10, 6-10
range<-data.frame(r1=1,r2=5)
for (d in 1:10) {
	r1<-ifelse(d<=5, 1, d-4)
	r2<-ifelse(d<=5, 5+d, 10)
	range<-rbind(range,c(r1,r2))
	}
range
#**POINT: Uses for-loops and a random number generator

#repeat bayesian updating simulation for each of the 11 underlying distributions for NK
priors <- data.frame(d=NA, n=NA, p=NA)
for (distr in 1:11) {		
	#define underlying distribution
	us_range<-1:10
	nk_range<-range[distr,1]:range[distr,2]
	
	#simulate 1000 draws from each country's distribution, and determine outcome of each pair of draws
	N<-1000
	n=1
	results<-data.frame(result=0)
	repeat{
		us_sample<-sample(us_range,1,replace=TRUE) #draw from US distribution
		nk_sample<-sample(nk_range,1,replace=TRUE) #draw from NK distribution
		result<-ifelse(us_sample>nk_sample,1,ifelse(us_sample<nk_sample,2,3)) #if US wins, loses, or ties
		results<-rbind(results, result)
			
		n<-n+1
		if(n>N) break
		}
	cond_probs<-table(results$result) #store simulated conditional probabilities

	#define how probability p is updated depending on the outcome of each interaction
	f1<-function(p) cond_probs[2]*p/(cond_probs[2]*p+cond_probs[3]*(1-p)) #if outcome is that U.S. wins
	f2<-function(p) cond_probs[3]*p/(cond_probs[3]*p+cond_probs[2]*(1-p)) #if outcome is that U.S. loses
	f3<-function(p) cond_probs[4]*p/(cond_probs[4]*p+cond_probs[4]*(1-p)) #if outcome is that U.S. ties
	#**POINT: defines and uses at least two functions

	#initialize prior as p=0.5 (equally likely for NK to have weaker or stronger arsenal than U.S.)
	p<-0.5 

	#simulate 30 repeated interactions between U.S. and NK
	N<-30
	n=1
	repeat {
		set.seed(423)
		result<-sample(c(1,2,3), 1, replace=TRUE, c(cond_probs[2]/1000,cond_probs[3]/1000,cond_probs[4]/1000)) #draw outcome from underlying true distribution
		if (result==1) {f<-f1 
			} else if (result==2) {f<-f2
			} else if (result==3) {f<-f3 
			}

		p<-f(p) #update probability
		priors<-rbind(priors, c(distr,n,p))
	
		n<-n+1
		if (n>N) break
	}
}
cloud(p ~ n * d, priors, xlab="number of interactions", ylab="relative strength of NK arsenal", zlab="posterior", pch = 20, cex = 2, scales = list(arrows = TRUE))
##plot how quickly bayesian updating approaches certainty, depending on how close the underlying distributions are between the two countries

##break down plot into components
	#simulations where the NK distribution is weaker than the US (but increasing becoming more evenly matched)
	ggplot(subset(priors, d<6) , aes(n,p,colour=d))+geom_point()

	#simulation where the NK distribution is the same as that of the US
	ggplot(subset(priors, d==6) , aes(n,p,colour=d))+geom_point()

	#simulation where the NK distribution is stronger than that of the US (and increasingly becoming stronger)
	ggplot(subset(priors, d>6), aes(n,p,colour=d))+geom_point()



