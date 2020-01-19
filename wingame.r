wingame.fcn <- function(lpts,wprob){
# calculate the probability of winning a game by a score
# of 11-lpts if lpts <= 9,
# or by lpts+2-lpts if lpts >=10
# lpts is the number of points scored by the loser
# wprob is the probability that the winning player scores a
# given point
#gameprob <- ifelse(lpts<=9,dnbinom(lpts,11,wprob),dnbinom(lpts,2+lpts,wprob))
xm10 <- lpts-10
gameprob <- ifelse(lpts<=9,dnbinom(lpts,11,wprob),
dbinom(10,20,wprob)*wprob^(xm10+2)*(1-wprob)^xm10*2^xm10)
return(gameprob)}


# how about win probabilities for given current scores??
# Likelihood inference!
