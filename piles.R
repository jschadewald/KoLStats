# These are functions used to predict the number of random animal bones
# received from using a specified number of piles of dusty animal bones.
# These are computationally expensive, so should only be used for small
# number of piles. For larger values, use a normal distribution instead.

# Probability of getting `rb` random bones from `np` piles.
# Don't pass negative/nonsensical values, please.
prbFromPiles <- function(np, rb) {
    #Manage funny business.
    np<-as.integer(np)[1]
    rb<-as.integer(rb)[1]
    if (rb > 2*np) return(0)
    
    # Below, we partition rb into sums of 1's and 2's.
    # `m` is the number of 2's and `n` is the number of 1's.
    d<-data.frame(m=0:((rb - rb%%2)/2))
    d$n<-rb-2*d$m
    d<-d[d$n+d$m<=np,] # Drop rows that require more than np piles.
    
    # Probability of getting np-n-m 0's, n 1's, and m 2's.
    d$p<-.75^(np-d$n-d$m)*.2375^d$n*.0125^d$m*choose(np, d$n)*choose(np-d$n,d$m)
    sum(d$p)
}

# Probability Mass Function of random animal bones from dusty piles
pmfFromPiles <- function(piles) {
    sapply(0:(piles*2), function(i) prbFromPiles(np=piles, rb=i))
}

# Cumulative Distribution Function of random animal bones from dusty piles
cdfFromPiles <- function(piles) {
    pmf<-pmfFromPiles(piles) #Vector of length 2*piles + 1
    len<-length(pmf)
    ret<-integer(len)
    ret[1]<-pmf[1]
    for (i in 2:len) {
        ret[i]<-ret[i-1]+pmf[i]
    }
    ret #Vector of length 2*piles + 1
}

# Quantile Function of random animal bones from dusty piles. Here, we use the
# definition Q(p) = inf{x | p <= CDF}
# See http://en.wikipedia.org/wiki/Quantile_function
qFromPiles <- function(p, numPiles=NULL, cdf=NULL, lower.tail=TRUE) {
    if (!is.null(numPiles)) cdf<-cdfFromPiles(numPiles)
    
    if (lower.tail) {
        indices<-which(cdf >= p)
        if (length(indices)) {
            return(min(indices)-1) #The smallest index represents the quantile
        } else { #All values were < p
            return(length(cdf)-1)
        }
    } else { #Upper tail
        indices<-which(cdf < p)
        if (length(indices)) { #The largest index represents the quantile
            return(max(indices)-1)
        } else { #All values were >= p
            return(0)
        }
    }
}
