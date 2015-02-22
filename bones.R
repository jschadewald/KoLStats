# These are functions used to determine the distribution of unique bones
# based on the number of random bones generated.
# These are computationally expensive, so should only be used for small
# values of their arguments (length <=1000).

# Returns the probability of selecting a "k-complete string" from strings of
# a given `length` over an alphabet of size `numAlpha`.
# A string is called "k-complete" if it contains at least one of each
# character/symbol in a subset of size `k` of the alphabet.
pKcompleteStrings <- function(length, numAlpha, k=numAlpha) {
    # Simplify variable names and manage nonsense
    l<-as.integer(length)
    n<-as.integer(numAlpha)
    k<-as.integer(k)
    if (l < k || n < k) return(0) #C'mon! Really?
    
    sum(sapply(0:k, function(i) {(-1)^i * choose(k,i) * ((n-i)/n)^l}))
}

# Generic Discrete Quantile function that computes quantiles from 
# a given discrete cumulative distribution function. The `cdf` is assumed
# to be a vector of values s.t. cdf[i] represents P(X < i-1).
qDiscreteCDF <- function(p, cdf) {
    indices<-which(cdf >= p)
    if (length(indices)) {
        return(min(indices)-1) #The smallest index represents the quantile
    } else { #All values were < p
        return(length(cdf)-1)
    }
}


# Probability of getting `u` unique values in a string of length `l` where
# the alphabet has `n` letters and where the possible values of `u` are 
# taken from a set of size `k`, which itself is a subset of `n`.
# IMPORTANT:
# NYI in the UI because it acts strangely for many combinations of inputs.
# The plan is to replace it with an appropriate continuous approximation.
ukStrings <- function(length, numAlpha, k=numAlpha, unique=k) {
    l<-as.integer(length)
    n<-as.integer(numAlpha)
    k<-as.integer(k)
    u<-as.integer(unique)
    if (l < u || n < k || k < u) return(0) #C'mon! Really?
    
    ret<-choose(k, u) * 
            sum(sapply(0:(l-u), function(i) {
                ((n-k)/n)^i * choose(l,l-i) *
                    sum(sapply(0:(u-1), function(j) {
                        (-1)^j * ((u-j)/n)^(l-i) * choose(u,j)
                    }))
            }))
    ret
}
