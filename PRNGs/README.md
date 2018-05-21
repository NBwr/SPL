```yaml

Name of Quantlet: PRNGs

Published in:      
  
Description: Different functions for creating random numbers and plots.
 
Keywords: random numbers, shiny app, plots, performance 

See also: Randnmbrs.shiny

Author: Nikolaj Bewer
  
Submitted: Fri, March 30 2018 by Nikolaj Bewer

```


```r
# Note: All generators take (at least) the input n, which is the quantity of generated numbers


# The Middle-square method
msm = function(Seed, n) {
    # Seed is not provided, n is the number of PRN
    a = nchar(Seed)  
    b = nchar(Seed)/2  
    empty.vec = NULL  
    for (i in 1:n) {
        Seedseed  = Seed^2  
        Seed      = (Seedseed%/%10^b)%%10^a  
        empty.vec = c(empty.vec, Seed)  
    }
    return(empty.vec)
}


# The Lehmer number generator
mcg.lehmer = function(n, a = 48271, m = 2147483647) {
    Rnum      = as.numeric(Sys.time()) 
    Rnumb.out = vector(length = n)  
    for (i in 1:n) {
        Rnum         = (a * Rnum)%%m
        Rnumb.out[i] = Rnum/m 
    }
    return(Rnumb.out) 
}


# The Lagged Fibonacci Generator
LFG = function(n, j = 7, k = 10, m = 2147483647, Seed = c(4, 8, 2, 8, 3, 9, 1, 8, 7, 1)) {
    gn.out = NULL  # Initializes gn.out
    for (i in 0:n) {
        Output    = (Seed[j + i] + Seed[k + i])%%m  
        i         = i + 1  
        Seed      = append(Seed, Output)  
        gn.out[i] = Output  
    }
    return(gn.out)  
}

```
