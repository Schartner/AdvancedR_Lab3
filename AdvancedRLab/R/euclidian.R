euclidian <- function(a, b){
    #assertion
    stopifnot(length(a) <= 1)
    stopifnot(length(b) <= 1)
    stopifnot(is.numeric(a) & is.numeric(b))
    
    #implementation
    while(b != 0){
        t <- b
        b <- a %% b
        a <- t
    }
    return(a)
}