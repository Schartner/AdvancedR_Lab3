#' Euclidian Algorithm
#' 
#'A method for finding the greatest common divisor (GCD)
#'
#'@param a a numeric scalar
#'@param b a numeric scalar
#'
#'@return The largest number that divides the two numbers (\code{a} and \code{b}) without leaving any remainder.
#'
#'@examples
#'euclidian(123612, 13892347912) #4
#'euclidian(100,10000) #100
#'
#'@references \url{http://en.wikipedia.org/wiki/Euclidean_algorithm}
#'

euclidian <- function(a, b){
    #assertion
    stopifnot(length(a) <= 1)
    stopifnot(length(b) <= 1)
    stopifnot(is.numeric(a) & is.numeric(b))
    
    #positive numbers
    a <- abs(a)
    b <- abs(b)
    
    while(b != 0){
        t <- b
        b <- a %% b
        a <- t
    }
    return(a)
}