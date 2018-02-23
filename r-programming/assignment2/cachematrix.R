## Put comments here that give an overall description of what your
## functions do

## The function creates a matrix object to cache the inverse
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solveMatrix) inv <<- solveMatrix
    getInv <- function() inv
    list(set = set, get = get, setInverse = setInv, getInverse = getInv)
}



## The function computes the inverse. If elements of matrix are not changed, it must return the cached data
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    {
        inv <- x$getInv()
        if(!is.null(inv))  
        {
            message("gGetting cached data")
            return(inv)                 #Returns inverse
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv      
    }
}
