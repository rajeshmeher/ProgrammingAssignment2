## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function to create Cached Matrix
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL  ## Variable to cache inverse of x. Initialized to null
        set <- function(y) {
                ## Function to set value of x
                x <<- y ## Change value of x to passed in value
                im <<- NULL ## Reset cached value as x has changed
        }
        get <- function() x ## Return matrix  
        setinv <- function(inv) im <<- inv ## Cache inverse in variable im
        getinv <- function() im ## Return value of im as inverse or null if not cached
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invmet <- x$getinv() ## Get inverse. Null if not cached 
        if(!is.null(invmet)) {
                message("getting cached data")
                return(invmet)
        }
        ## Inverse was not cached so create the cache
        data <- x$get() ## Get data
        message("solving fresh")
        invmet <- solve(data, ...) ## Create Inverse
        x$setinv(invmet) ## Cache inverse by calling setinv
        invmet ## Return inverse 
}
