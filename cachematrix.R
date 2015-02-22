
## The two functions makeCacheMatrix and cacheSolve are intended to
## work together and, when used properly, will cache results of
## solve(x, ...). To create objects that can cache the inverse of
## solve call makeCacheMatrix. To solve and cache, use objects created
## with makeCacheMatrix as the first argument to cacheSolve. Upon
## first (successful) invocation of cacheSolve, the inverse will be
## solved, cached and returned. Any further invocation of cacheSolve with the
## same object, will return the cached inverse directly. 




## Function to create a cached matrix object that is designed to work
## in conjunction with cacheSolve. 
## 
## Given a matrix in argument x, it creates a list of functions that
## close over the matrix data given and, when evaluated (or solved) at
## leased once, over the inverse of the matrix data as well.
makeCacheMatrix <- function(x = matrix()) {
    ## object to hold the inverse matrix
    i <- NULL;
    
    ## accessor functions for the matrix
    set <- function (y) {
        x <<- y;
        i <<- NULL;
    }
    get <- function () x;

    ## accessor functions for the inverse
    setinverse <- function(inv) i <<- inv;
    getinverse <- function() i;


    ## return a list as "interface" to our cached matrix "type"
    list(set = set, 
         get = get, 
         setinverse = setinverse,
         getinverse = getinverse);
}


## Function to solve and cache the inverse of a matrix. The first
## argument x must be an object created with makeCacheMatrix. The
## remaining arguments (...) are given to solve directly. Returns the
## inverse of x.
cacheSolve <- function(x, ...) {
    ## check whether the inverse was already computed
    i <- x$getinverse()
    if(!is.null(i)) {
        ## inverse already known
        message("Getting cached inverse!")
        return(i)
    }
    # inverse not cached, calculate first
    i <- solve(x$get(), ...)

    # then cache and return
    x$setinverse(i)
    i
}
