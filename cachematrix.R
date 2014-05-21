## The makeCacheMatrix function creates a special 'matrix' object which is 
## able to cache its inverse.  It returns a list of methods for 'getting'
## and 'setting' the inverse in the cache.  The matrix must be square for the
## functions to work.

makeCacheMatrix <- function(x = matrix()) {
    # initialise inverse matrix
    inv <- NULL
    set <- function(y) {
        # place matrix and empty inverse matrix in function environment (cache)
        x <<- y
        inv <<- NULL
    }
    # function to 'get' the original matrix
    get <- function() x
   
    # function to place the inverse in the cache
    setinverse <- function(solve) inv <<- solve
   
    # function to 'get' the inverse from the cache
    getinverse <- function() inv
    
    # return the 'matrix' of functions 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the matrix placed in the 
## cache by makeCacheMatrix function.  If the inverse has already been computed
## then it will retrieve it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # use getinverse method to retrieve cache contents
    inv <- x$getinverse()
    
    # return cache contents if it isn't empty
    if(!is.null(inv)) {  
        message("getting cached data")
        return(inv)
    }
    # if the cache is empty the original matrix is retrieved
    data <- x$get()
    
    # calculate the matrix inverse
    inv <- solve(data, ...)
    
    # place the inverse in the cache
    x$setinverse(inv)
    
    # return the calculated inverse
    inv
}
