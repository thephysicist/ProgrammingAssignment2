## ========================================================================
## This set of functions allows us to create a specialized class holding a matrix
## and its (if exists) cache inverse.  The specialized class is instantiated using
## makeCacheMatrix.  The output of makeCacheMatrix can be used in cacheSolve to 
## compute and cache the inverse to save time and computation cycles.
## ========================================================================


## ======================================================================== 
## This method 'initialized an object of a specialized class that holds
## an invertable matrix and its inverse.  The retrieval of the matrix and
## its inverse are done via the methods:
##          get -- gets the matrix
##          set -- sets the matrix
##   getInverse -- gets the inverse of the matrix
##   setInverse -- sets the inverse of the matrix
## ========================================================================
makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() { x }
    setInverse <- function(inInverse) { inv <<- inInverse }
    getInverse <- function() { inv }
    
    # return a list containing four functions
    # somehow this source file fails to load when the '(' character is
    # on a line by itself :(
    list (
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## ========================================================================
## Computes the inverse of a square matrix, using cache if a cache exists
## ========================================================================
## Inputs:
##    x - a list of four functions:
##          get -- gets the matrix
##          set -- sets the matrix
##   getInverse -- gets the inverse of the matrix
##   setInverse -- sets the inverse of the matrix
## 
## Output:
##    The inverse of x.$get().
##
##
## Side-effect:
##    The inverse is cached inside of the input x.
##
## ========================================================================
cacheSolve <- function(x, ...)
{
    ## Check whether the inverse exists.
    ## In the inverse exists, then return the cache
    inv <- x$getInverse()
    if(!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    
    ## The inverse does not exist, so compute the inverse
    ## and then cache it inside of the input x
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    
    ## return the just-computed inverse
    return(x$getInverse())
}
