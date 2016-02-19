## Calculates and saves inverse of a (singular) matrix

## Saves the input matrix and assigns a cache for the inverse matrix.
## function also checks for singularity.

makeCacheMatrix <- function(x = matrix()) {
    ## check if square matrix
    if(nrow(x) != ncol(x)) {
        message("square matrix input required")
        return()
    }
    ## check for singular matrix
    if(det(x) < 1.0e-8) {
        message("singular matrix does not have inverse")
        return()
    }
    ## main logic
    matInv <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        matInv <<- NULL
    }
    getInverse <- function() matInv
    setInverse <- function(solve) matInv <<- solve
    ## result list
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes inverse matrix and saves in cache. 
## If inverse is already present in cache, calculation is skipped.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matInv <- x$getInverse()
    ## check if inverse is in cache
    if(!is.null(matInv)) {
        message("getting inverse")
        return(matInv)
    }
    ## get data
    data <- x$get()
    ## solve inverse
    matInv <- solve(data)
    ## set inverse
    x$setInverse(matInv)
    ## return
    matInv
}
