## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## check if square matrix
    if(nrow(x) != ncol(x)) {
        message("square matrix input required")
        return()
    }
    ## check for singular matrix
    if(det(x) < 1.0e-8) {
        message("singular matrix")
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


## Write a short comment describing this function

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
