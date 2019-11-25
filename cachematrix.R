## These functions can create an inverse matrix and cache the result, in order
##    to reduce computation times arising from computing the same inverse matrix
##    over and over again. This way, a matrix inversion can be calculated once,
##    stored in memory, and recalled instantly when it is needed again.

## The makeCacheMatrix is the "getter and setter" function of this pair. This
##    will store any calculated inverse matrix in cache. Retrieving existing
##    matrices or storing new matrices will be completed through this function.
##    There are four functions inside of this function that will be called to
##    either set the matrix or inverse (i.e. store it in cache), or get the 
##    matrix of inverse (i.e. retrieve the data from cache).

makeCacheMatrix <- function(x = matrix()) { # x initialized as an empty matrix
    # calculatedInverse set to NULL to initialize it as an object to be used later.
    calculatedInverse <- NULL
    # "set" will assign a new matrix to "x" stored in the parent environment.
    # calculatedInverse will be set to NULL to clear any previously saved inverses.
    set <- function(newMatrix) {
        x <<- newMatrix
        calculatedInverse <<- NULL
    }
    # "get" will retrieve the matrix "x" stored in the parent environment
    get <- function() { x }
    # "setInverse" will assign the inverted matrix from cacheSolve.
    setInverse <- function(solved) { calculatedInverse <<- solved }
    # "getInverse" will return the calculatedInverse (or NULL if none are calculated)
    getInverse <- function() { calculatedInverse }
    # The function will return a list of the four functions described above for
    #   use in the parent environment, and allows the use of the $ extract operator.
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve is where the inverse function will actually be calculated. It will
##    first check to make sure there is no cache already in the makeCacheMatrix,
##    to save any computing time. If it does not find any in cache, it will
##    solve the inversion, and use makeCacheMatrix to store the results.

cacheSolve <- function(x, ...) {
    calculatedInverse <- x$getInverse()
    # calculatedInverse gets the stored value from makeCacheMatrix
    # (from the parent environment)
    # It will then check to see if that value is NULL
    if(!is.null(calculatedInverse)) {
      print(calculatedInverse)
      # If the value is NOT null, the cached data will be returned.
      message("Retrieving cached data...")
      return(calculatedInverse)
    }
    # the data will be retrieved from the input:
    data <- x$get()
    # the inverse will be calculated:
    calculatedInverse <- solve(data, ...)
    # the calculatedInverse matrix will be stored in cache using makeCacheMatrix's
    #   "setInverse" function.
    x$setInverse(calculatedInverse)
    # The caculated Inverse matrix will be returned.
    calculatedInverse
}
