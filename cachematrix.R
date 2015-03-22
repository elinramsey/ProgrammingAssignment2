## The goal of this pair of functions is to avoid running costly processes multiple times.

## This function returns a list of functions that are used in the cacheSolve function.
## with this we can return a cached value if one exists or set a new cache value if it doesn't

makeCacheMatrix <- function(x = matrix()) {

  ## m can be thought of as the solved inverse
  ## x is the matrx to be solve
  
  ## here we set m to null
  m <- NULL
  ## if the set function is called, that is because there isn't already a solved cached.
  ## as such, m (the solved inverse) must be reset to null and x is set to the passed in matrix
  set <- function(y) {
    ## we use the <<- modifier because the environment of "makeCacheMatrix" is differnt than cacheSolve
    x <<- y
    m <<- NULL
  }
  ## get returns the unsolved matrix
  get <- function() x
  ## set inv takes a solved matrix and assigns it to the internal variable for solved matrix. 
  ## the << is used because the environment of "makeCacheMatrix" is differnt than cacheSolve 
  setinv <- function(inv) m <<- inv
  ## getinv will return a previously cached solved matrix
  getinv <- function() m
  ## this list returns a function that will be excecuted in the environment of makeCacheMatrix rather than where it is called.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve will either pull up the cached solved matrix, or solve the matrix if one isn't already solved.

cacheSolve <- function(x, ...) {

    ##This checks to see if the solved inverse variable had previously been set and stored.
    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    ##solve is the function to find the inverse of a square matrix.
    m <- solve(data, ...)
    x$setinv(m)
    m
  
}
