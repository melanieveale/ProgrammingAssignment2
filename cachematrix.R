## Two fuctions:
## - makeCacheMatrix to create a special matrix object that can cache it's inverse
## - cacheSolve to calculate the inverse of such a matrix (using the cache if possible)


## makeCacheMatrix will create a special matrix object capable of caching it's inverse
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL # to start with, the inverse has not been calculated
  # Next define getting and setting functions for the matrix itself
  set <- function(y) {
    x <<- y # change the value of the matrix itself
    xinv <<- NULL # reset the mean to NULL
  }
  get <- function() x # return the current value of the matrix itself
  # Next define getting and setting functions for the inverse
  setinv <- function(newxinv) xinv <<- newxinv # store a newly calculated inverse for later
  getinv <- function() xinv # return the currently stored inverse (could be NULL)
  list(set = set, get = get, setinv = setinv, getinv = getinv) # I wonder what this does
}


## cacheSolve will solve the inverse of a "cacheable" matrix, caching it for later (and using the cache when possible)
cacheSolve <- function(x, ...) {
  ## First try to look up the cached inverse
  xinv <- x$getinv() # look up the inverse, which might be null
  if(!is.null(xinv)) { # if it's not null, return it
    message("getting cached inverse")
    return(xinv)
  }
  ## If that doesn't work, calculate it fresh
  message("calculating fresh inverse and storing in cache")
  data <- x$get() # this get the value of the actual matrix
  xinv <- solve(data, ...) # this does the inverse computation
  x$setinv(xinv) # have to store the inverse for later
  xinv # then return it
}
