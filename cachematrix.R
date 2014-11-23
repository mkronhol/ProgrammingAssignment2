##This function creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  get <- function() x
  getinv <- function() invx
  setinv <- function(invMatToBeSet) invx <<- invMatToBeSet
  set <- function(newMat) {
    x <<- newMat
    invx <<- NULL
  }
  list(set = set, get = get, getinv = getinv, setinv = setinv)
}
## This function computes the inverse of the "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
  invx <- x$getinv()
  if(!is.null(invx)) {
    message("getting cached inverse matrix")
    return(invx)
  }
  newmatrix <- x$get()
  invx <- solve(newmatrix)
  x$setinv(invx)
  invx
  
  ## Return a matrix that is the inverse of 'x'
}
