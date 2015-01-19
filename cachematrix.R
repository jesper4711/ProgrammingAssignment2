## Put comments here that give an overall description of what your
## functions do

## This function generates the internal representation of our
## cache-enhanced matrix. The function implements get- and set-
## access to the internally stored inverse object. 
##
## example:
##   m<-matrix(c(1,2,3,4),nrow=2,ncol=2)
##   mc<-makeCacheMatrix(m)
##   i1<-cacheSolve(mc)
## to access the cached inverse, just call cacheSolve again
##   i2<-cacheSolve(mc)
## note the resulting commment stating the matrix inverse was
## not recomputed, but instead accessed from cache
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of a matrix, using the solve-
## function, unless it is found in the cache. If the inverse was 
## found in the cache, the inverse is not calculated and a message
## is passed stating that the inverse was retrieved from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
