## makeCacheMatrix creates a special Matrix object that can cache its inverse
## cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## The functions are written to apply the cache logic using lexical scoping for the invertible square matrices alone.

## makeCacheMatrix - creates an object for the given square matrix with four functions set, get, setinverse, getinverse where
## set can be used to change the given matrix to another square matrix,
## get cab be used to print the given matrix,
## setinverse can be used to change the inverse matrix for the given matrix,
## getinverse can be used to get the inverse matrix for the given matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y  = matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse = matrix()) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function  checks for the inverse of given matrix in Cache using getinverse function of makeCacheMatrix.
## if the matrix is a repitative one, then the the precalculated inverse will be retrieved from the cache.
## else NULL is returned indicating that the matrix is a new one,
## the new matrix is stored in the cache using the get function, inverse of the matrix is calculated using solve function
## the calculated inverse is stored in the cache using the setinverse function.

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
