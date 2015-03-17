## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. The two functions perform the caching of the 
## inverse of a matrix.
## Example test:
##   m <- matrix(c(-1, -2, 1, 1), 2, 2)
##   x <- makeCacheMatrix(m)
##   x$get()
##   inv <- cacheSolve(x)
##   inv
##   inv <- cacheSolve(x)

## makeCacheMatrix
## Description: Provides a list of functions that permit reading and 
##              writing of a matrix and its inverse.
## Usage: c <- makeCacheMatrix(m)
## Arguments: m = a matrix that is assumed to be invertible
## Details: The functions that are provided are the following:
##   * c$get(): returns the stored matrix that is provided during initialization or by
##              calling c$set(m)
##   * c$set(m): stores a matrix to the cache 
##   * c$getinverse(): returns the matrix that is stored with c$setinverse(m)
##   * c$setinverse(m): store the inverse of a matrix to the cache
makeCacheMatrix <- function(m = matrix()) {
  ## initialize cached matrix
  inv <- NULL
  ## store a new matrix and clear cached inverse
  set <- function(m) {
    m <<- m
    inv <<- NULL
  }
  ## return the target matrix
  get <- function() m
  ## store the inverse matrix
  setinverse <- function(m) inv <<- m
  ## return the inverse matrix
  getinverse <- function() inv
  ## prepare a list with the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## Description: Returns the inverse of a matrix
## Arguments: x = the list of functions returned by makeCacheMatrix() function
## Details:  If the inverse has already been calculated (and the matrix has not changed), 
## then the function retrieves the inverse from the cache. Otherwise, performs the 
## rather costly computation and stores the result to the cache
cacheSolve <- function(x, ...) {
  ## Read from cache
  invx <- x$getinverse()
  ## If found, display a message
  if(!is.null(invx)) {
    message("getting cached data")
  }
  ## Else if not found
  else {
    ## Get the matrix
    matrix <- x$get()
    ## Do the computation
    invx <- solve(matrix)
    ## Store the result to cache
    x$setinverse(invx)
  }
  ## Return the inverse matrix
  invx
}
