## Because matrix inversion is a costly computational operation, 
## cachematrix.R defines functions can be used to 1) cache the inverse of 
## a matrix and 2) retrieve from cache a previously computed calculation
##
## Assumption: input X is a square invertible matrix

## makeCacheMatrix constructs a matrix with methods to get and set the inverse of the parameter x
makeCacheMatrix <- function(x = matrix()) {
  ## initialize a NULL matrix for cache
  m <- NULL
  
  ## function resets cache to NULL and sets x to parameter matrix y
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## function gets input matrix after constructing object with makeCacheMatrix
  get <- function() x
  
  ## function sets matrix inverse into cache
  setinverse <- function(inverse) m <<- inverse
  
  ## function gets inverse matrix from cache
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the results of the matrix inversion
## if the inverse of the matrix was already calculated, the function
## returns the result from cache
cacheSolve <- function(x, ...) {
  ## extract matrix inverse for matrix parameter x
  m <- x$getinverse()
  
  ## checks cache to see if matrix inversion previously calculated
  if(!is.null(m)) {
    message("getting cached data")
    
    ## Return a matrix that is the inverse of 'x'
    return(m)
  }
  
  ## no previous calculation found
  
  ## initialize local variable with matrix inverse from parameter x
  data <- x$get()
  
  ## compute matrix inversion
  m <- solve(data, ...)
  
  ## assign the calculation to cache
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}