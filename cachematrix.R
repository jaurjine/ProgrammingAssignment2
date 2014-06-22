## makeCacheMatrix and cacheSolve compute the inverse of an invertible matrix. If the inverse
## has already been computed, it is stored in the cache (the variable m) and is simply printed, 
## not calculated again.

## makeCacheMatrix takes a matrix as input and creates a cache of this matrix's inverse
## using the setinverse function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## m is created in the environment of this function.
  set <- function(y) ## set allows the matrix to be explicitly stated by the user using makeCacheMatrix$set(matrix). 
    x <<- y          
  m <<- NULL  ##   When the matrix value is set, the cache is automatically nulled in the global environment.
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## cacheSolve takes the list created by makeCacheMatrix as input and determines 
## if the inverse has already been calculated (ie if m is not null). If so, it prints  
## the cache. If not, it uses the solve() function to calculate the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() ## Get the value of m from makeCacheMatrix
  if(!is.null(m)) { ## if m is not null (ie if the inverse has been calculated before)...
    message("getting cached data")
    return(m) ##...return the value of m
  }
  data <- x$get() ## if m is null, get the value of matrix, and...
  m <- solve(data, ...) ## ...calculate the inverse, assigning the result to m 
  x$setinverse(m) ## update the value of m so that the next time x$getinverse() is called it does not return NULL.
  message("calculating")
  m ## print the value of m
}
