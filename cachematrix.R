## Demo of how one can cache the inverse of a matrix

## produces a "matrix" that can cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_ <- NULL
  set <- function(y) {
    x <<- y
    inverse_ <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_ <<- inverse
  getinverse <- function() inverse_
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Takes a "cacheable" matrix from the above function and returns its inverse from 
## cache if it has already been computed once, otherwise calculates and caches it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  originalMatrix <- x$get()
  inverse <- solve(originalMatrix, ...)
  x$setinverse(inverse)
  inverse
}

# demo
M <- matrix(1:4, 2, 2)
cacheableM <- makeCacheMatrix(M)

cacheSolve(cacheableM)
cacheSolve(cacheableM)
