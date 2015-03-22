makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  set <- function(y) {
      cacheMatrix <<- y
      cacheInv <<- NULL
  }
    get <- function() cacheMatrix
    setInv <- function(myInv) cacheInv <<- myInv
    getInv <- function() cacheInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special cached "matrix" 
  ## returned by `makeCacheMatrix`. If the inverse has already been calculated
  ## (and the matrix has not been changed), then `cacheSolve` should retrieve 
  ## the inverse from the cache.
  myMatrix <- makeCacheMatrix()$get()
  myInv <- makeCacheMatrix()$getInv()
  if(!is.null(myInv) & identical(myMatrix, x)) {
      message("getting cached data")
      return(myInv)
  }
  message("making cached data")
  makeCacheMatrix()$set(x, ...)
  myInv <- solve(x, ...)
  makeCacheMatrix()$setInv(myInv)
  myInv
}
