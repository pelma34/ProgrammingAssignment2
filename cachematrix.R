## Caching the Inverse of a Matrix

##`makeCacheMatrix`: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setslove <- function(slove) s <<- solve
  getsolvefunction() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##`cacheSolve`: This function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. 

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
