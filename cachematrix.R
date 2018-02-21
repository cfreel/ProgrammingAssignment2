## Utility functions to support caching of matrix inverse using the built-in solve().
## Useful if you have a large invertible matrix that cost a lot to invert and you need 
## to do it many times. Even more useful as a learning exercise for a few things!  :^)

## Function to create a caching wrapper that can store the 
## pre-computed inversion for future reuse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function to perform the actual matrix inversion;
## holds onto the result and returns it if called again.
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
