## Functions that enable caching of inverse of a matrix

## Create a special matrix object, with ability to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list (set = set, get = get, 
        setSolve = setSolve,
        getSolve = getSolve)

}


## If inverse already exists, return it.
## else calculate inverse, set and return it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data") 
    return(m)
  }
  # get the data
  data <- x$get()
  # Find the inverse
  message("solving inverse of matrix")
  m <- solve(data, ...)
  # Set value of x
  x$setSolve(m)
  # return
  m
}
