## This function, makeCacheMatrix creates a list containing a function to 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m  <- NULL
  set <- function(y) {
    x <<- y
    m <- NULL
  }
  get  <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}


## This function cacheSolve computes the inverse of the matrix  
## returned by the above function after checking
## if it has already been computed. If it has already computed, 
## it skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
