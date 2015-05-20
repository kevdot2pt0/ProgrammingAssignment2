## The first function takes as a default argument an empty matrix x
## The output of the function is a list of functions to
##  1.  Set the value of the matrix  (set())
##  2.  Get the value of the matrix  (get())
##  3.  Set the value of the matrix inverse  (setinv())
##  4.  Get the value of the matrix inverse  (getinv())

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
}


## This function will check if the matrix inverse has been calculated and 
## returns it if stored
## If the inverse has not been calculated, the function will get the data and compute it.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m     ## Return a matrix that is the inverse of 'x'
}
