## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The function makeCacheMatrix is intended to cache (or store) a matrix, 
#ideally the inverse of a square matrix


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


## Write a short comment describing this function
# The fucntion cacheSolve is intended to either find the cache of a matrix if it exists, 
# otherwise find the inverse of a matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$getinv(m)
  m
}
