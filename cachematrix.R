## The below functions perform two activities. The makeCacheMatrix stores or caches the matrix. 
## Whereas the cacheSolve function checks to see if the inverse of a matrix exists in the cache and if it doesn't it caculates the inverse


# The function makeCacheMatrix is intended to cache (or store) a matrix, 
#ideally the inverse of a square matrix


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x #get the value
  setinv <- function(inv) m <<- inv #set as the inverse
  getinv <- function() m #get the inv as m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



# The fucntion cacheSolve is intended to either find the cache of a matrix if it exists, 
# otherwise find the inverse of a matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() #get inverse from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #return cached inv 
  }
  data <- x$get()
  m <- solve(data, ...) #solve for inverse if not there
  x$getinv(m) 
  m
}
