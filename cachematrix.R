## Creates a new matrix with list of get and set functions

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  set <- function(y) {
        m <<- y
        inv <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Gets the inverse of a matrix from cache if already exists else finds inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- m$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinverse(inv)
  inv
}
