## in this code inverse matix in cache is reset to NULL everytime makeCacheMatrix
## runs 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  sets <- function(y) {
    x <<- y
    m <<- NULL
  }
  gets <- function() x
  
    
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(sets = sets, gets = gets,
       setsolve = setsolve,
       getsolve = getsolve)
}

## create inverse matrix if no matrix is stored in cache
## 
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$gets()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}

