makeCacheMatrix <- function(x = matrix()) {
  invtemp <- NULL
  set <- function(y) {
    x <<- y
    invtemp <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invtemp <<- inverse
  getinverse <- function() invtemp
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
  invtemp <- x$getinverse()
  if(!is.null(invtemp)) {
    message("Data from Cahce")
    return(invtemp)
  }
  data <- x$get()
  invtemp <- solve(data)
  x$setinverse(invtemp)
  invtemp
}
