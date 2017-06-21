## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  mv <- NULL
  
  set <- function(b){
    x <<- b
    mv <<- NULL
  }

  get <- function () x
  setinverse <- function (inverse) mv <<- inverse
  getinverse <- function () mv
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  mv <- x$getinverse()
  if(!is.null(mv)){
    message("getting cached matrix")
    return(mv)
  }
  data <- x$get()
  mv <- solve(data, ...)
  x$setinverse(mv)
  mv
}
