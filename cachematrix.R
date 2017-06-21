## This function takes input of a matrix and inverse it.
## this does inverse the matrix

## this function 
## sets  the values for matrix
## gets the values for matrix
## sets value of inverse
## gets value of inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## initializing the matrix
  mv <- NULL
  
  ##setting constructor 
  set <- function(b){
    x <<- b
    mv <<- NULL
  }

  get <- function () x
  setinverse <- function (inverse) mv <<- inverse
  getinverse <- function () mv
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns a inverse matrix that is that is created in the makeCacheMatrix function.
## this inverses the matrix by solve function.

cacheSolve <- function(x, ...) {
        
  
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
