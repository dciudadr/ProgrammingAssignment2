## makeCacheMatrix gets a matrix and creates and object that stores the matrix 
## itself and its inverse. Both can be recovered using 
## objetc$get() and object$getinverse() respectively.
## Once the object is created with makeCacheMatrix, the stored inverse is NULL. 


makeCacheMatrix <- function(x = matrix())   m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve is a function that allows to calculate the inverse of the matrix  
## stored in an object created with makeCacheMatrix. After calling cacheSolve,
## the inverse is stored in such an objectx.
## Caution: The matrix is supposed to be invertible (determinant != 0)

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get() 
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
  
