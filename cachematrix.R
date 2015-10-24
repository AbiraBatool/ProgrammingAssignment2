## makeCacheMatrix provides a list with a function to
## Set Value of matrix and get value of a matrix
## Set and get Inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve() first determines whether inverse of inputted matrix 
## is in Cache or not. If it is, it is outputed from Cache. IF its not in 
## Cache then it calculates the inverse of matrix, output it and also place
## it in cache for further use


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Caching Inversed Matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}