## Inversing matrix is generally computationally intensive,
## so it's useful to have it cached rather than compute it everytime. 


## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv 
  list(set = set, get= get, setinverse= setinverse, getinverse= getinverse)
}


## the following function checks to see if the matrix inverse is already computed, 
## if yes, it retrieves from the cache memory with appropriate message.
## Else, it computes the inverse and returns the value. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)){
      message ("getting cached data")
      return(inv)
    }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}      
