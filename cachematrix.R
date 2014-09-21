## Put comments here that give an overall description of what your functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL         ##initial setting inverse null
      
      set <- function(y) { ## get new matrix and reset inverse to null
            x <<- y
            inv <<- NULL
      }
      get <- function() { x }
      setinv <- function(inverse) { inv <<- inverse }
      getinv <- function() { inv }
      
      list(set = set, get = get, setinv = setinv, getinv = getinv)  
      ##result of this function is a list containing four funtions
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
      ## Return a matrix that is the inverse of 'x'
}


