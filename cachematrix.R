## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
                        inv <- NULL
                set <- function(y) {
                                            x <<- y
                                                  m <<- NULL
                                         }
                          get <- function() x
                          setinverse <- function(inverse) inv <<- inverse
                          getinverse <- function() inv
                          list(set = set,
                               get = get,
                               setinverse = setinverse,
                               getinverse = getinverse)
                  }
                            
     ## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above.                    
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
                         inv <- x$getinverse()
  #If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.    
                          if(!is.null(inv)) {
                                                  message("getting cached data")
                                                  return(inv)
                                          }
                          data <- x$get()
                          inv <- solve(data, ...)
                          x$setinverse(inv)
                         inv
                  }
