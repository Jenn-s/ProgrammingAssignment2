## Assignment 2 - R (august, 2014).
## This function calculates the inverse of a matrix, 
## and saves it in a cache. If the matrix remains
## unchanged, the cache inverse is used and not computed
## again to decrease computational costs.

## This function creates a special 'matrix' object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      set <- function(y) {
            data <<- y
            m <<- NULL
      }
      get <- function() {
            matequal <- function(x, y) {
                  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
            }
            if(!exists("data") | !matequal(x,data)){
                  data <- x
                  m <<- NULL
            }
            data
      }
      setinverse <- function(solve) m <<- solve
      getinverse <- function() {
            if(!exists("m")) {
                  m <- NULL     
            }
            m 
      }
      list2use <- list(set = set, get = get, 
                       setinverse = setinverse, 
                       getinverse = getinverse)
}

## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the 
## cache.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      data <- x$get()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      message("calculating matrix inverse")
      m <- solve(data,...)
      x$setinverse(m)
      m      
}
