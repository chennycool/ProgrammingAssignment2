## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly 
## These are a pair of functions that can cache the inverse of a matrix.

## 'makeCacheMatrix' creates a list containing 
## 1. 'set': set the value of the square matrix
## 2. 'get': get the value of the square matrix
## 3. 'setinverse': set the value of the inverse
## 4. 'getinverse': get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
                   m <- NULL
                   set <- function(y) {
                     x <<-y
                     m <<-NULL
                   }
                   get <- function() x
                   setinverse <- function(inverse=matrix()) m <<- inverse
                   getinverse <- function () m
                   list (set=set, get=get,
                         setinverse=setinverse,
                         getinverse=getinverse)

}


## 'cacheSolve' calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it `get`s the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
              mi <- x$getinverse()              
              if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
              }
              data <- x$get()
              mi <- solve(data, ...)
              x$setinverse(mi)
              mi
  
        ## Return a matrix that is the inverse of 'x'
}


