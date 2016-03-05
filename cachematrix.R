## makeCacheMatrix provides four functions:
## set - to set the value of matrix.
## get - to get the value of matrix.
## setinverse - to set the value of inverse.
## getinverse - to get the value of inverse.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<- function(y) {
    x<<-y
    i<-NULL
  }
  get<-function() x
  setinverse<- function(inverse) i<<- inverse
  getinverse<- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by the function makeCacheMatrix above. 
## If the inverse has already been calculated then the cacheSOlve function will retrieve this value from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data, ...)
  x$setinverse(i)
  i
}
