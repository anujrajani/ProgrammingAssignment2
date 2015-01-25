## Following functions calculate inverse of a matrix and cache it for effectiveness

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <-function(y){
    x<<-y
    inv<<-NULL
  }
  get<- function() x
  setInverse <- function(inverse) inv<<-inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Get inverse from cache for a given matrix
    inv <- x$getInverse()
    
    ## if inverse of the matrix already exists in cache return the above fetched inverse
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    
    ## get the data i.e. matrix in this case
    data <- x$get()
    
    ## calculate inverse for the above fetched data/matrix via solve()
    inv <- solve(data)
    
    ## set the calculated inverse in cache
    x$setInverse(inv)
    inv
}