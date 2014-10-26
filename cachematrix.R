# This function creates a matrix object that can cache its inverse.
# First the function looks to see if the inverse has already been computed. 
# If it has, it returns that result. 
# If it has not been computed, it computes the inverse, sets the value in the cache via
# setinverse() function.

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


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Matrix identified; this has been solved previously. Getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

#Sample output: 
#> x=rbind(c(2,3),c(3,2))
#> m=makeCacheMatrix(x)
#> m$get()
#     [,1] [,2]
#[1,]    2    3
#[2,]    3    2
#> 
#> cachesolve(m)
#Error: could not find function "cachesolve"
#> cacheSolve(m)
#     [,1] [,2]
#[1,] -0.4  0.6
#[2,]  0.6 -0.4
#> 
#> cacheSolve(m)
#Matrix identified; this has been solved previously. Getting cached data.
#     [,1] [,2]
#[1,] -0.4  0.6
#[2,] 0.6 -0.4