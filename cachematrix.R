## This program creates a matrix in memory and if it computes an
## inverse, the inverse is stored in the object too

## this create's a matrix object with get/set methods

makeCacheMatrix <- function(x = matrix()) {

   ix <- NULL
   set <- function(y){ x <<- y ; ix <<- NULL }
   get <- function() x
   setinv <- function(iy) ix <<- iy
   getinv <- function() ix

   #extention to assignment - put function within object
   cacheSolve <- function() {
      if(!is.null(ix)){
         message("getting cached data")
         return(ix)
      }
      ix  <<- solve(x)
      ix
   }

   list(set=set, get=get, setinv=setinv, getinv=getinv, cacheSolve=cacheSolve)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
   ix <- x$getinv()
   if(!is.null(ix)){
      message("getting cached data")
      return(ix)
   }
   data <- x$get()
   ix <- solve(data,...)
   x$setinv(ix)
   ix
}

mtx3 <- makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
mtx3$cacheSolve()
cacheSolve(mtx3)
mtx3$get() %*% mtx3$getinv()
mtx3$get() %*% cacheSolve(mtx3)

