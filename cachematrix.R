## makeCacheMatrix function creates a spectial matrix object
## which is able to contain its inverse matrix
## The function takes a matrix as argument
## The function returns a list of methods which may be used to:
## - get the matrix (get), set a new matrix (set)
## - store or read the inverse matrix in cache (setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
      
      inverse_matrix <- NULL
      set <-function(y) {
            x <<- y
            inverse_matrix <<- NULL
      }   
      get <- function () x
      setinverse <- function(inverse) inverse_matrix <<-inverse
      getinverse <- function() inverse_matrix
      list(set=set, get=get,
           setinverse=setinverse,
           getinverse=getinverse)
}



## cacheSolve takes a special matrix object as argument
## The function reads the inverse matrix stored in the matrix
## object cache. If no value stored, the inverse matrix is calculated
## using the solve function. 
## The function returns the inverse matrix

cacheSolve <- function(x, ...) {
      inverse_matrix <- x$getinverse()
      if(!is.null(inverse_matrix)) {
            message("getting cached data")
            return(inverse_matrix)
      }
      matrix<-x$get()
      inverse_matrix<-solve(matrix)
      x$setinverse(inverse_matrix)
      inverse_matrix      
}
