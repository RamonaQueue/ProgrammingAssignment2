## makeCacheMatrix and cacheSolve work in tandem to store a matrix and its inverse.
## The inverse of the matrix can therefore be called repeatedly without having
## to recompute the matrix's inverse each time.
## Note: This code has been tested on several matrices and returns the correct
## inverse if one exists, and returns the appropriate error
## when the matrix is singular and cannot be inverted.

## makeCacheMatrix takes as input a matrix. 

makeCacheMatrix <- function(x = matrix()) {
   n <- NULL
   set <- function(y) {
       x <<- y
       n <<- NULL
   }
   get <- function() x
   ## Set n as a global variable
   ## Note that "solve" is the built-in R function for getting the inverse of a matrix
   setmatrixinverse <- function(solve) n <<- solve
   getmatrixinverse <- function() n
   ## store the functions
   list (set = set, get = get, setmatrixinverse = setmatrixinverse,
         getmatrixinverse = getmatrixinverse)
  

}

## cacheSolve calls the functions that have been stored in makeCacheMatrix.
## It begins by subsetting the list of functions to get the inverse of the matrix.
## If this has aleady been computed, it returns it --- this is where the cache comes in ---
## otherwise it computes the inverse and returns it.


cacheSolve <- function(x = matrix(), ...) { 
  ## Return a matrix that is the inverse of the matrix x
  n <- x$getmatrixinverse()
  if (!is.null(n)){
       message("getting cached matrix inverse data")
       return(n)
  }
  matrix <- x$get()
  n <- solve(matrix)
  x$setmatrixinverse(n)
  n
}



