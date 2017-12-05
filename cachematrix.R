## The functions makeCacheMatrix and cacheSolve are used to calculate the inverse of a matrix and cache 
## it to save resources. Before the inverse is calculated cacheSolves checks that it is not already cached 
## in the special matrix object, this ensures that the calculation of the inverse matrix is only done once.
## 

## makeCacheMatrix creates the special matrix. The special matrix has four functions that can
## get and set the matrix itself as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv_x <- NULL
      set_matrix <- function(y){
          x <<- y
          inv_x <<- NULL
      }
      get_matrix <- function() x
      set_inv_matrix <- function(inv_matrix) inv_x <<- inv_matrix
      get_inv_matrix <- function() inv_x
      list(set_matrix = set_matrix, get_matrix = get_matrix, 
           set_inv_matrix = set_inv_matrix, get_inv_matrix = get_inv_matrix)
}


## This function operates on the special matrix created with makeCacheMatrix and returns the inverse
## of the matrix. If the inverse matrix is not yet in the special matrix, it is calculated by cacheSolve 
## and then stored in the special matrix object.
## If the inverse is already calculated and stored in the special matrix, cacheSolve is only returning it
## without calculating it again.

cacheSolve <- function(x, ...) {
      inv_x <- x$get_inv_matrix()
      if(!is.null(inv_x)){
          return(inv_x)
      }
      matrix <- x$get_matrix()
      inv_x <- solve(matrix)
      x$set_inv_matrix(inv_x)
      inv_x
}
