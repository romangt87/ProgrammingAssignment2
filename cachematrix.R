#The first function, makeCacheMatrix creates a special "matrix", 
#which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of the matrix
#get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse_matrix <<- inv
  getInverse <- function() inverse_matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function returns the inverse of the matrix that is introduced. First we check
# if the inverse has already been calculated. If yes, the function return the result and do not
# make the calculation. If not, the function calculates the inverse of the matrix and keep
# the value in the cache trougth the setInverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getInverse()
  
  if(is.null(inverse_matrix)) {
    print("First time we calculate inverse matrix...")
    
    matrix2 <- x$get()
    
    inverse_matrix2 <- solve(matrix2)
    x$setInverse(inverse_matrix2)
    return(inverse_matrix2)
    
  } else {
    print("We retrieve the calculated inverse matrix...")
    
    return(inverse_matrix)
  } #end if
} #end function
