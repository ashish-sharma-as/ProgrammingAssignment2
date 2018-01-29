# --------------------------------
# W3 - Programming assignment 2
# --------------------------------

# 2 functions have been written in this file. 
# These functions utilize the scoping rules in R to retrieve a cached object, 
# (in this case, the inverse of a matrix)
# to avoid repeat processing if this 
# has already been done in the past.

# The 2 functions interact with each other to check if a cached matrix inverse exists
#     If it exists, then the existing inverse is returned by the function
#     If it does not exist, then the solve() is used to newly compute the inverse
#         This newly computed inverse is sent to be stored in cache and also returned 

# The functions utilize the "<<-" operator to assign a value to an object in an environment
# that is different from the current environment   

# --------------------------------
## Function 1. makeCacheMatrix: 
# --------------------------------
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  # defining set() to set x to input matrix and inver to NULL:
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  # defining get() matrix function:
  get <- function() {
    x
  }
  # defining setinverse() function to assign a new value to inverse:
  setinverse <- function(inverse){
    inver <<- inverse                 # value that is passed when calling SetInverse is placed in main function return object
  }
  # defining getinverse() function to return the inverse:
  getinverse <- function(){
    inver
  }
  # Create a List of the 4 above:
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# --------------------------------
## Function 2. cacheSolve: 
# --------------------------------
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inver <- x$getinverse()
    # Checking if inver gets a NULL value from getinverse function
    # If it does return NULL, then an inverse matrix has not previously been cached
    # If it does not return NULL, then simply pass this as the inversed matrix retrieved from cache
    if (!is.null(inver)){
      message("getting cached data")
      return(inver)
    }
    matrix1 <- x$get()
    inver <- solve(matrix1, ...)    # using the solve() to calculate inverse of a matrix
    x$setinverse(inver)             # setting the inverse just calculated using solve above
    inver                           # returning newly calculated inverse
}
