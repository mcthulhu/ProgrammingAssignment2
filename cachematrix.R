## These functions set up a cache for the inverse of a square matrix and return
## either the stored result, or the newly calculated inverse. Assume that the matrix supplied is always invertible.

## This function returns a labeled list of functions used to cache and retrieve the inverse of a matrix.


makeCacheMatrix <- function(x = numeric()) {
	  i <- NULL
        set <- function(y) {
		    # set x to argument y and initializes i to NULL in parent (global) environment
                x <<- y
                i <<- NULL
        }
	  # get returns value of x
        get <- function() x
	  # setinverse sets i in makeCacheMatrix to inverse
        setinverse <- function(inverse) i <<- inverse
	  # getinverse returns cached value of i 
        getinverse <- function() i
	  # return labeled list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)



}

## This function either retrieves the stored matrix, or calculates a new inverse matrix.

cacheSolve <- function(x, ...) {
	  # check to see if inverse is cached
	  i <- x$getinverse() 					
        if(!is.null(i)) {
                message("getting cached data")
		    # return the cached inverse if one exists
                return(i)					 
        }
	  # else call get to retrieve the argument, and store it in variable data
        data <- x$get()

	  # no need to test if matrix is square since the assignment says
	  # to assume an invertible matrix, which is square by definition 

	  # calculate the inverse matrix and store in i
        i <- solve(data, ...)
	  if(data %*% i  == 1) {
		message("product of matrices is an identity matrix")
	  } else {
		stop("product of matrices is not an identity matrix")
	  }
	  # store this inverse in the cache					
        x$setinverse(i)
        # return the inverse of the matrix
        i								
}
