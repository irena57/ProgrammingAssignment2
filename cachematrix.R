## Programming Assignment 2 of the R programming course:

   ## Function "makeCacheMatrix" creates a special matrix and cashes its inverse.
   ## Function "cacheSolve" calculates the inverse of this matrix or returns
   ## its cached value if it was calculated before and the matrix didn't change.

## You can run this file (source("cachematrix.R")) and see the examples!

###########################################################################

# Function makeCacheMatrix creates a "special matrix" containing a function 
# - setting the value of the matrix
# - getting the value of the matrix
# - setting the value of the inverse of the matrix and caching it
# - getting the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
	      set <- function(y) {
              	 	x <<- y 
      	       	m <<- NULL
	 	}
      	get <- function() x
			nrows <- nrow(x)
			ncols <- ncol(x)
	if(nrows==ncols) { # Inverse can be calculated only for a square matrix
		setinverse <- function(solve) m <<- solve
	} else {
		cat("\n Matrix is non-square and cannot be inverted \n")
		setinverse <- NA
	}
        	getinverse <- function() m
      list(set = set, get = get, 
	     setinverse = setinverse, getinverse = getinverse)
}


###########################################################################

## Function cacheSolve calculates the inverse of this matrix passed via x, 
## but only if the inverse has not been calculated before; in case it was, 
## it takes the inverse from the cache and saves the time of re-computation.

cacheSolve <- function(x, ...) {
        matr_inv <- x$getinverse()
        if(!is.null(matr_inv)) {
                message("getting cached data")
                return(matr_inv)
        }
        data <- x$get()
        matr_inv <- solve(data, ...)
        x$setinverse(matr_inv)
        matr_inv
}


###########################################################################

# Examples of input and output of above functions
#
# 1) First call of functions
   matr <- matrix(runif(9),3,3)            # Setting an example matrix
   matr_cached <- makeCacheMatrix(matr)    # Forming it into list with inverse and cache
   matr_inverse <- cacheSolve(matr_cached) # Calculating its mean or taking it from cache
   cat("\n --- First test: matr * matr_inverse is (must be unit matrix) =\n")
   print(round(matr %*% matr_inverse))
#
# 2) If cacheSolve is called again WITH THE SAME(unchanged) matrix, 
# then it produces the message "getting cached data" and 
# matr_inverse is not recalculated, but the cached value is used
	cat("\n --- Second run of cacheSolve with the same matrix \n")	
	cacheSolve(matr_cached) 
	cat("\n --- One more run of cacheSolve with the same matrix \n")	
	cacheSolve(matr_cached) 
#
# 3)If, however, matr is reset and makeCacheMatrix is called again, 
# the inverse is recalculated!
	matr <- matrix(runif(9),3,3)            # Setting a new example matrix
	matr_cached <- makeCacheMatrix(matr)    
   	matr_inverse <- cacheSolve(matr_cached) 
	cat("\n --- New test with new matrix: matr * matr_inverse is (must be unit matrix) =\n")
	print(round(matr %*% matr_inverse))
#
# 4) Test with non-square matrix - must tell it can't calculate the inverse
	cat("\n --- One more test with new NON-SQUARE matrix:") 	
	matr <- matrix(runif(12),3,4)
	matr_cached <- makeCacheMatrix(matr) 
	if (!is.na(matr_cached$setinverse)) {
		matr_inverse <- cacheSolve(matr_cached) 
	} else {
		cat(" Hence I don't call cacheSolve function \n\n")
	}

###########################################################################