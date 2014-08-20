# Assignment 2 in R example 
# https://github.com/rdpeng/ProgrammingAssignment2/blob/master/README.md
#

#In this example we introduce the <<- operator which can be used to assign
# a value to an object in an environment that is different from the current
# environment. Below are two functions that are used to create a special
# object that stores a numeric vector and caches its mean.
#
# The first function, makeVector creates a special "vector", which is really a list containing a function to

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y # Help on "<<" operator see here: 
				# http://127.0.0.1:19044/library/base/html/assignOps.html
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

###################################################
# The following function calculates the mean of the special "vector" 
# created with the above function. However, it first checks to see 
# if the mean has already been calculated. If so, it gets the mean from 
# the cache and skips the computation. Otherwise, it calculates the mean 
# of the data and sets the value of the mean in the cache 
# via the setmean function.

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

###################################################
# Example of input and output:
#
# 1) First call of functions
	v <- c(2,4,9)                 # Setting a numeric vector
	v_cached <- makeVector(v)     # Making a list out this vector, containing its mean and cache
	v_mean <- cachemean(v_cached) # Calculating its mean or taking it from cache
	cat("The mean of vector v is ",v_mean,"\n") # Must be (2+4+9)/3=5.0
#
# 2) If cachemean is called again WITH THE SAME(unchanged) v, 
# then it produces the message "getting cached data" and 
# v_mean is not recalculated, but the cached value is shown
#
# 3) If, however, v is reset and makeVector is called again, the mean is recalculated!
	v <- c(2,4,39)
	v_cached <- makeVector(v)
	v_mean <- cachemean(v_cached) # Must give (2+4+39)/3=15.0
   


