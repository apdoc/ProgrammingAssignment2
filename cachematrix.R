# makeCacheMatrix function creates a lists of functions which:
# 1) set: sets the value of a matrix
# 2) get: gets the value of a matrix
# 3) set_inverse: sets the inverse of a matrix
# 4) get_inverse: gets the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL                                       # i and x must be defined inside the function to stop the <<- operator                                                                              
        set <- function(y) {                            # in the set function from creating i & x in the global environment
                x <<- y                                 
                i <<- NULL
        }
        
        get <- function() x
        set_inverse <- function(inverse) i <<- inverse
        get_inverse <- function() i
        
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


# cacheSolve computes the inverse of a matrix, 
# or collects it from memory if the calculation has been done before.

cacheSolve <- function(x, ...) {
        
        i <- x$get_inverse()
        
        if(!is.null(i)) {                               # The data is obtained from memory ending the function by
                message("obtaining cached data")        # returning the inverse matrix if previously calculated
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$set_inverse(i)
        
        i
}

# An example of the working code is shown below:

# > z <- makeCacheMatrix(matrix(1:4, 2, 2))
# > cacheSolve(z)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# If cacheSolve(z) is called a second time, then it displays a message
# stating that the value is 'obtained from cached data' and the inverse
# matrix is not computed again, but obtained straight from memory

# > cacheSolve(z)
# obtaining cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5