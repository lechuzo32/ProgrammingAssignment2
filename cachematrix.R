## makeCacheMatrix
# This function creates a special "matrix", which is a list containing a menu to:

# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse of the matrix using the solve() function
# 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    # Defining "get" function
    get <- function() x
    # Defining "setinverse" function
    # Solve is an existing fucntion on R
    setinverse <- function(solve) a <<- solve ## <<- it's de Cache operator
    # Defining "getinverse" function
    getinverse <- function() a

    # Menu list
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve
# This function cimputes de inverse if the special "matrix" created with
# the makeCacheMatrix function. 
# First of all, the function checks if the inverse has been computed. In that
# case, the function gets the inverse from Cache and omits the calculation.
# If the inverse has not been computed, the function computes de inverse matrix
# and save in cache using the existing function Solve().

cacheSolve <- function(x, ...) {
        ## Returns the inverse matrix of 'x'
        a <- x$getinverse()
        
        ## Checks if somthing has been changed
        if(!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        
        # Defining "get" function
        b <- x$get()
        
        # Defining "setinverse" function
        # Solve is an existing fucntion on R
        a <- solve(b, ...)
        x$setinverse(a)
        
        # Returns inverse matrix
        a
}
