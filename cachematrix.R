## makeCacheMatrix
# This function creates a special "matrix", which is a list containing a menu to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse of the matrix using the solve() function
# 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Initializing
    a <- NULL
    ## Defining a function to save values on Cache, special operator "<<-"
    set <- function(y) {
        ## Saving on Cache
        x <<- y
        ## Initializing
        a <<- NULL
    }
    
    # Defining "get" function
    get <- function() x
    # Defining "setinverse" function
    # Solve is an existing function on R
    setinverse <- function(solve) a <<- solve ## Remembre, the special operator "<<-"
    # Defining "getinverse" function
    getinverse <- function() a

    # Menu list, this four operations are used to work with the matrix x
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve
# This function computes de inverse if the special "matrix" created with
# the makeCacheMatrix function. 
# First of all, the function verifies if the inverse matrix has been computed. In that
# case, the function gets the inverse matrix from Cache and omits the "new" computation.
# If the inverse matrix has not been computed, the function computes the inverse matrix
# and save it on cache using the existing function Solve().

cacheSolve <- function(x, ...) {
    ## Returns the inverse matrix of x, usaing one of the operatios which are defined
    ## in the Menu list, on makeCacheMatrix
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

    ## Stores on Cache the computed value of the inverse matrx
    x$setinverse(a)

    # Returns inverse matrix
    a
}
