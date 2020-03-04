## makeCacheMatrix
# This function creates a special "matrix", which is a list containing a menu to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse of the matrix using the solve() function
# 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Initializing
    save_a <- NULL
    ## Defining a function to save values on Cache, special operator "<<-"
    set <- function(y) {
        ## Saving on Cache
        save_x <<- y
        ## Initializing
        save_a <<- NULL
    }
    
    # Defining "get" function
    get <- function() save_x
    # Defining "setinverse" function
    # Solve is an existing function on R
    setinverse <- function(solve) a <<- solve ## Remembre, the special operator "<<-"
    # Defining "getinverse" function
    getinverse <- function() save_a

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
    save_a <- x$getinverse()

    ## Checks if somthing has been changed
    if(!is.null(save_a)) {
            message("getting cached data")
            return(save_a)
    }

    # Defining "get" function
    save_b <- x$get()

    # Defining "setinverse" function
    # Solve is an existing fucntion on R
    save_a <- solve(save_b, ...)

    ## Stores on Cache the computed value of the inverse matrx
    x$setinverse(save_a)

    # Returns inverse matrix
    save_a
}
