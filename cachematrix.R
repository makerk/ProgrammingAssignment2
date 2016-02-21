## x: a square invertible matrix
## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
        ## <<- is being used to assign a value to an object in an environment different from the current enviornment
        
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(Inverse) Inv <<- Inverse
        getInv <- function() Inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## ## x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        Inv <- x$getInv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        matrix.data <- x$get()
        Inv <- solve(matrix.data, ...)
        x$setInv(Inv)
        return(Inv)

        
        
}
