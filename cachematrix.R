## We're creating 2 functions to cache the inverse of a given matrix


## The function below will create a special matrix object able to cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## here argument default mode= "matrix"
    ## Initialize invr as NULL which will hold inverse of the matrix
    invr<- NULL

    ## set function to assign new
    set <- function( matrix ) {
            x <<- matrix
            invr<<- NULL
    }

    ## get function to return the value of matrix argument
    get <- function() {
    	## Return the matrix
    	x
    }

    ## This method is used to set the inverse of the matrix
    setInverse <- function(inverse) {
        invr<<- inverse
    }

    ## This method is used to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        invr
    }

    ## Used to refer to the functions with $ operator
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}


## This function computes the inverse of the special matrix returned by "makeCacheMatrix"
## In case the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x <- x$getInverse()

    ## Returns the inverse if its already set
    if( !is.null(invr) ) {
            message("getting cached data")
            return(invr)
    }

    ## Gets matrix from our object and store it
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    invr <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(x)

    ## Return the matrix
    invr
}
