## cashematrix.R consists of two functions makeCacheMatrix(x) and cacheSolve(x)
## to compute and save inverse of invertible matrices. makeCacheMatrix function 
## creates a list of functions that store in cache and retrieve from cache, 
## the matrix and its inverse. cacheSolve, returns the inverse of a given matrix. 
## It uses the inverse of a matrix from cache, if it is already computed and stored 
## using makeCacheMatrix. If the inverse has not been computed and stored 
## before, cacheSolve computes that and stores it in cache using 
## makeChacheMatrix.

## makeCacheMatrix is a function that creates a list of functions to 
## get the matrix value or store a matrix in Cache that can be used by other functions.
## It also adds two more functions to the list that stores inverse of a matrix in cache and retrieves
## the inverse.
makeCacheMatrix <- function(x = matrix()) {
    
        inv <- NULL        ## initialise the matrix inverse with an empty variable
        set <- function(y) {  ## set fuction store given data (matrix)  and empty inverse in cache envirenment.
            x <<- y
            inv <<- NULL
        }    
        get <- function() x ## get function prints the existing value of a matrix.
        setinv <- function(invrs) inv <<- invrs  ## setinv function stores the given inverse matrix of a matrix in cache.
        getinv <- function() inv   ## retrieves and prints the inverse of a matrix from cache
        list(set = set, get = get, ## a list of functions are created that consists of all the functions defined within makecacheMatrix
             setinv = setinv,
             getinv = getinv)
    }

    
## cacheSolve returns inverse of a given matrix. If the inverse is already stored 
## in cache in a list associated with this matrix, retrieves the inverse from cache.
## otherwise, it computes the inverse and stores that in cache in the list associated with this matrix
## using the list of fucntions defined for x using makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()  ## read the inverse of matrix x stored in cache
    if(!is.null(inv)) { ## if the inverse of the matrix is saved, return the message that data is retireved from cache and not computed and print the inverse.
        
        message("getting cached data")
        return(inv)
    }
    data <- x$get() ## retrieve value of the matrix x from cache
    inv <- solve(data, ...) ## calculate the inverse and store in inv locally
    x$setinv(inv)   ## store the inverse in cache using the function defined by makeCacheMatrix.
    inv  # print the inverse.
    
}
