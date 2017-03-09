## The below functions will be used to cache the inverse of a matrix using R scoping rules.
## Author: Bryan Waldo

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
                        
                        i <- NULL
                        
                        # set the value of the matrix
                        set <- function(y){
                            x <<- y
                            i <<- NULL
                        }
                        
                        # get the value of the matrix
                        get <- function() x
                        
                        # set the value of the inverse
                        setInverse <- function(inverse) i <<- inverse    
                       
                         # get the value of the inverse
                        getInverse <- function() i
                       
                         # create a list of functions
                        list(set = set, get = get,
                             setInverse = setInverse,
                             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse
    
                        i <- x$getInverse()
                        
                        # check to see if the inverse is populated thus has been calculated, if so return i
                        if(!is.null(i)) {
                            message("getting cached data")
                            # get the inverse out of the cache and skip the computation
                            return(i)
                        }
                        
                        # if inverse has not been calculated then calculate the inverse
                        inv <- x$get()
                        
                        # use solve() to get the inverse of the matrix
                        i <- solve (inv, ...) # mean(data, ...)
                        
                        # use the function setinv to set the value of the inverse in the cache
                        x$setInverse(i)
                        
                        #returns a matrix that is the inverse of 'x'
                        return(i)
}
