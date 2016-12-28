## Makes it possible to cache the computation of 
## calculating the inverse of a matrix by using
## lexical scoping.
## 
## Usage example:  
## 
##   my_matrix <- matrix( c(3,1,5,7,4,2,3,4,5), nrow=3, ncol=3)
##   cache_matrix <- makeCacheMatrix(my_matrix)
##   cacheSolve(cache_matrix)
##   
##   cacheSolve(cache_matrix)
##           will produce: getting cached data
 

## Creates a list objects that contains 4 functions
## that make it possible to get and set the matrix
## as well as the calculated invers of the matrix.
## The returned object contains the original
## matrix as well as the calculates inverse from the 
## time it was set by the cacheSolve finction.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Chechs if the inverse of the matrix has
## alredy been calculated. If not it calculates it
## and puts it into the special object created by the 
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
