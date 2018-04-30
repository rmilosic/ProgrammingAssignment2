## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       
        #get cached value if exists from makeCacheMatrix 
        inv <- x$getInv()
        #check if it already exists
        if(!is.null(inv)) {
                message("getting cached data")
                #if it exists -> return cached value
                return(inv)
        }
        
        #get input
        data <- x$get()
       
        #
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
        
}
