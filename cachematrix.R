## Put comments here that give an overall description of what your
## functions do

## makecacheMatrix builds a list of functions  

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        #input a  new matrix by calling $set on the object
        set <- function(y) {
                x <<- y
                #put inverse back to null
                inv <<- NULL
        }
        #retrieve value of matrix
        get <- function() x
        #set inverse in cache
        setInv <- function(inverse) inv <<- inverse
        #retrieve inverse from cache
        getInv <- function() inv
        #return list of functions
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}


# cacheSolve function takes an argument x (makeCacheMatrix object) and checks if it was already calculated by calling
# getInv(). If cache has a value stored, the cacheSolve function returns 

cacheSolve <- function(x, ...) {
       
        #get cached value if exists from makeCacheMatrix 
        inv <- x$getInv()
        #check if it already exists
        if(!is.null(inv)) {
                message("getting cached data")
                #if it exists -> return cached value
                return(inv)
        }
        
        #if matrix inverse isn't cached, get matrix object
        data <- x$get()
       
        # compute matrix inverse
        inv <- solve(data, ...)
        # set computation in cache object
        x$setInv(inv)
        # return inverse of matrix
        inv
        
}
