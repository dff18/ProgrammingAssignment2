## The two functions below are complementary:
## they create a special matrix and cache the value of its inverse.

## The first function will create this special matrix.
## Its output is a list of 4 useful functions.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
  
        set <- function(a) {
                x <<- a
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(a) inv <<- a
        getinv <- function() inv
  
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The second function will return the inverse of the special matrix.
## If calculated previously, it will fetch the cached value of the inverse.
## If not, it will calculate and cache it.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("..getting cached inverse value..")
                return(inv)
        }
        inv <- solve(x$get(),...)
        x$setinv(inv)
        inv
}
