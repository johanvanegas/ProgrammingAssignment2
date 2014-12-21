## These functions allow the user caching the inverse of an input matrix
## (assumed always invertible), to save time and memory use

## The first function is called makeCacheMatrix. Its goal is taking an input
## matrix and creating a list with four elements:

## 1) 'set' allows to input a matrix without calling the function
## 2) 'get' prints the matrix
## 3) 'setinv' saves the inverse in the cache
## 4) 'getinv' prints the inverse

## Finally it prints the list

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second function is calles cacheSolve. This function examines if the list
## created with makeCacheMatrix has an inverse matrix saved in cache. If there 
## is one, the function returns the cached data with a message. If there isn't,
## the function calculates the inverse of the matrix and saves it in the list

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
