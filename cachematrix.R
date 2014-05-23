## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.
## If the matrix is not changing, it may make sense to cache its inverse matrix so that when we need it again, it can be looked up in the cache rather than recomputed. 
##
## Example using the functions below in order to read the cached inverted matrix. 
## When assigning the variable m_inv the inverted matrix is calculated, and when assiging the variable m_inv2 the cached data is read:
##
## > v_data <- 1:4
## > dim(v_data) <- c(2, 2)
## > m <- makeCacheMatrix(v_data)
## > m_inv <- cacheSolve(m)
## > m_inv2 <- cacheSolve(m)
## getting cached data
## > 

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) inv_x <<- inv
    get_inv <- function() inv_x
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv) # return
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inv <- x$get_inv()
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data, ...)
    x$set_inv(x_inv)
    x_inv # return
}
