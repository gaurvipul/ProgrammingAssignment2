###########################################################################
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. Below 2 functions are used to cache the inverse of a matrix.
# The first function, `makeCacheMatrix` creates a special "matrix" object
# that can cache its inverse.
#
# 1.  set() <- sets the value of the matrix
# 2.  get() <- gets the value of the matrix
# 3.  setinverse() <- sets the value of inverse of the matrix
# 4.  getinverse() <- get the value of inverse of the matrix
###########################################################################

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#########################################################################
##  This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` will retrieve the inverse from the cache.
# 
# solve() is the suggested function in the assignment but when trying to
# invert 3 by 3 matrix, solve() throws an error
# 
# As a workaround, using ginv() function will solve the issue.
# require(MASS) - To load mass package
# inv <- ginv(data) - ginv in place of solve
#########################################################################

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data) ## inv <- ginv(data) 
        x$setinverse(inv)
        inv
}


#########################################################################
# Run Log
#########################################################################
# > mx <- rbind(c(9,99),c(999,9999))
# > m <- makeCacheMatrix(mx)
# > m$get()
# [,1] [,2]
# [1,]    9   99
# [2,]  999 9999
# 
# > ## As it is first run "cacheSolve" will not fetch the matrix from cache
# > cacheSolve(m)
# [,1]         [,2]
# [1,] -1.1222222  0.011111111
# [2,]  0.1121212 -0.001010101
# 
# > ## Second run, will fetch the matrix from cache
# > cacheSolve(m)
# getting cached data.
# [,1]         [,2]
# [1,] -1.1222222  0.011111111
# [2,]  0.1121212 -0.001010101
#########################################################################