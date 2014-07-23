# Name : makeCacheMatrix
# Description:
# This function creates a special "matrix" object that can cache its inverse
# Usage:
# > x <- matrix(c(3,1,2,1),nrow=2,ncol=2)  // Create a matrix x
# > b <- makeCacheMatrix()  // Call Special matrix Object function   
# > b$set(x) // Store x
# > b$get()
# [,1] [,2]
# [1,]    3    2
# [2,]    1    1 // Return matrix x

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

# Name : cacheSolve
# Description:
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# Usage:
# > cacheSolve(x) 
# > cacheSolve(b)
# [,1] [,2]
# [1,]    1   -2
# [2,]   -1    3  // Return the inverse (without Cache)
# > cacheSolve(b)
# ....getting cached data 
# [,1] [,2]
# [1,]    1   -2
# [2,]   -1    3   // Return the inverse (with Cache)


cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
    ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
