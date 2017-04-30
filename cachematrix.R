## This function provides functions to a particualar:
## - set - it sets the cache for the matrix
## - get - it gets the cache of a matrix
## - set - it sets matrix's inverse
## - get - it gests the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

  }		  


#The following fucntion createse the inverse of a matrix based on the fuction written above
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


#TEST (in this test there check of written code, which shows that it works)
hg <- matrix(rnorm(49),7,7)
test <- makeCacheMatrix(hg)
cacheSolve(test)

#Result
#[,1]         [,2]       [,3]        [,4]        [,5]       [,6]
#[1,] -0.12009395  0.282163661 -0.3508276 -0.30406293  0.22167101  0.2925418
#[2,] -0.99961588 -0.679339985  0.3883424  2.05557352 -0.66111222 -1.0609968
#[3,] -0.52464332 -0.559984315  0.4655793  1.23144368 -0.36165379 -1.7688495
#[4,] -0.07616729 -0.005083196  0.9137089 -0.13880202 -0.02758720 -0.8353309
#[5,]  0.01427884  0.024926741 -0.0225419 -0.28391272 -0.31020300 -0.6319318
#[6,]  0.06365874 -0.027621054  0.3089754  0.05751552 -0.01096344 -0.7876521
#[7,] -0.07332230  0.090031986 -0.3701921  1.34145872 -0.57162204  0.9341907
#[,7]
#[1,]  0.15194140
#[2,]  0.77745239
#[3,]  0.89338259
#[4,] -0.15031695
#[5,] -0.09342804
#[6,] -0.17787228
#[7,]  0.36646630
