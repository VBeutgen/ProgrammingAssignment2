## Programming Assignment 2





## makeCacheMatrix creates a set of functions and returns them to the parent environment as a list



makeCacheMatrix <- function(x = matrix()) { # x initialized as function object, default empty matrix
      invMat <- NULL # initializes object invMat in function environment
      
      set <- function(y) {
            x <<- y # assignes the value of y to x in parent environment
            invMat <<- NULL # assign value NULL to invMat in parent environment
      }
      
      get <- function() x # accesses x from parent environment
      setInv <- function(solve) invMat <<- solve # assigns input (solve) to invMat
      getInv <- function() invMat # access invMat and retrieve its value
      list(set = set, get = get,# creates named list, returned to parent environment
           setInv = setInv,
           getInv = getInv)
}



## Return a matrix that is the inverse of 'x'. First tries to access value from cache. If no value is stored, it is calculated and then cached

cacheSolve <- function(x, ...){ # requires input in form of makeCacheMatrix
      invMat <- x$getInv() # try to retrieve inverse matrix from object passed as argument
      if(!is.null(invMat)){ # if invMat is not Null, cached value can be returned to parent environment
            message("Retrieving data from cache")
            return(invMat)
      }
      data <- x$get() # if invMat is Null, matrix from input is accessed
      invMat <- solve(data, ...) # inverse matrix is calculated solve()
      x$setInv(invMat) # sets result in input object
      invMat # returns inverse matrix
}


