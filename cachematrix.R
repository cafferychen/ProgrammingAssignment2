## The first function in the file, makeCacheMatrix() creates an R object that stores a squared matrix and its inverse. 
## The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() in order to retrieve the 
## inverse matrix from the cached value that is stored in the makeCacheMatrix() object's environment.

## Return an R object containing four functions: 
## 1.set(y): input the matrix value
## 2.get(): return the matrix
## 3.setinverse(inverse_m): input the inverse matrix of x
## 4.getinverse(): return the inverse matrix


makeCacheMatrix = function(x = matrix()){
      inver = NULL          # store the inverse matrix of x
      set = function(y){    # y is a squared matrix
            x <<- y 
            inver <<- NULL 
      }
      get = function() x
      setinverse = function(inverse_m) inver <<- inverse_m
      getinverse = function() inver
      
      list(set=set, get=get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## input an R object returned by makeCacheMatrix(), and then return the inverse matrix; if the object has stored the inverse
## matrix, use the cached value, if not, use solve() function to get the inverse function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse = x$getinverse()
      if(is.null(inverse) == FALSE){
            message('getting cached data')
            return(inverse)
      }
      da = x$get()
      inverse = solve(da, ...)
      x$setinverse(inverse)
      inverse
}
