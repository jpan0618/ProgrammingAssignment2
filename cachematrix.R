## makeCacheMatrix creates a special matrix object
## cacheSolve calculates the inverse of the matrix 
## If the inverse of the matrix has been calculated already, it will find it in the cache and return, avoiding calculate it again. 
 

makeCacheMatrix <- function(x = matrix()) { 
   inv <- NULL 
   set <- function(y) { 
         x   <<- y 
         inv <<- NULL 
     } 
   get <- function() x 
   setinverse<- function(inverse) inv <<-inverse 
   getinverse<- function() inv

      list(set = set, get = get, 
           setinverse = setinverse, 
           getinverse = getinverse) 
} 

## The function cacheSolve returns the inverse of a matrix A created with the makeCacheMatrix function. 
## If the cached inverse is there, cacheSolve retrieves it; if not, it calculates, caches, and returns. 

cacheSolve <- function(x, ...) { 
       inv <- x$getinverse() 
          if (!is.null(inv)) { 
            message("getting cached inverse matrix") 
            return(inv) 
          } else { 
            inv <- solve(x$get()) 
            x$setinverse(inv) 
            return(inv) 
          } 
} 
