## This file has a set of 2 functions that allows to cache a matrix and claculate its
## inverse from cache

## Function 1: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix = function(x = matrix())
        {inv = NULL
         set = function(y)
            {x <<- y;
             inv <<- NULL
            }
         get = function() x
         setinv = function(inverse) inv <<- inverse
         getinv = function() inv
         list(set = set, get = get, setinv = setinv, getinv = getinv)
        }

## Function 2: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve = function(x,...)
        {inv = x$getinv()
         if(!is.null(inv))
            {message("getting cached data")
             return(inv)
            }
         data = x$get()
         inv = solve(data,...)
         x$setinv(inv)
         inv
        }