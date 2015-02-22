# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a matrix and a cached value of the inverse of the matrix
# Contains the following functions:
# setMatrix  -    to set the value of a matrix
# getMatrix  -   to get the value of a matrix
# cacheInverse -   to find inverse of the matrix
# getInverse  -   to get the cached value (inverse of the matrix)

#cacheSolve is a function that computes the inverse of matrix created using makeCacheMatrix function
#It checks if inverse exists and then computes it if not present
#It conatins only one function for computing inverse: solve()

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # hold the cached value or NULL if nothing is cached
    cache <- NULL
        
  # Store the matrix
    setMatrix <- function(newValue) {
            x <<- newValue
            # since the matrix is assigned a new value, clear cache
            cache <<- NULL
    }
  # Get the matrix
     getMatrix <- function() {
            x
        }
  # Find the inverse of matrix 
     cacheInverse <- function(solve) {
            cache <<- solve
        }

  # Get the cached value
     getInverse <- function() {
            cache
        }
        
  # Return a list of functions
     list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}



# The following function calculates the inverse of a "special" matrix created with the above function-makeCacheMatrix
cacheSolve <- function(y, ...) {
     # Get the Inverse Matrix
       inverseMat <- y$getInverse()
     # Check if Inverse matrix exists retrieve it
        if(!is.null(inverseMat)) {
              message("getting cached data")
              return(inverseMat)
        }
        
     # Compute the inverse matrix if it doesn't exist and store it in cache
        data <- y$getMatrix()
        inverseMat <- solve(data)
        y$cacheInverse(inverseMat)
        inverseMat
 }
}
