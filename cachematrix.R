## prepares a matrix for being passed to the cacheSolve function
## the function returns a list which is a storage for the original matrix and the result of the inversion
## the list contains 4 functions:
## set
## get
## getSolved
## setSolved

makeCacheMatrix <- function(x = matrix()) {
        # x is added to the lexical scope of the function and is available
        # for all function which are defined inside of makeCacheMatrix
        
        # initializing
        solution <- NULL

        # set a new matrix to the existing data structure
        setMatrix <- function(y) {
                # with <<- we access the x from the upper scope (makeCacheMatrix)
                x <<- y
                
                # clear the solution for the new matrix
                solution <<- NULL
        }
        getMatrix <- function() x
        
        setSolved <- function(solved) solution <<- solved
        getSolved <- function() solution
        
        list( setMatrix = setMatrix,
              getMatrix = getMatrix,
              setSolved = setSolved,
              getSolved = getSolved )
}


## returns result of the solve function and stores it in the internal cache for immediate returning
## in case of further calls with the same arguments 

cacheSolve <- function(x, ...) {
        # if solution is present return the result immediately,
        # otherwise perform calculation, save the solution and return the result
        m <- x$getSolved()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setSolved(m)
        m
}

#  # Sample usage (uncomment and run if needed):
#   
#  # creating a 1000 x 1000 random matrix.
#  # (There is though a slight chance that a random matrix is irrevertible, in this case just repeat) 
#  sampleMatrix <- matrix(runif(1000000, 0, 100), 1000, 1000)
#  
#  # preparing the matrix for cacheSolve
#  sampleCacheMatrix = makeCacheMatrix(sampleMatrix)
#  
#  # perform inversion on the same data 2 times and measure time of both calls
#  # should take long, ~ 8 sec on my PC
#  ptm <- proc.time()
#  solved <- cacheSolve(sampleCacheMatrix)
#  diff <- proc.time() - ptm
#  
#  print(diff['elapsed'])
#  
#  # should take less
#  ptm <- proc.time()
#  solved <- cacheSolve(sampleCacheMatrix)
#  diff <- proc.time() - ptm
#  
#  print(diff['elapsed'])