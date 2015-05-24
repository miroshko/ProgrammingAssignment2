## prepares a matrix for being passed to the cacheSolve function
## the function returns a list which is a storage for the original matrix and the result of the inversion
## the list contains 4 functions:
## set
## get
## getSolved
## setSolved

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        
        setSolved <- function(solved) s <<- solved
        getSolved <- function() s
        
        list( set = set,
              get = get,
              setSolved = setSolved,
              getSolved = getSolved )
}


## returns result of the solve function and stores it in the internal cache for immediate returning
## in case of further calls with the same arguments 

cacheSolve <- function(x, ...) {
        m <- x$getSolved()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolved(m)
        m
}

# Aample usage:
# 
# creating a 1000 x 1000 random matrix.
# (There is though a slight chance that a random matrix is irrevertible, in this case just repeat) 
sampleMatrix <- matrix(runif(1000000, 0, 100), 1000, 1000)
#
# preparing the matrix for cacheSolve
sampleCacheMatrix = makeCacheMatrix(sampleMatrix)
#
# perform inversion on the same data 2 times and measure time of both calls
# should take long, ~ 8 sec on my PC
ptm <- proc.time()
solved <- cacheSolve(sampleCacheMatrix)
diff <- proc.time() - ptm

print(diff['elapsed'])

# should take less
ptm <- proc.time()
solved <- cacheSolve(sampleCacheMatrix)
diff <- proc.time() - ptm

print(diff['elapsed'])