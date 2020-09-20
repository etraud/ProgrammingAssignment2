## Rodrigo Duarte's assignment on Lexical scoping
#
## There are two functions, based on the vector example: makeCacheMatrix,
## caches a matrix, and cacheSolve, which retrieves the cached value, or not

## makeCacheMatrix takes a matrix "x" as parameter and stores it 

makeCacheMatrix <- function(x = matrix()) {
        #creates the placeholder for the inverse
        matrixInverse <- NULL
        #sets the matrix
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL #initialized as null
        }
        #gets the matrix
        get <- function() x #returns the matrix
        setInv <- function(inverse)  matrixInverse <<- inverse #loads the matrix
        getInv <- function() matrixInverse
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve gets a matrix as a parameter, checks if there is an inverse matrix
## and decides whether to retrieve or calculate

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getInv()
        if (!is.null(invrs)) { #if there is something stored...
                message("getting cached data")
                return(invrs) #return the stored value
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}
#
#Let's test this out
#create vectors
v1 <- c(3, 0, 5) 
v2 <- c(2, 3, 0) 
v3 <- c(4, 2, 1) 

# ...and bind them
M <- rbind(v1, v2, v3)
##test the code
mk<-makeCacheMatrix(M)
cacheSolve(mk) #calculates
cacheSolve(mk) #uses cached value

