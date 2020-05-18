##Create function that crease special 'matrix' that can cache the inverse
##@parameters x A matrix
##@return A list containing four functions to set and get the value of the 
##matrix and to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        ## Define the function to set the value of the matrix. It also clears 
        ## old inverse from the cache
        
        set <- function(y) {
                x <<- y ##Set the value
                m <<- NULL ##Clear the cache
        }
        ##Define function to get the value of the matrix
        get <- function() x
        ##Define function to set the inverse. This is oly used by get inverse()
        ##when there is no cached inverse
        setInverse <- function(inverse) m <<-inverse
        ## Define function to get the inverse
        getInverse <- function() m
        
        ##Return the list of the four functions
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


##Return inverse of matrix X

##This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix. If the inverse has already been calculated (and the 
##matrix has not been changed). Then cacheSolve retrieves the inverse of
##cache

##parameter- a special matrix created makeCacheMatrix

# at return the inverse of the matrix 

cacheSolve <- function(x) {
        m <- x$getInverse() ## This fetches the cached value of the inverse
        if(!is.null(m)) { ##If the cache is not empty, return it with message 
                ## ("getting cached data")
                message("getting cached data")
                return(m)
        }
        ##The cache was empty. We need to calculate it, cache it, and then 
        ##return it.
        data <- x$get() ##Get value of the matrix
        m <-solve(data) ##Calculate the inverse
        x$setInverse(m) ##Cache the result
        m               ##Return the inverse
}

