## There are four functions in this script that reduce computation by retrieving a mean stored in cache.
## makeCacheMatrix - creates a matrix object that can cache its inverse
## cachesolve - computes the inverse of a matrix unless the inverse is already stored in cache
## makevector - creates a vector and sets or gets the value or mean from that vector
## cachemean - retrieves a mean from cache if available, otherwise calculates the mean
## Some background explanation:  https://class.coursera.org/rprog-010/forum/thread?thread_id=364
##
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve function should retrieve the inverse from the cache.
makeCacheMatrix <- function(x = matrix()) {
        ## if matrix is already inverted and has not been changed then call cachesolve
        ## create a "matrix" object that can cache its inverse
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## cachesolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cachesolve retrieves the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R.
## if X is a square invertible matrix, then solve(X) returns its inverse.
cacheSolve <- function(x, ...) {
        ## inverse already created and matrix has not changed
        ## inverse not already created or matrix has changed
        ## Return a matrix that is the inverse of 'x' x <- solve(x)
        cacheSolve <- function(x=matrix(), ...) {
                m<-x$getmatrix()
                if(!is.null(m)){
                        message("getting cached data")
                        return(m)
                }
                matrix <- x$get() 
                m<-solve(matrix, ...)
                x$setmatrix(m)
                m
        }
        
}

## makeVector creates a special "vector", 
## which is really a list containing a function to
##     1) set the value of the vector
##     2) get the value of the vector
##     3) set the value of the mean
##     4) get the value of the mean
## <<- and ->> are normally only used in functions, and cause a search to made through 
## parent environments for an existing definition of the variable being assigned. 
## If such a variable is found (and its binding is not locked) then its value is redefined, 
## otherwise assignment takes place in the global environment. 
makeVector <- function(x = numeric()) {
        m <- NULL                       ## m holds the mean, set to null when starting
        set <- function(y) {            ## set value of vector
                x <<- y
                m <<- NULL
        }
        get <- function() x             ## get value of vector
        setmean <- function(mean) m <<- mean  ## set value of the mean
        getmean <- function() m               ## get value of the mean
        list(set = set, get = get,setmean = setmean,getmean = getmean)
}

## cachemean calculates the mean of the special "vector" created with 
## makevector above. cachemean first checks to see if the mean has already been
## calculated. If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in 
## the cache via the setmean function.

cachemean <- function(x, ...) {
        m <- x$getmean()   ## set m equal to the mean in list x
        if(!is.null(m)) {  ## if m is not null then get data from cache
                message("getting cached data")
                return(m)
        }
        data <- x$get()     ## if m is null then calculate mean by filling 'data' from x$get
        m <- mean(data, ...)## calculate the mean
        x$setmean(m)        ## store the mean in x$setmean from m
        m                   ## print 'm' which is the mean for 'data'
}