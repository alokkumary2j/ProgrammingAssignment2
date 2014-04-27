## makeCacheMatrix() function takes a matrix (say m1), and returns 
##    a list (say l1)
## Now CacheSolve() function takes the list (l1) returned by 
##   makeCacheMatrix() method, and returns the inverse of matrix 
##   m1 in following manner
##		if l1 is passed first time, it computes inverse of m1 and 
##		     stores it for reusability
##		otherwise, it returns the precomputed inverse of m1  
##Usage Example:
##	mat<-makeCacheMatrix(matrix(c(1,3,2,4),nrow=2,ncol=2))
##	cacheSolve(mat) ##It will compute inverse for first time
##	cacheSolve(mat) ## Simply returns the precomputed value



## makeCacheMatrix() function takes a matrix as input
## and returns a list which is used by cacheSolve() method.
## It basically stores the matrix and its inverse in its parent
## scope, so that these values can be cached as long as we want.
makeCacheMatrix <- function(mat = matrix()) {
	matinv <- NULL 
    set <- function(newMat) {
			mat <<- newMat 
			matinv <<- NULL
    }
	get <- function() mat
	setMatInv <- function(newmatinv)  matinv <<- newmatinv 
	getMatInv <- function() matinv
	list(set = set, get = get,setMatInv = setMatInv,getMatInv = getMatInv)
}


##cacheSolve method takes the list(say l1) returned by makeCacheMatrix(),
## and checks if the inverse is not computed, then it computes the
## inverse and set it inside l1, so that next time it can reuse it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matInv<-x$getMatInv()
        if(!is.null(matInv))
        {
			message("Getting Cached Data for Matrix Inverse")
			return (matInv)
		}
		##Need to compute the Matrix Inverse
		matrixData<-x$get()
		matInv<-solve(matrixData, ...) ##Assuming invertible matrix
		x$setMatInv(matInv) ##Cached result to avoid Recomputation
		matInv
}
