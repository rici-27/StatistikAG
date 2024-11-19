## (das hier verwendet optim nicht, und muss von uns nicht gemacht werden!)


## Step 1: Lasso function using bfgs
# coordinate descent
lasso <-function(X, y, lambda = .1, tol=1e-6, iter=100, verbose=T) {
  w = solve(crossprod(X) + diag(lambda, ncol(X))) %*% crossprod(X,y)
  tol_curr= 1
  J = ncol(X)
  a = rep(0, J)
  c_ = rep(0, J)
  i= 1
  
  while (tol< tol_curr&& i< iter) {
    w_old= w 
    a = colSums(X^2)
    l = length(y)*lambda  # for consistency with glmnetapproach
    c_ = sapply(1:10, function(j)  sum(X[,j]*(y -X[,-j]%*%w_old[-j])))
    w = w_old
    w[c_< l & c_ > -l] = 0
    tol_curr= crossprod(w -w_old)  
    i= i+ 1
    if (verbose && i%%10 == 0) message(i)
  } 
  w
}

## Step 2: Simulate data
set.seed(1234)
N = 500
p = 10
X = scale(matrix(rnorm(N*p), ncol=p))
b = c(.5, -.5, .25, -.25, .125, -.125, rep(0, 4))
y = scale(X %*% b + rnorm(N, sd=.5))


# Step 3: Estimation
result= lasso(X, y, lambda=0.1, tol=1e-12)