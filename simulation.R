library(MASS)
X <- mvrnorm(n= 1000, mu= rep(0, 7), Sigma= diag(7))

xprod <- vector(mode= "numeric", length= 1000)

xprod <- xprod + 1;
for(i in seq(7))
  xprod <- xprod * X[, i]

xsin <- rowSums(abs(sapply(seq(7), function(i) sin(X[, i]))))

set.seed(1);
X1 <- xprod + xsin + rnorm(1000, 0, 3)
X2 <- xprod - xsin + rnorm(1000, 0, 3)

N <- rnorm(1000,0,3)
X1_cor <- xprod + xsin + N
X2_cor <- xprod - xsin + N + rnorm(1000, 0, .01)
