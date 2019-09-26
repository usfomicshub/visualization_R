# Generate scaled 4*5 matrix with random std normal samples
set.seed(101)
mat <- scale(matrix(rnorm(20), 4, 5))
dimnames(mat) <- list(paste("Sample", 1:4), 
                      paste("Var", 1:5))

# Perform PCA
myPCA <- prcomp(mat, scale. = F, center = F)
myPCA$rotation # loadings
myPCA$x # scores


# Perform SVD
mySVD <- svd(mat)
mySVD # the diagonal of Sigma mySVD$d is given as a vector
sigma <- matrix(0,4,4) # we have 4 PCs, no need for a 5th column
diag(sigma) <- mySVD$d # sigma is now our true sigma matrix

# X = U %*% D %*% t(V)
# Compare PCA scores with the SVD's U*Sigma = X %*% V
theoreticalScores <- mySVD$u %*% sigma
all(round(myPCA$x,5) == round(theoreticalScores,5)) # TRUE

# Compare PCA loadings with the SVD's V
all(round(myPCA$rotation,5) == round(mySVD$v,5)) # TRUE
