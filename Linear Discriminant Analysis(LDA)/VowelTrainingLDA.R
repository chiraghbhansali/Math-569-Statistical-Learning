vowel <- read.table("vowel.train.txt", header = TRUE, sep = ",", quote = "")
da <- vowel[, -1]
color <- c("red", "blue", "green", "yellow", "black","brown", "orange", "beige", "grey75", "purple", "pink")
Y <- da[, 1]
X <- da[, -1]
K <- length(unique(Y))
N <- length(Y)
p <- ncol(X)
mu.k <- do.call("rbind", lapply(1:K, function(k) colMeans(X[Y == k,])))
mu.bar <- colMeans(mu.k)
mu.k.tmp <- matrix(rep(t(mu.k), N / K), nrow = ncol(mu.k))
Sigma <- (t(X) - mu.k.tmp) %*% t(t(X) - mu.k.tmp) / (N - K)
Sigma.eigen <- eigen(Sigma)
Sigma.inv.sqrt <- Sigma.eigen$vectors %*% diag(1/sqrt(Sigma.eigen$values)) %*% t(Sigma.eigen$vectors)
X.star <- t(Sigma.inv.sqrt %*% (t(X) - mu.bar))
mu.k.star <- t(Sigma.inv.sqrt %*% (t(mu.k) - mu.bar))
M <- mu.k.star
M.svd <- eigen(t(M) %*% M / K)
X.new <- X.star %*% M.svd$vectors
mu.k.new <- mu.k.star %*% M.svd$vectors
plot(-X.new[, 1], X.new[, 2], col = color[Y], pch = Y + 1,main = "Linear Discriminant Analysis",xlab = "Coordinate 1 for Training Data", ylab = "Coordinate 2 for Training Data")
points(-mu.k.new[, 1], mu.k.new[, 2], col = color[Y], pch = 19, cex = 1.5)
library(MASS)
LDA <- lda(y ~ ., data = da)
X.pred <- predict(LDA, newdata = X)
mu.k.pred <- predict(LDA, newdata = as.data.frame(LDA$means))
X.new <- X.pred$x
mu.k.new <- mu.k.pred$x
x.lim <- range(X.new[, 1])
y.lim <- range(X.new[, 2])
x.grid <- 100
y.grid <- 100
grid <- cbind(seq(x.lim[1], x.lim[2], length = x.grid),
rep(seq(y.lim[1], y.lim[2], length = y.grid), each = x.grid))
get.class <- function(grid, mu){
K <- nrow(mu)
apply(grid, 1, function(x) which.min(dist(rbind(x, mu))[1:K]))
}
grid.class <- get.class(grid, mu.k.new[, 1:2])
plot(X.new[, 1], X.new[, 2], col = color[Y], type = "n",
main = "Classified to nearest mean",
xlab = "Coordinate 1", ylab = "Coordinate 2")
points(grid[, 1], grid[, 2], col = color[grid.class], pch = 19, cex = 0.3)
points(X.new[, 1], X.new[, 2], pch = Y + 1, cex = 0.8)
points(mu.k.new[, 1], mu.k.new[, 2], pch = 19, cex = 1.5)
library(VGAM)
da.glm <- as.data.frame(cbind(Y, X.new[, 1], X.new[, 2]))
colnames(da.glm) <- c("y", "c.1", "c.2")
LR <- vglm(y ~ ., family = multinomial(), data = da.glm)
X.pred <- predict(LR, newdata = da.glm[, -1], type = "response")
mu.k.new <- do.call("rbind",lapply(1:K, function(k) colMeans(da.glm[da.glm[,1] == k, -1])))
mu.k.pred <- predict(LR, newdata = as.data.frame(mu.k.new), type = "response")
X.new.class <- apply(X.pred, 1, which.max)
mu.k.new.class <- apply(mu.k.pred, 1, which.max)
x.grid <- 100
y.grid <- 100
px1 <- seq(x.lim[1], x.lim[2], length = x.grid)
px2 <- seq(y.lim[1], y.lim[2], length = y.grid)
grid <- cbind(seq(x.lim[1], x.lim[2], length = x.grid),rep(seq(y.lim[1], y.lim[2], length = y.grid), each = x.grid))
grid <- as.data.frame(grid)
colnames(grid) <- c("c.1", "c.2")
grid.pred <- predict(LR, grid, type = "response")
grid.class <- apply(grid.pred, 1, which.max)
plot(X.new[, 1], X.new[, 2], col = color[Y], type = "n", main = "Classified by Logistic Regression", xlab = "Canonical Coordinate 1", ylab = "Canonical Coordinate 2")
points(grid[, 1], grid[, 2], col = color[grid.class], pch = 19, cex = 0.3, type="p")
points(X.new[, 1], X.new[, 2], pch = Y + 1, cex = 0.8)
points(mu.k.new[, 1], mu.k.new[, 2], pch = 19, cex = 1.5)
