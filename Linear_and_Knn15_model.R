library(MASS)
#Generate classesSigmaSigma <- matrix(c(1,0,0,1),nrow = 2, ncol = 2)
Normalmean1 <- mvrnorm(n = 50, mu = c(1,0), Sigma)
Normalmean2 <- mvrnorm(n = 50, mu = c(0,1), Sigma)

nc <- ncol(Normalmean1)
nr <- nrow(Normalmean1)

mean1 <- Normalmean1[sample(1:nr, size = 50 , replace = TRUE),]
obs1 <- t(apply(mean1, 1, function(x) mvrnorm(n=1, mu=x, Sigma=Sigma/5)))
colnames(obs1) <- c('x1','x2')

nc <- ncol(Normalmean2)
nr <- nrow(Normalmean2)

mean2 <- Normalmean2[sample(1:nr, size = 50 , replace = TRUE),]
obs2 <- t(apply(mean2, 1, function(x) mvrnorm(n=1, mu=x, Sigma=Sigma/5)))
colnames(obs2) <- c('x1','x2')

# generate label
y <- rep(c(0,1), each = 50)

# training data matrix
tm <- as.data.frame(cbind(y, rbind(obs1, obs2)))

library(lattice)
with(tm, xyplot(x2 ~ x1,groups = y, col=c('red', 'blue')))


#KNN Model
library(class)
# get the range of x1 and x2
rx1 <- range(tm$x1)
rx2 <- range(tm$x2)
# get lattice points in predictor space
px1 <- seq(from = rx1[1], to = rx1[2], by = 0.1 )
px2 <- seq(from = rx2[1], to = rx2[2], by = 0.1 )
xnew <- expand.grid(x1 = px1, x2 = px2)

knn15 <- knn(train = tm[,2:3], test = xnew, cl = tm[,1], k = 15, prob = TRUE)
prob <- attr(knn15, "prob")
prob <- ifelse(knn15=="1", prob, 1-prob)
prob15 <- matrix(prob, nrow = length(px1), ncol = length(px2))

par(mar = rep(2,4))
contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main="15-nearest neighbour", axes=FALSE)
points(tm[,2:3], col=ifelse(tm[,1]==1, "blue", "red"))
points(xnew, pch=".", cex=1.2, col=ifelse(prob15>0.5, "blue", "red"))
box()


# linear regression  model
lrm <- lm(y ~ x1 + x2 , data = tm)

lrm <- lm(y ~ x1 + x2 , data = tm)
m2.pred <- predict.lm(lrm,new=tm,type="response",se.fit=TRUE)
tm$pred <- m2.pred$fit
TAB <- table(tm$pred > 0.5, tm$y)
miss_class  <- 1 -sum(diag(TAB))/sum(TAB)
print(miss_class)

# get the slope and intercept for the decision boundary
intercept <- -(lrm$coef[1] - 0.5) / lrm$coef[3]
slope <- - lrm$coef[2] / lrm$coef[3]

# Figure 2.1
xyplot(x2 ~ x1, groups = y, col = c('red', 'blue'), data = tm,
panel = function(...)
{
panel.xyplot(...)
panel.abline(intercept, slope)
},
main = 'Linear Regression ')    
	  
# Misclassification Error Liner Model

lrm <- lm(y ~ x1 + x2 , data = tm)
m2.pred <- predict.lm(lrm,new=tm,type="response",se.fit=TRUE)
tm$pred <- m2.pred$fit

TAB <- table(tm$pred > 0.5, tm$y)

miss_class  <- 1 -sum(diag(TAB))/sum(TAB)
print(miss_class)

