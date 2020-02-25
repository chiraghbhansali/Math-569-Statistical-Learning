library(splines)
n=50
v1=runif(n,0,1)
v1=sort(v1)
X=cbind(1,v1,v1^2,v1^3)
eps=rnorm(length(v1))
### coefficients
beta <- c(4.4678,1.4908,-0.1316,0.1876)
Y=X%*%beta+eps
#Y=cos(v1)+eps


#global linear
X1=cbind(1,v1)
H4=X1%*%solve(t(X1)%*%X1)%*%t(X1)
yhat_4=H4%*%Y
sigma2_4=(1/(n-ncol(X1)))*sum((yhat_4-Y)^2)
var4=diag(H4)


#Global cubic ploynomial
P=poly(v1,3)
H3=P%*%solve(t(P)%*%P)%*%t(P)
yhat_3=H3%*%Y
sigma2_3=(1/(n-ncol(P)))*sum((yhat_3-Y)^2)
var3=diag(H3)


###cubic spline 2 knots
N=bs(v1,knots=c(0.33,0.66),degree =6)
N=cbind(1,N)
H=N%*%solve(t(N)%*%N)%*%t(N)
yhat_1=H%*%Y
sigma2_1=(1/(n-ncol(N)))*sum((yhat_1-Y)^2)
var1=diag(H)


###natural cubic spline 6 knots
v=seq(0.1,0.9,length=6)
N2=ns(x = v1,knots = v[2:5],Boundary.knots = c(0.1,0.9),df = 6)
H2=N2%*%solve(t(N2)%*%N2)%*%t(N2)
yhat_2=H2%*%Y
sigma2_2=(1/(n-ncol(N2)))*sum((yhat_2-Y)^2)
var2=diag(H2)



minY=min(var1,var2,var3,var4)
maxY=max(var1,var2,var3,var4)
plot(v1,var1,pch=20,type='b',ylim=c(minY,maxY+0.1),col="green",
     xlab = "X",ylab = "pointwisevariance",main = "Pointwise variance curves")
lines(v1,var2,pch=20,type='b',col="blue")
lines(v1,var3,pch=20,type='b',col="red")
lines(v1,var4,pch=20,type='b',col="orange")
legend("top",legend=c("Global Linear","Global Cubic Polynomial","Cubic Spline - 2 Knots","Natural Cubic Spline - 6 Knots"),
       col=c("orange","red","green","blue"),lty=1)
