library(intrinsicDimension)
library(boot)

#100 simulations of data on a 20 dim hypercube
sim = list()
for (s in 1:100){
  set.seed(s)
  data = hyperCube(Ns = 80, n = 20)
  Y = c()
  for(i in 1:80)
  {
    if(sum(data[i,1:10])>5)
    {
      Y[i] = 1
    }
    else
    {
      Y[i] = 0
    }
  }
  data = cbind(data, Y)
sim[[s]] = data.frame(data)
}



#Prediction error on 100 simulations
mse_lm = matrix(nrow = 100, ncol = 20)
for(s in 1:100)
{
  df = sim[[s]]
  for(p in 1:20)
  {
    model_lm = glm(Y ~ ., data = df[, c(1:p, 21)])
    pred = predict(model_lm, newdata = df[, c(1:p, 21)])
    y = df[, 21]
    mse_lm[s,p] = mean((pred - y)^2)
  }
}
avg_lm = apply(mse_lm, 2, mean)

par(mfrow = c(2,2))
for(i in 1:100){
  if(i == 1)
  {
    plot(mse_lm[i,], type = 'l', ylim = range(0, 0.3), xlab = 'Subset size p', ylab = 'Error' , main="Prediction Error")
  }
  else{
    points(mse_lm[i,], type = 'l', col = i)
  }
  if(i == 100)
  {
    points(avg_lm, type = 'l', col = 'red', lwd = '5')
  }
}

#k-fold(here k = 10) CV error on 100 simulations
mse_kfold = matrix(nrow = 100, ncol = 20)
for(s in 1:100)
{
  df = sim[[s]]
  for(p in 1:20)
  {
    model_cv = glm(Y ~ ., data = df[, c(1:p, 21)])
    cv_err = cv.glm(df[, c(1:p, 21)], model_cv, K = 10)$delta[1]
    mse_kfold[s,p] = cv_err
  }
}
###avg_kfold = apply(mse_cv, 2, mean)
avg_kfold = apply(mse_kfold, 2, mean)

#Plotting the error curves w.r.t subset
for(i in 1:100){
  if(i == 1)
  {
    plot(mse_kfold[i,], type = 'l', ylim = range(0, 0.3), xlab = 'Subset size p', ylab = 'Error' , main="10-Fold CV Error")
  }
  else{
    points(mse_kfold[i,], type = 'l', col = i)
  }
   if(i == 100)
   {
     points(avg_kfold, type = 'l', col = 'black', lwd = '5')
   }
}

#Leave-one-out CV error on 100 simulations
mse_loo = matrix(nrow = 100, ncol = 20)
for(s in 1:100)
{
  df = sim[[s]]
  for(p in 1:20)
  {
    model_cv = glm(Y ~ ., data = df[, c(1:p, 21)])
    cv_err = cv.glm(df[, c(1:p, 21)], model_cv)$delta[1]
	mse_loo[s,p] = cv_err
  }
}
avg_loo = apply(mse_loo, 2, mean)

#Plotting the error curves w.r.t subset 
for(i in 1:100){
  if(i == 1)
  {
    plot(mse_loo[i,], type = 'l', ylim = range(0, 0.3), xlab = 'Subset size p', ylab = 'Error', main="Leave-One-Out CV Error")
  }
  else{
    points(mse_loo[i,], type = 'l', col = i)
  }
  if(i == 100)
  {
    points(avg_loo, type = 'l', col = 'black', lwd = '5')
  }
}

# Mean absolute deviation  
mean1 = c()
for(i in 1:20){
  diff = abs(mse_kfold[,i] - mse_lm[,i])
  m = mean(diff)
  mean1[i] = m
}
mean1 = sort(mean1, decreasing = TRUE)
mean2 = c()
for(i in 1:20){
  diff = abs(mse_loo[,i] - mse_lm[,i])
  m = mean(diff)
  mean2[i] = m
}
mean2 = sort(mean2, decreasing = TRUE)


mean3=c()
for(i in 1:20){
  diff = abs(mse_kfold[,i] - mean(mse_lm))
  m = mean(diff)
  mean3[i] = m
}
mean3 = sort(mean3, decreasing = TRUE)

plot(mean1, col = 'green', type = 'b', pch = 20, xlab = 'Subset size p', ylab = 'Mean Absolute deviation', main="Approximation Error")
points(mean2, col = 'blue', type = 'b', pch = 20)
points(mean3, col = 'orange', type = 'b', pch = 20)
legend("topright", legend = c('E|CV10 - Err|','E|CV10-ErrT|', 'E|CVn - ErrT|'), col = c('orange','green', 'blue'), pch = 20)




