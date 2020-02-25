res = load_prostate_data(globalScale=FALSE,trainingScale=TRUE)
XTraining = res[[1]]
XTesting = res[[2]]

### Libraries
library(glmnet)
library(plotmo)

### create model matrix from the training matrix
x=model.matrix(lpsa~.-1,data=XTraining)
y=XTraining$lpsa

### fit the model using glmnet package
fit.lasso=glmnet(x,y,alpha=1)

#### plot the model 
plot_glmnet(fit.lasso,xvar="norm",label=TRUE,col="black")
