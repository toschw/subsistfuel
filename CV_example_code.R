# example of leave-one-out cross-validation code
# data with 352 records (households)
# import data here calling it gasdata 
  
# setting 30 cross-validation trials
trials<-c(1:30)
# creating training set without one data point out
gasdata2<-cbind(data,c1)

# loop to fit regression using the training set
# Note, X is vector of covariates: X1, X2, X3, ..., model still needs to be specified
# loop records predictions
# calculates difference between predicted and observed agallons
diff1<-numeric(30)
  for(i in 1:30) {
    model1<-lm(agallons~gasdata2$X,subset=(c1!=i),data=gasdata2)
              +agallonspr<-predict(model1,list(X1=gasdata2[i,2],X2=gasdata2[i,3],X3=gasdata2[i,4]),data=gasdata2))
              +diff1[i]<-gasdata2[i,1]-agallonspr 
  }

#calculating the sum of squared errors
summ1<-numeric(1)
summ1=0
 for(i in 1:30) {
  summ1<-summ1+diff1[i]^2
  }
summ1