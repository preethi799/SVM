#Import Libraries:
library(ggplot2)
library(caret)
library(RColorBrewer)

#DATA SIMULATION:
#Step1: Simulate variables r,t:
set.seed(1)  #For reproducibility
r <- sqrt(runif(50))  
t <- 2*pi*runif(50) 
r2 <- sqrt(3*runif(50)+1) 
t2 <- 2*pi*runif(50) 
r3 <- sqrt(5*runif(50)+3) 
t3 <- 3*pi*runif(50)  

#Step2:Create Data points:
data1 <- cbind(r*cos(t), r*sin(t))
data2 <- cbind(r2*cos(t2), r2*sin(t2)) 
data3 <- cbind(r3*cos(t3), r3*sin(t3)) 

#Step3: Create Data Frame:
y<-rep(1:3, each = 50)
Data<- rbind(data1,data2,data3)
df<-data.frame(x1=Data[,1],x2=Data[,2], y = as.factor(y))

#SCATTER PLOT:
ggplot(df, aes(x = x1, y = x2, color = y,shape=y)) +geom_point(size=1.5) +theme_classic()+scale_colour_brewer(palette = "Set1")

#Test-Train Split:
set.seed(333)
trainIndex = createDataPartition(df$y, p = 0.5, list = FALSE)
train=df[trainIndex,] #Train Data
test.feature=df[-trainIndex,-3]#Test Features
test.label=df$y[-trainIndex] #Test Label

#Setting Train Control:
set.seed(10)
fitControl=trainControl(method = "repeatedcv", number = 5)

#RBF SVM:
#RBF Grids:
grid_radial=expand.grid(sigma = c(0.01, 0.1, 1,10),C = c(0.01, 0.1, 1, 10))

#Rbf SVM:
svm.Radialg=train(y ~., data = train, method = "svmRadial",
                  trControl=fitControl,
                  preProcess = c("center", "scale"), tuneGrid = grid_radial)

svm.Radialg
plot((svm.Radialg))

pred=predict(svm.Radialg,test.feature, type = 'raw')
testAcc=mean(pred==test.label)
testAcc

#POLYNOMIAL SVM:
#Polynomial Grid:
grid_poly = expand.grid(degree = c(1, 2, 3,4),
                        C = c(0.01, 0.1, 1, 10),
                        scale = c(0.001,0.01,1))
#Polynomial SVM:
svm.Poly=train(y ~., data = train, method = "svmPoly",
               trControl=fitControl,
               preProcess = c("center", "scale"), tuneGrid = grid_poly)

svm.Poly

pred1=predict(svm.Poly,test.feature, type = 'raw')
testAcc1=mean(pred1==test.label)
testAcc1

#LINEAR SVM:
#Linear Grid:
grid_linear = expand.grid(C = c(0.01, 0.1, 1, 10))

#Linear SVM:
svm.Linear=train(y ~., data = train, method = "svmLinear",
                 trControl=fitControl,
                 preProcess = c("center", "scale"), tuneGrid = grid_linear)

svm.Linear

pred2=predict(svm.Linear,test.feature, type = 'raw')
testAcc2=mean(pred2==test.label)
testAcc2

