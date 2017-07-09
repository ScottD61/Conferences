#Read data
data.dir <- '/Users/scott/downloads/'
train.file <- paste0(data.dir, 'training.csv') 
test.file <- paste0(data.dir, 'test.csv')

#show working directory
list.files('/Users/scott/downloads')

#Create dataframe
d.train <- read.csv(train.file, stringsAsFactors = F)

#Inspect dataset
str(d.train)

#Data preparation
#Create value from column in dataset
im.train <- d.train$Image

#Remove column from dataset
d.train$Image <- NULL

#Look at first variable of dataframe
im.train[1]
#Convert strings to integer
as.integer(unlist(strsplit(im.train[1], " ")))

#Install library to iterate over all columns
install.packages('doMC')
library(doMC) 
registerDoMC()

#Implement parallelization - training set
#Converts strings to integer for all variables 
im.train <- foreach(im = im.train, .combine=rbind) %dopar% { as.integer(unlist(strsplit(im, " ")))
}


#Test set
d.test <- read.csv(test.file, stringsAsFactors = F)

#Implement parallelization - test set
im.test <- foreach(im = d.test$Image, .combine=rbind) %dopar% { as.integer(unlist(strsplit(im, " ")))
}

d.test$Image <- NULL

#Save all four variables
save(d.train, im.train, d.test, im.test, file = 'model.Rd')
load('model.Rd')

#Visualization
#Convert integers into matrix
im <- matrix(data = rev(im.train[1,]), nrow = 96, ncol = 96)
image(1:96, 1:96, im, col = gray((0:255)/255))

#Color coordinates
points(96-d.train$nose_tip_x[1], 96-d.train$nose_tip_y[1], col="red") 
points(96-d.train$left_eye_center_x[1], 96-d.train$left_eye_center_y[1], col="blue") 
points(96-d.train$right_eye_center_x[1], 96-d.train$right_eye_center_y[1], col="green")

#Where are centers of nose in the images
for(i in 1:nrow(d.train)) {
  points(96-d.train$nose_tip_x[i], 96-d.train$nose_tip_y[i], col="red")
}

#Image 2
idx <- which.max(d.train$nose_tip_x)
im <- matrix(data=rev(im.train[idx,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255)) 
points(96-d.train$nose_tip_x[idx], 96-d.train$nose_tip_y[idx], col="red")

#Compute the mean of the coordinates of each keypoint in the training set and 
#Use that as a prediction for all images
colMeans(d.train, na.rm = T)

#Apply these computed coordinates to the test instances
p <- matrix(data=colMeans(d.train, na.rm=T), nrow=nrow(d.test), ncol=ncol(d.train), byrow=T)
colnames(p) <- names(d.train)

#Predict
predictions <- data.frame(ImageId = 1:nrow(d.test), p) 
head(predictions)

