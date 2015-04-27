library(caret);library(ggplot2);library(rattle)

data <- read.csv("PML/pml-training.CSV")
summary(data)

qplot(pitch_forearm,roll_forearm,data=data,colour=classe)
qplot(pitch_forearm,roll_belt,data=data,colour=classe)

col <- names(data) %in% c("X","user_name","cvtd_timestamp","raw_timestamp_part_1",
                              "raw_timestamp_part_2","num_window")

data$kurtosis_roll_belt <- as.numeric(levels(data$kurtosis_roll_belt)[data$kurtosis_roll_belt])
data$kurtosis_picth_belt <- as.numeric(levels(data$kurtosis_picth_belt)[data$kurtosis_picth_belt])
data$kurtosis_yaw_belt <- as.numeric(levels(data$kurtosis_yaw_belt)[data$kurtosis_yaw_belt])
data$skewness_roll_belt <- as.numeric(levels(data$skewness_roll_belt)[data$skewness_roll_belt])
data$skewness_roll_belt.1 <- as.numeric(levels(data$skewness_roll_belt.1)[data$skewness_roll_belt.1])
data$skewness_yaw_belt <- as.numeric(levels(data$skewness_yaw_belt)[data$skewness_yaw_belt])
data$max_yaw_belt <- as.numeric(levels(data$max_yaw_belt)[data$max_yaw_belt])
data$min_yaw_belt <- as.numeric(levels(data$min_yaw_belt)[data$min_yaw_belt])
data$amplitude_yaw_belt <- as.numeric(levels(data$amplitude_yaw_belt)[data$amplitude_yaw_belt])
data$kurtosis_roll_arm <- as.numeric(levels(data$kurtosis_roll_arm)[data$kurtosis_roll_arm])
data$kurtosis_picth_arm <- as.numeric(levels(data$kurtosis_picth_arm)[data$kurtosis_picth_arm])
data$kurtosis_yaw_arm <- as.numeric(levels(data$kurtosis_yaw_arm)[data$kurtosis_yaw_arm])
data$skewness_roll_arm <- as.numeric(levels(data$skewness_roll_arm)[data$skewness_roll_arm])
data$skewness_pitch_arm <- as.numeric(levels(data$skewness_pitch_arm)[data$skewness_pitch_arm])
data$skewness_yaw_arm <- as.numeric(levels(data$skewness_yaw_arm)[data$skewness_yaw_arm])
data$kurtosis_roll_dumbbell <- as.numeric(levels(data$kurtosis_roll_dumbbell)[data$kurtosis_roll_dumbbell])
data$kurtosis_picth_dumbbell <- as.numeric(levels(data$kurtosis_picth_dumbbell)[data$kurtosis_picth_dumbbell])
data$kurtosis_yaw_dumbbell <- as.numeric(levels(data$kurtosis_yaw_dumbbell)[data$kurtosis_yaw_dumbbell])
data$skewness_roll_dumbbell <- as.numeric(levels(data$skewness_roll_dumbbell)[data$skewness_roll_dumbbell])
data$skewness_pitch_dumbbell <- as.numeric(levels(data$skewness_pitch_dumbbell)[data$skewness_pitch_dumbbell])
data$skewness_yaw_dumbbell <- as.numeric(levels(data$skewness_yaw_dumbbell)[data$skewness_yaw_dumbbell])
data$max_yaw_dumbbell <- as.numeric(levels(data$max_yaw_dumbbell)[data$max_yaw_dumbbell])
data$min_yaw_dumbbell <- as.numeric(levels(data$min_yaw_dumbbell)[data$min_yaw_dumbbell])
data$amplitude_yaw_dumbbell <- as.numeric(levels(data$amplitude_yaw_dumbbell)[data$amplitude_yaw_dumbbell])
data$kurtosis_roll_forearm <- as.numeric(levels(data$kurtosis_roll_forearm)[data$kurtosis_roll_forearm])
data$kurtosis_picth_forearm <- as.numeric(levels(data$kurtosis_picth_forearm)[data$kurtosis_picth_forearm])
data$kurtosis_yaw_forearm <- as.numeric(levels(data$kurtosis_yaw_forearm)[data$kurtosis_yaw_forearm])
data$skewness_roll_forearm <- as.numeric(levels(data$skewness_roll_forearm)[data$skewness_roll_forearm])
data$skewness_pitch_forearm <- as.numeric(levels(data$skewness_pitch_forearm)[data$skewness_pitch_forearm])
data$skewness_yaw_forearm <- as.numeric(levels(data$skewness_yaw_forearm)[data$skewness_yaw_forearm])
data$max_yaw_forearm <- as.numeric(levels(data$max_yaw_forearm)[data$max_yaw_forearm])
data$min_yaw_forearm <- as.numeric(levels(data$min_yaw_forearm)[data$min_yaw_forearm])
data$amplitude_yaw_forearm <- as.numeric(levels(data$amplitude_yaw_forearm)[data$amplitude_yaw_forearm])


datacln <- data[,!col]

## Convierte los NA en 0
for(i in 1:dim(datacln)[2]){
  if(sum(is.na(datacln[,i])) > 0){
    datacln[is.na(datacln[,i]),i] <- 0
  }
}
## Filtramos los constntes
datacln <- datacln[, sapply(datacln, function(v) var(v, na.rm=TRUE)!=0)]



inTrain <- createDataPartition(y=datacln$classe, p=0.75,list=FALSE)
training <- datacln[inTrain,]
testing <- datacln[-inTrain,]

set.seed(10)
trfs <- createFolds(y=data$classe,k=10,list=TRUE,returnTrain=TRUE)

fit1 <- train(classe~.,method="rpart", data=datacln[trfs[[1]],])
fit2 <- train(classe~.,method="rpart", data=datacln[trfs[[2]],])
fit3 <- train(classe~.,method="rpart", data=datacln[trfs[[3]],])
fit4 <- train(classe~.,method="rpart", data=datacln[trfs[[4]],])
fit5 <- train(classe~.,method="rpart", data=datacln[trfs[[5]],])
fit6 <- train(classe~.,method="rpart", data=datacln[trfs[[6]],])
fit7 <- train(classe~.,method="rpart", data=datacln[trfs[[7]],])
fit8 <- train(classe~.,method="rpart", data=datacln[trfs[[8]],])
fit9 <- train(classe~.,method="rpart", data=datacln[trfs[[9]],])
fit10 <- train(classe~.,method="rpart", data=datacln[trfs[[10]],])

fitFinal <- train(classe~
                    pitch_forearm
                    + roll_forearm
                    + roll_belt
                    + magnet_dumbbell_y
                    + accel_belt_z
                    + magnet_belt_y
                    + yaw_belt
                    + total_accel_belt
                    + magnet_arm_x
                    + accel_arm_x
                    + roll_dumbbell
                    + magnet_dumbbell_z
                    + accel_dumbbell_y
                    + magnet_dumbbell_x
                    + roll_arm
                    ,method="rpart", data=training)


result <- predict(fitFinal, newdata = testing)
confusionMatrix(testing$classe,result)

testdata <- read.csv("PML/pml-testing.CSV")


