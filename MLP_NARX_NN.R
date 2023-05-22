# load installed package libraries
library(dplyr)
library(readxl)
library(neuralnet)
library(Matrix)
#install first the package before calling it via the library command

#setup directory path
script_path <- dirname(rstudioapi::getSourceEditorContext()$path);
setwd(script_path)
data <- data.frame(read_excel("uow_consumption.xlsx"))

# load columns to variables
data_20th_hour <- data.frame(data)[,4]
data_19th_hour <- data.frame(data)[,3]
data_18th_hour <- data.frame(data)[,2]

# Creating I/O matrixes by time delayed variables 
time_lag_20_hour <- bind_cols(previous7 = lag(data_20th_hour, 7),
                     previous4 = lag(data_20th_hour, 4),
                     previous3 = lag(data_20th_hour, 3),
                     previous2 = lag(data_20th_hour, 2),
                     previous1 = lag(data_20th_hour, 1),
                     output = data_20th_hour)


time_lag_19_hour <- bind_cols(previous7 = lag(data_19th_hour, 7),
                     previous4 = lag(data_19th_hour, 4),
                     previous3 = lag(data_19th_hour, 3),
                     previous2 = lag(data_19th_hour, 2),
                     previous1 = lag(data_19th_hour, 1),
                     output = data_20th_hour)


time_lag_18_hour <- bind_cols(previous7 = lag(data_18th_hour, 7),
                     previous4 = lag(data_18th_hour, 4),
                     previous3 = lag(data_18th_hour, 3),
                     previous2 = lag(data_18th_hour, 2),
                     previous1 = lag(data_18th_hour, 1),
                     output = data_18th_hour)


# Remove rows with NA
time_lag_20_hour <- time_lag_20_hour[complete.cases(time_lag_20_hour),]
time_lag_19_hour <- time_lag_19_hour[complete.cases(time_lag_19_hour),]
time_lag_18_hour <- time_lag_18_hour[complete.cases(time_lag_18_hour),]

remove_outliers <- function(IO_matrix){
  # Create empty list to store outlier row indices
  outlier_rows <- list()
  
  # Loop through columns and extract outlier row indices
  boxplot(IO_matrix, plot = TRUE)
  for (col in names(IO_matrix)) {
    # while going through columns in the matrix collecting outlier values
    outliers <- boxplot(IO_matrix[[col]], plot = FALSE)$out
    # get rows indicate to the outlier values
    row_indices <- which(IO_matrix[[col]] %in% outliers)
    
    #if there isnt any outlier it wont go through this process
    if (length(row_indices) > 0) {
      outlier_rows[[col]] <- as.integer(row_indices)
    }
  }
  
  
  #checking rows that have the values on df_outliers. if it's in there the row will remove
  outlier_rows <- unlist(unique(outlier_rows))
  removed <- IO_matrix
  if(!is.null(outlier_rows)) {removed <- IO_matrix[-outlier_rows, ]}
  
  #adding to a new list that removed outliers
  return(as.data.frame(removed))
}

time_lag_20_hour <- remove_outliers(time_lag_20_hour)
time_lag_19_hour <- remove_outliers(time_lag_19_hour)
time_lag_19_hour <- remove_outliers(time_lag_20_hour)

# checking time-delayed I/O configuration from these rows
head (time_lag_20_hour)
head (time_lag_19_hour)
head (time_lag_18_hour)

# define function to normalize the dataset
normalize <- function(value) {
  (value - min(value)) / (max(value) - min(value))
}

IO_matrix_20th <- as.data.frame(lapply(time_lag_20_hour, normalize))
IO_matrix_19th <- as.data.frame(lapply(time_lag_19_hour, normalize))
IO_matrix_18th <- as.data.frame(lapply(time_lag_18_hour, normalize))

#Declaring the input and output matrices
io_matrix_type3_20th <- IO_matrix_20th[,1:6]
io_matrix_type2_20th <- IO_matrix_20th[,2:6]
io_matrix_type1_20th <- IO_matrix_20th[,3:6]

io_matrix_type3_19th <- IO_matrix_19th[,1:6]
io_matrix_type2_19th <- IO_matrix_19th[,2:6]
io_matrix_type1_19th <- IO_matrix_19th[,3:6]


io_matrix_type3_18th <- IO_matrix_18th[,1:6]
io_matrix_type2_18th <- IO_matrix_18th[,2:6]
io_matrix_type1_18th <- IO_matrix_18th[,3:6]

#-------------------------------------------------------------------------------------------------------------------
# creating train Data set
io_matrix_type3_20th_train <- io_matrix_type3_20th[1:380, ]
io_matrix_type2_20th_train <- io_matrix_type2_20th[1:380, ]
io_matrix_type1_20th_train <- io_matrix_type1_20th[1:380, ]

io_matrix_type3_19th_train <- io_matrix_type3_19th[1:380, ]
io_matrix_type2_19th_train <- io_matrix_type2_19th[1:380, ]
io_matrix_type1_19th_train <- io_matrix_type1_19th[1:380, ]


io_matrix_type3_18th_train <- io_matrix_type3_18th[1:380, ]
io_matrix_type2_18th_train <- io_matrix_type2_18th[1:380, ]
io_matrix_type1_18th_train <- io_matrix_type1_18th[1:380, ]

# creating test Data set
io_matrix_type3_20th_test <- io_matrix_type3_20th[381:nrow(io_matrix_type3_20th), ]
io_matrix_type2_20th_test <- io_matrix_type2_20th[381:nrow(io_matrix_type2_20th), ]
io_matrix_type1_20th_test <- io_matrix_type1_20th[381:nrow(io_matrix_type1_20th), ]

io_matrix_type3_19th_test <- io_matrix_type3_19th[381:nrow(io_matrix_type3_19th), ]
io_matrix_type2_19th_test <- io_matrix_type2_19th[381:nrow(io_matrix_type2_19th), ]
io_matrix_type1_19th_test <- io_matrix_type1_19th[381:nrow(io_matrix_type1_19th), ]


io_matrix_3_18th_test <- io_matrix_type3_18th[381:nrow(io_matrix_type3_18th), ]
io_matrix_type2_18th_test <- io_matrix_type2_18th[381:nrow(io_matrix_type2_18th), ]
io_matrix_type1_18th_test <- io_matrix_type1_18th[381:nrow(io_matrix_type1_18th), ]

#-------------------------------------------------------------------------------------------------------
# creating models of 18th hour 
model_18th_3_m1 <- neuralnet(output ~. , hidden = 6, data = io_matrix_type3_18th_train, learningrate = 0.0001, linear.output=FALSE)

model_18th_3_m2 <- neuralnet(output ~. , hidden = c(4,8), data = io_matrix_type3_18th_train ,act.fct = "logistic", learningrate = 0.01, linear.output=FALSE)



model_18th_2_m1 <- neuralnet(output ~. , hidden = 6, data = io_matrix_type2_18th_train, learningrate = 0.0001, linear.output=FALSE)

model_18th_2_m2 <- neuralnet(output ~. , hidden = c(4,8), data = io_matrix_type2_18th_train ,act.fct = "logistic", learningrate = 0.01, linear.output=FALSE)



model_18th_1_m1 <- neuralnet(output ~. , hidden = 6, data = io_matrix_type1_18th_train, learningrate = 0.0001, linear.output=FALSE)

model_18th_1_m2 <- neuralnet(output ~. , hidden = c(4,8), data = io_matrix_type1_18th_train ,act.fct = "logistic", learningrate = 0.01, linear.output=FALSE)

#------------------------------------------------------------------------------------------------------
# predicting the output of 18th hour, base on given training data
pred_rslt_18th_3_m1 <- compute(model_18th_3_m1, io_matrix_type3_18th_train[,1:5])
pred_rslt_18th_3_m2 <- compute(model_18th_3_m2, io_matrix_type3_18th_train[,1:5])

pred_rslt_18th_2_m1 <- compute(model_18th_2_m1, io_matrix_type2_18th_train[,1:4])
pred_rslt_18th_2_m2 <- compute(model_18th_2_m2, io_matrix_type2_18th_train[,1:4])

pred_rslt_18th_1_m1 <- compute(model_18th_1_m1, io_matrix_type1_18th_train[,1:3])
pred_rslt_18th_1_m2 <- compute(model_18th_1_m2, io_matrix_type1_18th_train[,1:3])








#-------------------------------------------------------------------------------------------------------
# creating again 19th hour train matrix
io_matrix_3_19th_m1_train <- cbind(io_matrix_type3_19th_train[,1:5], pred_rslt_18th_3_m1$net.result, io_matrix_type3_19th_train["output"])
io_matrix_3_19th_m2_train <- cbind(io_matrix_type3_19th_train[,1:5], pred_rslt_18th_3_m2$net.result, io_matrix_type3_19th_train["output"])

io_matrix_2_19th_m1_train <- cbind(io_matrix_type2_19th_train[,1:4], pred_rslt_18th_2_m1$net.result, io_matrix_type2_19th_train["output"])
io_matrix_2_19th_m2_train <- cbind(io_matrix_type2_19th_train[,1:4], pred_rslt_18th_2_m2$net.result, io_matrix_type2_19th_train["output"])

io_matrix_1_19th_m1_train <- cbind(io_matrix_type1_19th_train[,1:3], pred_rslt_18th_1_m1$net.result, io_matrix_type1_19th_train["output"])
io_matrix_1_19th_m2_train <- cbind(io_matrix_type1_19th_train[,1:3], pred_rslt_18th_1_m2$net.result, io_matrix_type1_19th_train["output"])

colnames(io_matrix_3_19th_m1_train)[6] <- "previous18"
colnames(io_matrix_3_19th_m2_train)[6] <- "previous18"
colnames(io_matrix_2_19th_m1_train)[5] <- "previous18"
colnames(io_matrix_2_19th_m2_train)[5] <- "previous18"
colnames(io_matrix_1_19th_m1_train)[4] <- "previous18"
colnames(io_matrix_1_19th_m2_train)[4] <- "previous18"

# creating models of 19th hour 
model_19th_3_m1 <- neuralnet(output ~. , hidden = 6, data = io_matrix_3_19th_m1_train, learningrate = 0.0001, linear.output=FALSE)

model_19th_3_m2 <- neuralnet(output ~. , hidden = c(4,8), data = io_matrix_3_19th_m2_train ,act.fct = "logistic", learningrate = 0.01, linear.output=FALSE)



model_19th_2_m1 <- neuralnet(output ~. , hidden = 6, data = io_matrix_2_19th_m1_train, learningrate = 0.0001, linear.output=FALSE)

model_19th_2_m2 <- neuralnet(output ~. , hidden = c(4,8), data = io_matrix_2_19th_m2_train ,act.fct = "logistic", learningrate = 0.01, linear.output=FALSE)



model_19th_1_m1 <- neuralnet(output ~. , hidden = 6, data = io_matrix_1_19th_m1_train, learningrate = 0.0001, linear.output=FALSE)

model_19th_1_m2 <- neuralnet(output ~. , hidden = c(4,8), data = io_matrix_1_19th_m2_train ,act.fct = "logistic", learningrate = 0.01, linear.output=FALSE)


# predicting the output of 19th hour, base on given training data
pred_rslt_19th_3_m1 <- compute(model_19th_3_m1, io_matrix_3_19th_m1_train[,1:6])
pred_rslt_19th_3_m2 <- compute(model_19th_3_m2, io_matrix_3_19th_m2_train[,1:6])

pred_rslt_19th_2_m1 <- compute(model_19th_2_m1, io_matrix_2_19th_m1_train[,1:5])
pred_rslt_19th_2_m2 <- compute(model_19th_2_m2, io_matrix_2_19th_m2_train[,1:5])

pred_rslt_19th_1_m1 <- compute(model_19th_1_m1, io_matrix_1_19th_m1_train[,1:4])
pred_rslt_19th_1_m2 <- compute(model_19th_1_m2, io_matrix_1_19th_m2_train[,1:4])








#-------------------------------------------------------------------------------------------------------
# creating again 20th hour train matrix
io_matrix_type3_20th_m1_train <- cbind(io_matrix_type3_20th_train[,1:5], pred_rslt_19th_3_m1$net.result, io_matrix_type3_20th_train["output"])
io_matrix_type3_20th_m2_train <- cbind(io_matrix_type3_20th_train[,1:5], pred_rslt_19th_3_m2$net.result, io_matrix_type3_20th_train["output"])

io_matrix_type2_20th_m1_train <- cbind(io_matrix_type2_20th_train[,1:4], pred_rslt_19th_2_m1$net.result, io_matrix_type2_20th_train["output"])
io_matrix_type2_20th_m2_train <- cbind(io_matrix_type2_20th_train[,1:4], pred_rslt_19th_2_m2$net.result, io_matrix_type2_20th_train["output"])

io_matrix_type1_20th_m1_train <- cbind(io_matrix_type1_20th_train[,1:3], pred_rslt_19th_1_m1$net.result, io_matrix_type1_20th_train["output"])
io_matrix_type1_20th_m2_train <- cbind(io_matrix_type1_20th_train[,1:3], pred_rslt_19th_1_m2$net.result, io_matrix_type1_20th_train["output"])


colnames(io_matrix_type3_20th_m1_train)[6] <- "previous19"
colnames(io_matrix_type3_20th_m2_train)[6] <- "previous19"
colnames(io_matrix_type2_20th_m1_train)[5] <- "previous19"
colnames(io_matrix_type2_20th_m2_train)[5] <- "previous19"
colnames(io_matrix_type1_20th_m1_train)[4] <- "previous19"
colnames(io_matrix_type1_20th_m2_train)[4] <- "previous19"

# creating models of 20th hour 
model_20th_3_m1 <- neuralnet(output ~. , hidden = 6, data = io_matrix_type3_20th_m1_train, learningrate = 0.0001, linear.output=FALSE)

model_20th_3_m2 <- neuralnet(output ~. , hidden = c(4,8), data = io_matrix_type3_20th_m2_train ,act.fct = "logistic", learningrate = 0.01, linear.output=FALSE)



model_20th_2_m1 <- neuralnet(output ~. , hidden = 6, data = io_matrix_type2_20th_m1_train, learningrate = 0.0001, linear.output=FALSE)

model_20th_2_m2 <- neuralnet(output ~. , hidden = c(4,8), data = io_matrix_type2_20th_m2_train ,act.fct = "logistic", learningrate = 0.01, linear.output=FALSE)



model_20th_1_m1 <- neuralnet(output ~. , hidden = 6, data = io_matrix_type1_20th_m1_train, learningrate = 0.0001, linear.output=FALSE)

model_20th_1_m2 <- neuralnet(output ~. , hidden = c(4,8), data = io_matrix_type1_20th_m2_train ,act.fct = "logistic", learningrate = 0.01, linear.output=FALSE)







#TESTING-----------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
# predicting the output of 18th hour, base on given testing data
pred_rslt_18th_3_m1_test <- compute(model_18th_3_m1, io_matrix_3_18th_test[,1:5])
pred_rslt_18th_3_m2_test <- compute(model_18th_3_m2, io_matrix_3_18th_test[,1:5])

pred_rslt_18th_2_m1_test <- compute(model_18th_2_m1, io_matrix_type2_18th_test[,1:4])
pred_rslt_18th_2_m2_test <- compute(model_18th_2_m2, io_matrix_type2_18th_test[,1:4])

pred_rslt_18th_1_m1_test <- compute(model_18th_1_m1, io_matrix_type1_18th_test[,1:3])
pred_rslt_18th_1_m2_test <- compute(model_18th_1_m2, io_matrix_type1_18th_test[,1:3])

#-------------------------------------------------------------------------------------------------------
# creating again 19th hour test matrix
io_matrix_3_19th_m1_test <- cbind(io_matrix_type3_19th_test[,1:5], pred_rslt_18th_3_m1_test$net.result, io_matrix_type3_19th_test["output"])
io_matrix_3_19th_m2_test <- cbind(io_matrix_type3_19th_test[,1:5], pred_rslt_18th_3_m2_test$net.result, io_matrix_type3_19th_test["output"])

io_matrix_2_19th_m1_test <- cbind(io_matrix_type2_19th_test[,1:4], pred_rslt_18th_2_m1_test$net.result, io_matrix_type2_19th_test["output"])
io_matrix_2_19th_m2_test <- cbind(io_matrix_type2_19th_test[,1:4], pred_rslt_18th_2_m2_test$net.result, io_matrix_type2_19th_test["output"])

io_matrix_1_19th_m1_test <- cbind(io_matrix_type1_19th_test[,1:3], pred_rslt_18th_1_m1_test$net.result, io_matrix_type1_19th_test["output"])
io_matrix_1_19th_m2_test <- cbind(io_matrix_type1_19th_test[,1:3], pred_rslt_18th_1_m2_test$net.result, io_matrix_type1_19th_test["output"])

colnames(io_matrix_3_19th_m1_test)[6] <- "previous18"
colnames(io_matrix_3_19th_m2_test)[6] <- "previous18"
colnames(io_matrix_2_19th_m1_test)[5] <- "previous18"
colnames(io_matrix_2_19th_m2_test)[5] <- "previous18"
colnames(io_matrix_1_19th_m1_test)[4] <- "previous18"
colnames(io_matrix_1_19th_m2_test)[4] <- "previous18"





# predicting the output of 19th hour, base on given training data
pred_rslt_19th_3_m1 <- compute(model_19th_3_m1, io_matrix_3_19th_m1_test[,1:6])
pred_rslt_19th_3_m2 <- compute(model_19th_3_m2, io_matrix_3_19th_m2_test[,1:6])

pred_rslt_19th_2_m1 <- compute(model_19th_2_m1, io_matrix_2_19th_m1_test[,1:5])
pred_rslt_19th_2_m2 <- compute(model_19th_2_m2, io_matrix_2_19th_m2_test[,1:5])

pred_rslt_19th_1_m1 <- compute(model_19th_1_m1, io_matrix_1_19th_m1_test[,1:4])
pred_rslt_19th_1_m2 <- compute(model_19th_1_m2, io_matrix_1_19th_m2_test[,1:4])

#-------------------------------------------------------------------------------------------------------
# creating again 20th hour test matrix
io_matrix_type3_20th_m1_test <- cbind(io_matrix_type3_20th_test[,1:5], pred_rslt_19th_3_m1$net.result, io_matrix_type3_20th_test["output"])
io_matrix_type3_20th_m2_test <- cbind(io_matrix_type3_20th_test[,1:5], pred_rslt_19th_3_m2$net.result, io_matrix_type3_20th_test["output"])

io_matrix_type2_20th_m1_test <- cbind(io_matrix_type2_20th_test[,1:4], pred_rslt_19th_2_m1$net.result, io_matrix_type2_20th_test["output"])
io_matrix_type2_20th_m2_test <- cbind(io_matrix_type2_20th_test[,1:4], pred_rslt_19th_2_m2$net.result, io_matrix_type2_20th_test["output"])

io_matrix_type1_20th_m1_test <- cbind(io_matrix_type1_20th_test[,1:3], pred_rslt_19th_1_m1$net.result, io_matrix_type1_20th_test["output"])
io_matrix_type1_20th_m2_test <- cbind(io_matrix_type1_20th_test[,1:3], pred_rslt_19th_1_m2$net.result, io_matrix_type1_20th_test["output"])


colnames(io_matrix_type3_20th_m1_test)[6] <- "previous19"
colnames(io_matrix_type3_20th_m2_test)[6] <- "previous19"
colnames(io_matrix_type2_20th_m1_test)[5] <- "previous19"
colnames(io_matrix_type2_20th_m2_test)[5] <- "previous19"
colnames(io_matrix_type1_20th_m1_test)[4] <- "previous19"
colnames(io_matrix_type1_20th_m2_test)[4] <- "previous19"





# predicting the output of 20th hour, base on given training data
pred_rslt_20th_3_m1_test <- compute(model_20th_3_m1, io_matrix_type3_20th_test[,1:6])
pred_rslt_20th_3_m2_test <- compute(model_20th_3_m2, io_matrix_type3_20th_test[,1:6])

pred_rslt_20th_2_m1_test <- compute(model_20th_2_m1, io_matrix_type2_20th_test[,1:5])
pred_rslt_20th_2_m2_test <- compute(model_20th_2_m2, io_matrix_type2_20th_test[,1:5])

pred_rslt_20th_1_m1_test <- compute(model_20th_1_m1, io_matrix_type1_20th_test[,1:4])
pred_rslt_20th_1_m2_test <- compute(model_20th_1_m2, io_matrix_type1_20th_test[,1:4])

# and find its maximum & minimum value
minimum_of_20th <- min(data_20th_hour)
maximum_of_20th <- max(data_20th_hour)
minimum_of_19th <- min(data_19th_hour)
maximum_of_19th <- max(data_19th_hour)
minimum_of_18th <- min(data_18th_hour)
maximum_of_18th <- max(data_18th_hour)

#Create the reverse of normalised function – de-normalized
denormalization <- function(value, min, max) {
  return( (max - min)*value + min )
}

predict_3_m1 <- denormalization(pred_rslt_20th_3_m1_test$net.result, minimum_of_20th, maximum_of_20th)
predict_3_m2 <- denormalization(pred_rslt_20th_3_m2_test$net.result, minimum_of_20th, maximum_of_20th)

predict_2_m1 <- denormalization(pred_rslt_20th_2_m1_test$net.result, minimum_of_20th, maximum_of_20th)
predict_2_m2 <- denormalization(pred_rslt_20th_2_m2_test$net.result, minimum_of_20th, maximum_of_20th)

predict_1_m1 <- denormalization(pred_rslt_20th_1_m1_test$net.result, minimum_of_20th, maximum_of_20th)
predict_1_m2 <- denormalization(pred_rslt_20th_1_m2_test$net.result, minimum_of_20th, maximum_of_20th)

original20th_3 <- denormalization(io_matrix_type3_20th_test["output"], minimum_of_20th, maximum_of_20th)
original20th_2 <- denormalization(io_matrix_type2_20th_test["output"], minimum_of_20th, maximum_of_20th)
original20th_1 <- denormalization(io_matrix_type1_20th_test["output"], minimum_of_20th, maximum_of_20th)


table_3_m1 <- cbind(predict_3_m1, original20th_3)
table_3_m2 <- cbind(predict_3_m2, original20th_3)
table_2_m1 <- cbind(predict_2_m1, original20th_2)
table_2_m2 <- cbind(predict_2_m2, original20th_2)
table_1_m1 <- cbind(predict_1_m1, original20th_1)
table_1_m2 <- cbind(predict_1_m2, original20th_1)

colnames(table_3_m1)[1] <- "predict"
colnames(table_3_m2)[1] <- "predict"
colnames(table_2_m1)[1] <- "predict"
colnames(table_2_m2)[1] <- "predict"
colnames(table_1_m1)[1] <- "predict"
colnames(table_1_m2)[1] <- "predict"

rownames(table_3_m1) <- 1:nrow(table_3_m1)
rownames(table_3_m2) <- 1:nrow(table_3_m2)
rownames(table_2_m1) <- 1:nrow(table_2_m1)
rownames(table_2_m2) <- 1:nrow(table_2_m2)
rownames(table_1_m1) <- 1:nrow(table_1_m1)
rownames(table_1_m2) <- 1:nrow(table_1_m2)

# Evaluating the model
library(Metrics)
standard_statistical_indices <- function(table){
  original <- unlist(table[,2])
  predict <- unlist(table[,1])
  
  print(head(table))
  
  rmse <- rmse(original, predict)
  message("rmse :",rmse)
  mae  <- mae(original, predict)
  message("mae :",mae)
  mape <- mape(original, predict)
  message("mape :",mape)
  smape<- smape(original, predict)
  message("smape :",smape)
}

standard_statistical_indices(table_3_m1)
standard_statistical_indices(table_3_m2)
standard_statistical_indices(table_2_m1)
standard_statistical_indices(table_2_m2)
standard_statistical_indices(table_1_m1)
standard_statistical_indices(table_1_m2)



