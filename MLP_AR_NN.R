library(readxl)
library(dplyr)
#install first the package before calling it via the library command

#setup directory path
script_path <- dirname(rstudioapi::getSourceEditorContext()$path);
setwd(script_path)
data <- data.frame(read_excel("uow_consumption.xlsx"))

load <- data[, 4]

#--------------------------------------------------------------------------------------------------------
# Creating columns using if today is t,
# t-1, t-2, t-3, t-4, t-7
time_lagged_data <- bind_cols(input7 = lag(load, 7),
                              input4 = lag(load, 4),
                              input3 = lag(load, 3),
                              input2 = lag(load, 2),
                              input1 = lag(load, 1),
                              output = load)

# checking and removing the existence of NA values due to that shifting
time_lagged_data <- time_lagged_data[complete.cases(time_lagged_data),]

input_output_matrix <- data.frame(time_lagged_data)


#--------------------------------------------------------------------------------------------------------
# creating vectors that contain column numbers of different matrixes
#col_combinations <- unlist(lapply(2:4, function(x) combn(1:4, x, simplify = FALSE)), recursive = FALSE)
col_combinations <- list(c(1,2), c(1,2,3), c(1,2,3,4))

# Create list of matrices for each combination
IO_list <- list()
for (cols in col_combinations) {
  print(cols)
  
  #selecting 2:5 columns because we are creating matrixes combinations only using from t-1 to t-4
  #after selecting data.frame that contains t-1:t-4, selecting columns that have column numbers in col_combinations
  mat <- as.matrix(input_output_matrix[2:5][, cols])
  
  #append output column to selected column from before step
  mat <- cbind(mat, input_output_matrix[6])
  print(head(mat))
  
  # This line concatenates the column names of the 'mat' matrix and stores the result in a new list element
  IO_list[[paste0(cols, collapse = "")]] <- mat
}

# adding full lagged IO_matrix, because it is the only one that can combine with t-7
IO_list[[length(IO_list)+1]] <- input_output_matrix
print(head(input_output_matrix))



#--------------------------------------------------------------------------------------------------------
# Remove outliers from each matrix in the list
outlier_removed_list <- list()
for (i in IO_list) {
  # Create empty list to store outlier row indices
  outlier_rows <- list()
  
  # Loop through columns and extract outlier row indices
  boxplot(i, plot = TRUE)
  for (col in names(i)) {
    # while going through columns in the matrix collecting outlier values
    outliers <- boxplot(i[[col]], plot = FALSE)$out
    # get rows indicate to the outlier values
    row_indices <- which(i[[col]] %in% outliers)
    
    #if there isnt any outlier it wont go through this process
    if (length(row_indices) > 0) {
      outlier_rows[[col]] <- as.integer(row_indices)
    }
  }
  
  
  #checking rows that have the values on df_outliers. if it's in there the row will remove
  outlier_rows <- unlist(unique(outlier_rows))
  removed <- i
  if(!is.null(outlier_rows)) {removed <- i[-outlier_rows, ]}
  
  #adding to a new list that removed outliers
  outlier_removed_list[[(length(outlier_removed_list)+1)]] <- as.data.frame(removed)
}




#--------------------------------------------------------------------------------------------------------
# Normalize each matrix on the list
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#values assigning for unnormalization in future
strength_min <- min(outlier_removed_list[[length(outlier_removed_list)]])
strength_max <- max(outlier_removed_list[[length(outlier_removed_list)]])

#applying normalization to data frame that removed outliers
normalized_list <- sapply(outlier_removed_list, normalize)


#--------------------------------------------------------------------------------------------------------
## creating matrix for train NN and test it
trainingset <- list()
testingset <- list()
for (i in normalized_list) {
  #create dataframe that contain first 380 rows to train the NN
  trainingset[[(length(trainingset)+1)]] <- as.data.frame(i[1:380,])
  
  #create dataframe that contain after 380 rows to test the NN
  test.frame <- as.data.frame(i[380:nrow(i),])
  
  #setting up row value to start from 0
  row.names(test.frame) <- 1:nrow(test.frame)
  testingset[[(length(testingset)+1)]] <- test.frame
}



#--------------------------------------------------------------------------------------------------------
library(neuralnet)
library(grid)
library(MASS)

set.seed(5056)
create_NN <- function(x){
  # making formula for neuralnet method
  formula <- as.formula(paste0("output ~ ", paste0(colnames(x)[1:(length(x)-1)], collapse = " + ")))

  #(v1, hidden layer =1 , nodes = (4), linear = TRUE)
  model1 <- neuralnet(formula, data = x, hidden = 2, learningrate = 0.001, linear.output = TRUE, act.fct = "tanh")
  plot(model1)
  cat(paste(names(x), collapse = ", "), "model - 1", nrow(x), "\n")
  
  #(v2, hidden layer =2 , nodes = ((4, 8)), linear = FALSE)
  model2 <- neuralnet(formula, data = x, hidden = c(4, 8), learningrate = 0.01, linear.output = FALSE)
  plot(model2)
  cat(paste(names(x), collapse = ", "), "model - 2", nrow(x), "\n")
  
  #(v3, hidden layer =3 , nodes = c(2, 3, 2), linear = FALSE)
  model3 <- neuralnet(formula, data = x, hidden = c(2, 3, 2), linear.output = FALSE, act.fct = "logistic")
  plot(model3)
  cat(paste(names(x), collapse = ", "), "model - 3", nrow(x), "\n")
  
  return(list(model1=model1, model2=model2, model3=model3))
}

#--------------------------------------------------------------------------------------------------------
standard_statistical_indices <- function(testing_data, compute_results){
  #testing data is list and compute results is double. 
  testing_data <- unlist(testing_data)
  
  
  rmse <- sqrt(mean((testing_data - compute_results)^2))
  mae <- mean(abs(testing_data - compute_results))
  mape <- mean(abs((testing_data - compute_results) / testing_data)) * 100
  smape <- mean(200 * abs(testing_data - compute_results) / (abs(testing_data) + abs(compute_results)))
  
  return(list(rmse=rmse, mae=mae, mape=mape, smape=smape))
}


#--------------------------------------------------------------------------------------------------------
# to unnormalize the columns the data
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

#--------------------------------------------------------------------------------------------------------
# store the outputs = {NN models, predicted resualts, standard_statistical_indices= evaluation,comparison_table}
nn_models_result <- list()

# testing the dataset and stored the results on nn_models_result list
for (i in 1:length(trainingset)) {
  
  #going through training data matrixes on list trainingset
  #creating models base on each matrixes to 3 NN models using create_NN() method
  training.data <- trainingset[[i]]
  nn <- create_NN(training.data)
  
  #going through testing data matrixes on list testing
  #get the column of original output column before normalizing
  testing.data <- testingset[[i]]
  output.data <- outlier_removed_list[[i]][380:nrow(outlier_removed_list[[i]]),]["output"]
  
  #set first rows number as 1 cause it was 381
  row.names(output.data) <- NULL
  
  #After predicting using compute method returned object will store here
  #evaluated values{ RMSE, MAE, MAPE and sMAPE }
  # original output and predicted output will store here
  nn_result <- list()
  evaluation <- list()
  comparison_table <- list()
  
  #Going through each model of matrixes
  for (model in nn) {
    # give testing matrixes as inputs and getting predited data object as output
    # store each outputs of matrixes on nn_result() list
    net.results <- compute(model, testing.data[, 1:(length(testing.data)-1)])
    nn_result[[length(nn_result)+1]] <- net.results
    
    #By giving predicted data by NN model, normalizing the data to compare with original data
    predicted.data <- unnormalize(net.results$net.result, strength_min, strength_max)
    
    # Evaluating the each model base on { RMSE, MAE, MAPE and sMAPE }
    # store evaluated data of each matrixes on evaluation matrix
    net.evaluation <- standard_statistical_indices(output.data, predicted.data)
    evaluation[[(length(evaluation)+1)]] <- net.evaluation
    
    # creating comparison table of predicted data and original data ans store the tables on comparison_table list
    output.data.table <- as.data.frame(cbind(output.data, predicted.data))
    colnames(output.data.table) <- c("Output.data", "Predicted.data")
    
    print(head(output.data.table))
    comparison_table[[(length(comparison_table)+1)]] <- output.data.table
  }
  
  #store all the data of matrixes that have created using NN on nn_models_result model
  nn_models_result[[(length(nn_models_result)+1)]] <- list(NN=nn, 
                                                           NN_test_output= nn_result, 
                                                           standard_statistical_indices= evaluation, 
                                                           comparison_table=comparison_table)
}
#--------------------------------------------------------------------------------------------------------