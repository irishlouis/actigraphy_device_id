library(h2o)

localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = "1g", nthreads = 6)

maxs <- apply(summary.dt %>% select(-device_id, -epoch_id, -steps_bin, -steps, -n), 2, max) 
mins <- apply(summary.dt %>% select(-device_id, -epoch_id, -steps_bin, -steps, -n), 2, min)

scaled.summary <- cbind(device_id = summary.dt$device_id, 
                        steps_bin = summary.dt$steps_bin, 
                        as.data.frame(scale(summary.dt %>% select(-device_id, -epoch_id, -steps_bin, -steps, -n), 
                                      center = mins, scale = maxs - mins)))

# partition
rows <- summary.dt[,.N,device_id][,N,] %>% min

set.seed(789465)
s <- sample(1:rows, round(0.6*rows))

## Import Data to H2O Cluster

h2o.results <- data.frame()
h2o.models <- list()
for (i in unique(scaled.summary$steps_bin)){
  # filter to steps_bin in question, partition and load data to cluster
  train_hex <- as.h2o(x = scaled.summary[s, ]  %>% filter(steps_bin == i) %>% select(-steps_bin), destination_frame = "localH2O")
  test_hex  <- as.h2o(x = scaled.summary[-s, ] %>% filter(steps_bin == i) %>% select(-steps_bin), destination_frame = "localH2O")
    
  # h2o.nnet.model <- h2o.deeplearning(x = 2:9,  # column numbers for predictors
  #                           y = 1,    # column number for label
  #                           training_frame = train_hex,
  #                           activation = "Tanh",
  #                           balance_classes = TRUE,
  #                           hidden = c(500,300,50),  ## three hidden layers
  #                           epochs = 100)
  
  # train h2o random forest
  h2o.rf.model <- h2o.randomForest(x = 2:9,
                                   y = 1,
                                   training_frame = train_hex)
  # store model in list of models
  h2o.models[which(i == unique(scaled.summary$steps_bin))] <- h2o.rf.model

  # predict on test dataset
  yhat_test <- h2o.predict(h2o.rf.model, test_hex)$predict %>% as.factor
  
  # store results
  results <- rbind(results, 
                   data.frame(steps_bin = i, 
                              actual_dev_id = test_hex$device_id %>% as.vector(),
                              pred_dev_id   = yhat_test))
}

cache('h2o.models')
cache('h2o.results')
# near perfect results
## suspiciously so!!
confusionMatrix(h2o.results$pred_dev_id, h2o.results$actual_dev_id)
