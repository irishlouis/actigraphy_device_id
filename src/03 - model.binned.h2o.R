
# initialise h2o cluster
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = "1g", nthreads = 6)

# get max / min for scaling
maxs <- apply(summary.dt %>% select(-device_id, -epoch_id, -steps_bin, -steps, -n), 2, max) 
mins <- apply(summary.dt %>% select(-device_id, -epoch_id, -steps_bin, -steps, -n), 2, min)

# scale summary 0-1
scaled.summary <- cbind(device_id = summary.dt$device_id, 
                        steps_bin = summary.dt$steps_bin, 
                        as.data.frame(scale(summary.dt %>% select(-device_id, -epoch_id, -steps_bin, -steps, -n), 
                                      center = mins, scale = maxs - mins)))

# partition
set.seed(789465)
s <- createDataPartition(summary.dt$device_id, p = 0.6, list = FALSE)

## Import Data to H2O Cluster
h2o.results <- data.frame()
h2o.models <- list()
for (i in unique(scaled.summary$steps_bin)){
  print(i)
  # filter to steps_bin in question, partition and load data to cluster
  train_hex <- as.h2o(x = scaled.summary[s, ]  %>% filter(steps_bin == i) %>% select(-steps_bin), destination_frame = "localH2O")
  test_hex  <- as.h2o(x = scaled.summary[-s, ] %>% filter(steps_bin == i) %>% select(-steps_bin), destination_frame = "localH2O")

  # train h2o random forest
  ## straight out of box
  h2o.rf.model <- h2o.randomForest(x = 2:8,
                                   y = 1,
                                   training_frame = train_hex)
  # store model in list of models
  h2o.models[[which(i == unique(scaled.summary$steps_bin))]] <- h2o.rf.model

  # predict on test dataset
  yhat_test <- h2o.predict(h2o.rf.model, test_hex)$predict %>% as.factor 
  
  # store results
  h2o.results <- rbind(h2o.results, 
                       data.frame(steps_bin = i, 
                                  actual_dev_id = test_hex$device_id %>% as.vector(),
                                  pred_dev_id   = yhat_test %>% as.vector()))
}
rm(train_hex)
rm(test_hex)

names(h2o.models) <- unique(unique(scaled.summary$steps_bin))

cache('h2o.models')
cache('h2o.results')

# near perfect results
## suspiciously so!!
confusionMatrix(h2o.results$pred_dev_id, h2o.results$actual_dev_id)

# variable importance
lapply(h2o.models, function(x) h2o.varimp(x))

# shut down cluster
h2o.shutdown()
## confirm shut down
y
