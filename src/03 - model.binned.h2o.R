
# initialise h2o cluster
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = "1g", nthreads = -1)

# get max / min for scaling
maxs <- apply(summary.dt %>% select(-device_id, -epoch_id, -steps_bin, -steps, -n), 2, max) 
mins <- apply(summary.dt %>% select(-device_id, -epoch_id, -steps_bin, -steps, -n), 2, min)

# scale summary 0-1
scaled.summary <- cbind(device_id = summary.dt$device_id, 
                        steps_bin = summary.dt$steps_bin, 
                        as.data.frame(scale(summary.dt %>% select(-device_id, -epoch_id, -steps_bin, -steps, -n), 
                                      center = mins, scale = maxs - mins)))

extra.test.data.scaled <- cbind(device_id = extra.test.data$device_id, 
                                steps_bin = extra.test.data$steps_bin, 
                                as.data.frame(scale(extra.test.data %>% 
                                                      select(-device_id, -epoch_id, -steps_bin, -steps, -n), 
                                                    center = mins, scale = maxs - mins)))



# partition
set.seed(789465)
s <- createDataPartition(summary.dt$device_id, p = 0.6, list = FALSE)

set.seed(654)
train.h2o <- scaled.summary[s, ]
s1 <- createDataPartition(train.h2o$device_id, p = 0.6, list = FALSE)
val.h2o   <- train.h2o[-s1, ]
train.h2o <- train.h2o[s1, ]
test.h2o  <- scaled.summary[-s, ]

# altogether
train_hex <- as.h2o(x = train.h2o %>% mutate(steps_bin = as.factor(steps_bin)), destination_frame = "localH2O")
val_hex   <- as.h2o(x = val.h2o %>% mutate(steps_bin = as.factor(steps_bin)), destination_frame = "localH2O")
test_hex  <- as.h2o(x = test.h2o %>% mutate(steps_bin = as.factor(steps_bin)), destination_frame = "localH2O")

# simple rf model
set.seed(789)
simple.h2o.rf.model <- h2o.randomForest(x = 2:9,
                                        y = 1,
                                        training_frame = train_hex, 
                                        validation_frame = val_hex)
set.seed(789)
simple.h2o.gbm.model <- h2o.gbm(x = 2:9,
                                        y = 1,
                                        training_frame = train_hex, 
                                        validation_frame = val_hex)


confusionMatrix(h2o.predict(simple.h2o.rf.model, test_hex)$predict %>% as.factor %>% as.vector(),
                test_hex$device_id %>% as.vector())

confusionMatrix(h2o.predict(simple.h2o.gbm.model, test_hex)$predict %>% as.factor %>% as.vector(),
                test_hex$device_id %>% as.vector())

# on new data
## mostly matches with device TAS1E31150028 which was me
table(h2o.predict(simple.h2o.rf.model, 
                  as.h2o(extra.test.data.scaled, destination_frame = "localH2O"))$predict %>% 
        as.factor %>% as.vector(),
      extra.test.data.scaled$device_id)

table(h2o.predict(simple.h2o.gbm.model, 
                  as.h2o(extra.test.data.scaled, destination_frame = "localH2O"))$predict %>% 
        as.factor %>% as.vector(),
      extra.test.data.scaled$device_id)

table(h2o.predict(simple.h2o.rf.model, 
                  as.h2o(extra.test.data.scaled, destination_frame = "localH2O"))$predict %>% 
        as.factor %>% as.vector(), 
      h2o.predict(simple.h2o.gbm.model, 
                  as.h2o(extra.test.data.scaled, destination_frame = "localH2O"))$predict %>% 
        as.factor %>% as.vector())

h2o.varimp(simple.h2o.rf.model)
h2o.varimp(simple.h2o.gbm.model)


## Import Data to H2O Cluster
h2o.results <- data.frame()
h2o.models <- list()
for (i in unique(scaled.summary$steps_bin)){
  print(i)
  # filter to steps_bin in question, partition and load data to cluster
  train_hex <- as.h2o(x = train.h2o  %>% filter(steps_bin == i) %>% select(-steps_bin), destination_frame = "localH2O")
  val_hex   <- as.h2o(x = val.h2o    %>% filter(steps_bin == i) %>% select(-steps_bin), destination_frame = "localH2O")
  test_hex  <- as.h2o(x = test.h2o   %>% filter(steps_bin == i) %>% select(-steps_bin), destination_frame = "localH2O")

  # train h2o random forest
  ## straight out of box
  set.seed(8)
  h2o.rf.model <- h2o.randomForest(x = 2:8,
                                   y = 1,
                                   training_frame = train_hex, 
                                   validation_frame = val_hex)
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
rm(i)
rm(s)
rm(yhat_test)

names(h2o.models) <- unique(unique(scaled.summary$steps_bin))

cache('h2o.models')
cache('h2o.results')

# near perfect results
## suspiciously so!!
confusionMatrix(h2o.results$pred_dev_id, h2o.results$actual_dev_id)


#### test on extra.test.data

h2o.results.extra <- data.frame()
for (i in unique(scaled.summary$steps_bin)) {
  print(i)
  extra.test_hex <- as.h2o(x = extra.test.data.scaled %>% 
                             filter(steps_bin == i) %>% 
                             select(-steps_bin), 
                           destination_frame = "localH2O")
  pred <- h2o.predict(h2o.models[i][[1]], extra.test_hex)$predict %>% as.factor
  h2o.results.extra <- rbind(h2o.results.extra, 
                        data.frame(steps_bin = i, 
                                   pred_dev_id   = pred %>% as.vector()))
}
table(h2o.results.extra$pred_dev_id, extra.test.data.scaled$device_id)

# variable importance
lapply(h2o.models, function(x) h2o.varimp(x))

# shut down cluster
h2o.shutdown()
## confirm shut down
y
