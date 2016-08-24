
# initialise h2o cluster
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, nthreads = -1)

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
val_hex   <- as.h2o(x = val.h2o   %>% mutate(steps_bin = as.factor(steps_bin)), destination_frame = "localH2O")
test_hex  <- as.h2o(x = test.h2o  %>% mutate(steps_bin = as.factor(steps_bin)), destination_frame = "localH2O")

# simple rf model


simple.h2o.rf.model <- h2o.randomForest(x = 2:9, 
                                        y = 1,
                                        training_frame = train_hex, 
                                        validation_frame = val_hex,
                                        ntrees = 50,
                                        max_depth = 10,
                                        stopping_rounds = 2, 
                                        stopping_metric = "AUTO",
                                        stopping_tolerance = 0.001,
                                        seed = 789)
simple.h2o.gbm.model <- h2o.gbm(x = 2:9,
                                y = 1,
                                training_frame = train_hex, 
                                validation_frame = val_hex,
                                ntrees = 50,
                                max_depth = 10,
                                stopping_rounds = 2, 
                                stopping_metric = "AUTO",
                                stopping_tolerance = 0.001,
                                seed = 789)

simple.h2o.dnn.model <- h2o.deeplearning(x = 2:9,
                                         y = 1,
                                         training_frame = train_hex, 
                                         validation_frame = val_hex,
                                         hidden = c(200,200,200),
                                         activation = "Rectifier",
                                         epochs = 10,
                                         stopping_rounds = 2, 
                                         stopping_metric = "AUTO",
                                         stopping_tolerance = 0.001,
                                         seed = 789)

confusionMatrix(h2o.predict(simple.h2o.rf.model, test_hex)$predict %>% as.vector(),
                test_hex$device_id %>% as.vector())

confusionMatrix(h2o.predict(simple.h2o.gbm.model, test_hex)$predict %>% as.vector(),
                test_hex$device_id %>% as.vector())

confusionMatrix(h2o.predict(simple.h2o.dnn.model, test_hex)$predict %>% as.vector(),
                test_hex$device_id %>% as.vector())


#### tune gbm
search_criteria <- list(strategy = "RandomDiscrete", 
                        max_runtime_secs = 240)
nfolds <- 5

# GBM Hyperparamters
learn_rate_opt <- c(0.01, 0.02, 0.03) 
max_depth_opt <- c(3, 5, 9, 15)
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
col_sample_rate_opt <- c(0.2, 0.4,  0.6,  0.8)
ntrees <- c(10, 100)
hyper_params <- list(learn_rate = learn_rate_opt,
                     ntrees = ntrees,
                     max_depth = max_depth_opt, 
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt)

gbm_grid <- h2o.grid("gbm", x = 2:9, y = 1,
                     training_frame = train_hex,
                     seed = 1,
                     nfolds = nfolds,
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)
gbm_grid <- h2o.getGrid(gbm_grid@grid_id, sort_by="logloss", decreasing = F)
h2o.performance(h2o.getModel(gbm_grid@model_ids[[1]]), test_hex)

confusionMatrix(h2o.predict(h2o.getModel(gbm_grid@model_ids[[1]]), train_hex)$predict %>% as.vector,
                test_hex$device_id %>% as.vector)

gbm_models <- lapply(gbm_grid@model_ids, function(model_id) h2o.getModel(model_id))


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
                                   validation_frame = val_hex,
                                   ntrees = 50,
                                   max_depth = 10,
                                   stopping_rounds = 2, 
                                   stopping_metric = "AUTO",
                                   stopping_tolerance = 0.001,
                                   seed = 789)
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

# results
confusionMatrix(h2o.results$pred_dev_id, h2o.results$actual_dev_id)


#### test on extra.test.data
## actually worse :(

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
y
