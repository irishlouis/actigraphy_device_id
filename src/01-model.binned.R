# run a model for every steps bin

# define model training control
init_control <- trainControl(
  method='repeatedcv',
  repeats=5,
  search="random",
  savePredictions=TRUE,
  classProbs=TRUE
)

# train list of models using control spec'd above
set.seed(456798)
mdls <-list()

for (i in unique(training$steps_bin)){
  print(i)
  tmp <- training[steps_bin == i][,steps_bin := NULL]
  tmp.mdl <- train(device_id ~ ., 
                   data = tmp, 
                   trControl = init_control,
                   metric = "Kappa",
                   # preProc = c("center", "scale"),
                   method = c("C5.0"),
                   tuneLength = 3,
                   continue_on_fail = TRUE)
  mdls[which(i == unique(training$steps_bin))] <- list(tmp.mdl)
}

names(mdls) <- unique(training$steps_bin)

for (i in unique(testing$steps_bin)){
  print(i)
  testing[steps_bin == i, results := predict(mdls[i], .SD)] 
}

confusionMatrix(testing$results, testing$device_id)

lapply(mdls, varImp)