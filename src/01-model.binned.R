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
mdls.nnet <-list()
mdls.rf <-list()
mdls.c50 <-list()

for (i in unique(training$steps_bin)){
  print(i)
  tmp <- training[steps_bin == i][,steps_bin := NULL]
  tmp.mdl <- train(device_id ~ ., 
                   data = tmp, 
                   trControl = init_control,
                   metric = "Kappa",
                   preProc = c("center", "scale"),
                   method = c("nnet"),
                   tuneLength = 5,
                   continue_on_fail = TRUE)
  mdls.nnet[which(i == unique(training$steps_bin))] <- list(tmp.mdl)
  
  tmp.mdl <- train(device_id ~ ., 
                   data = tmp, 
                   trControl = init_control,
                   metric = "Kappa",
                   preProc = c("center", "scale"),
                   method = c("rf"),
                   tuneLength = 5,
                   continue_on_fail = TRUE)
  mdls.rf[which(i == unique(training$steps_bin))] <- list(tmp.mdl)
  
  tmp.mdl <- train(device_id ~ ., 
                   data = tmp, 
                   trControl = init_control,
                   metric = "Accuracy",
                   preProc = c("center", "scale"),
                   method = c("C5.0"),
                   tuneLength = 5,
                   continue_on_fail = TRUE)
  mdls.c50[which(i == unique(training$steps_bin))] <- list(tmp.mdl)
}

names(mdls.nnet) <- unique(training$steps_bin)
names(mdls.rf) <- unique(training$steps_bin)
names(mdls.c50) <- unique(training$steps_bin)

# define col's used for model
sd.cols <- c("avg.vec","sd.vec","peaks.per.sec","avg.period","sd.period","avg.amp","sd.amp")
for (i in unique(testing$steps_bin)){
  print(i)
  testing[steps_bin == i, 
          results.nnet := predict(mdls.nnet[i], .SD), 
          .SDcols = sd.cols] 
  testing[steps_bin == i, 
          results.rf := predict(mdls.rf[i], .SD), 
          .SDcols = sd.cols] 
  testing[steps_bin == i, 
          results.c50 := predict(mdls.c50[i], .SD), 
          .SDcols = sd.cols] 
}

confusionMatrix(testing$results.nnet, testing$device_id)
confusionMatrix(testing$results.rf, testing$device_id)
confusionMatrix(testing$results.c50, testing$device_id)

# cor btn results
sum(diag(table(testing$results.rf, testing$results.c50))) / nrow(testing)
sum(diag(table(testing$results.rf, testing$results.nnet))) / nrow(testing)
sum(diag(table(testing$results.nnet, testing$results.c50))) / nrow(testing)

set.seed(789321)
s1 <- sample(1:nrow(testing), round(0.6*(nrow(testing)), 0) )
# ensemble model of tree
ens.rpart <- train(device_id ~ results.nnet+results.rf+results.c50,
                   trControl = init_control,
                   data = testing[s1],
                   metric = "Kappa",
                   method = c("rpart"),
                   tuneLength = 5)

confusionMatrix(predict(ens.rpart, testing[-s1]), testing[-s1, device_id])
confusionMatrix(testing[-s1, results.rf], testing[-s1, device_id])

for (i in unique(testing$device_id)){
  print(paste("device_id", i))
  print(table(testing[device_id == i, steps_bin], 
              testing[device_id == i, device_id == results.nnet]))
}

### look at individual subjects results nnet
confusionMatrix(testing[device_id == "TAS1E31150000", results.nnet], testing[,.(device_id = as.factor(device_id))][device_id == "TAS1E31150000", device_id])
##### does this majority hold samples?
repeats <- 1000
sample.size <- 100
for (i in unique(testing$device_id)){
  print(sum(sapply(1:repeats, function(x){
    s2 <- sample(testing[device_id == i, .N], sample.size)
    tbl <- confusionMatrix(testing[device_id == i, results.nnet][s2], 
                           testing[,.(device_id = as.factor(device_id))][device_id == i, device_id][s2])
    ifelse(which(tbl$table[,i] == max(tbl$table[,i])) == which(unique(testing$device_id) == i), 
           return(T), return(F))
  }))/repeats)
}

## seems like correct device is always the most frequently found



# what does this look like
## are mis classifieds clustered?

ggplot(testing[device_id == "TAS1E31150000", 
               .(epoch = as.POSIXct(testing.epochs[device_id == "TAS1E31150000", epoch_id], origin="1970-01-01"), 
                 pred.result = device_id == results.nnet)][1:100],
       aes(epoch, pred.result)) + 
  geom_point()
