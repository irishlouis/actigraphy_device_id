# run a model for every steps bin

# define model training2.subset control
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

for (i in unique(training2.subset$steps_bin)){
  print(i)
  tmp <- training2.subset[steps_bin == i][,steps_bin := NULL]
  tmp.mdl <- train(device_id ~ ., 
                   data = tmp, 
                   trControl = init_control,
                   metric = "Kappa",
                   preProc = c("center", "scale"),
                   method = c("nnet"),
                   tuneLength = 5,
                   continue_on_fail = TRUE)
  mdls.nnet[which(i == unique(training2.subset$steps_bin))] <- list(tmp.mdl)
  
  tmp.mdl <- train(device_id ~ ., 
                   data = tmp, 
                   trControl = init_control,
                   metric = "Kappa",
                   preProc = c("center", "scale"),
                   method = c("rf"),
                   tuneLength = 5,
                   continue_on_fail = TRUE)
  mdls.rf[which(i == unique(training2.subset$steps_bin))] <- list(tmp.mdl)
  
  tmp.mdl <- train(device_id ~ ., 
                   data = tmp, 
                   trControl = init_control,
                   metric = "Accuracy",
                   preProc = c("center", "scale"),
                   method = c("C5.0"),
                   tuneLength = 5,
                   continue_on_fail = TRUE)
  mdls.c50[which(i == unique(training2.subset$steps_bin))] <- list(tmp.mdl)
}

names(mdls.nnet) <- unique(training2.subset$steps_bin)
names(mdls.rf) <- unique(training2.subset$steps_bin)
names(mdls.c50) <- unique(training2.subset$steps_bin)

# define col's used for model
sd.cols <- c("avg.vec","sd.vec","peaks.per.sec","avg.period","sd.period","avg.amp","sd.amp")
for (i in unique(testing2$steps_bin)){
  print(i)
  testing2[steps_bin == i, 
          results.nnet := predict(mdls.nnet[i], .SD), 
          .SDcols = sd.cols] 
  testing2[steps_bin == i, 
          results.rf := predict(mdls.rf[i], .SD), 
          .SDcols = sd.cols] 
  testing2[steps_bin == i, 
          results.c50 := predict(mdls.c50[i], .SD), 
          .SDcols = sd.cols] 
}

confusionMatrix(testing2$results.nnet, testing2$device_id)
confusionMatrix(testing2$results.rf, testing2$device_id)
confusionMatrix(testing2$results.c50, testing2$device_id)

# cor btn results
sum(diag(table(testing2$results.rf, testing2$results.c50))) / nrow(testing2)
sum(diag(table(testing2$results.rf, testing2$results.nnet))) / nrow(testing2)
sum(diag(table(testing2$results.nnet, testing2$results.c50))) / nrow(testing2)

set.seed(789321)
s1 <- sample(1:nrow(testing2), round(0.6*(nrow(testing2)), 0) )
# ensemble model of tree
ens.rpart <- train(device_id ~ results.nnet+results.rf+results.c50,
                   trControl = init_control,
                   data = testing2[s1],
                   metric = "Kappa",
                   method = "rpart",
                   tuneLength = 5)

confusionMatrix(predict(ens.rpart, testing2[-s1]), testing2[-s1, device_id])
confusionMatrix(testing2[-s1, results.nnet], testing2[-s1, device_id])



for (i in unique(testing2$device_id)){
  print(paste("device_id", i))
  print(table(testing2[device_id == i, steps_bin], 
              testing2[device_id == i, device_id == results.nnet]))
}

### look at individual subjects results nnet
confusionMatrix(testing2[device_id == "TAS1E31150000", results.nnet], testing2[,.(device_id = as.factor(device_id))][device_id == "TAS1E31150000", device_id])
##### does this majority hold samples?
repeats <- 1000
sample.size <- 100
for (i in unique(testing2$device_id)){
  print(paste("Proportion of samples for device_id",i, "where it is the most freq classified" ))
  print(sum(sapply(1:repeats, function(x){
    s2 <- sample(testing2[device_id == i, .N], sample.size)
    tbl <- confusionMatrix(testing2[device_id == i, results.nnet][s2], 
                           testing2[,.(device_id = as.factor(device_id))][device_id == i, device_id][s2])
    ifelse(which(tbl$table[,i] == max(tbl$table[,i])) == which(unique(testing2$device_id) == i), 
           return(T), return(F))
  }))/repeats)
}

## seems like correct device is always the most frequently found



# what does this look like
## are mis classifieds clustered?

ggplot(testing2[device_id == "TAS1E31150028", 
               .(epoch = as.POSIXct(testing2.epoch[device_id == "TAS1E31150028", epoch_id], origin="1970-01-01"), 
                 pred.result = device_id == results.nnet,
                 steps_bin=steps_bin)],
       aes(epoch, pred.result)) + 
  geom_point() +
  facet_wrap(~steps_bin, ncol = 1)



## example of raw epoch
ggplot(data.frame(n=1:1000, 
                  v=dt[(device_id == "TAS1E31150000" & epoch_id == 1461596280), vec.mag, ]), 
       aes(n, v)) + geom_line()
## example of smoothed raw epoch
smoothby <- 25
ggplot(data.frame(n = 1:(1000-smoothby+1),
                  v = rollapply(dt[(device_id == "TAS1E31150000" & epoch_id == 1461596280), vec.mag, ],
                                smoothby,
                                mean) ),
       aes(n, v)) + geom_line()