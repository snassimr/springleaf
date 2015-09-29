perform_data_preparation <- function (me_input_data)
{
  
  me_input_data1       <- me_input_data[,-c(SYS_IDENTIFIER_FEATURES,target_index)]
  
  #CREATE DATA EXPLORATION REPORT
  source("/home/rstudio/springleafpj/Springleaf_functions.R")
  me_data_exploration_report <- create_data_exploration_report(me_input_data1,iteration = 1,output_mode = 'CSV' )
  uv_data_report1            <- data.frame(me_data_exploration_report$uv_data_report)
  
  #PREPARE DATA
  SYS_MIN_REQ_DISTINCT_VALUES <- 1
  SYS_MAX_REQ_DISTINCT_VALUES <- 50
  SYS_REQ_MIN_NUM_NAS         <- 0
  SYS_REQ_MAX_NUM_NAS         <- 100
  
  # CREATE 
  me_ts_var_features   <- as.character(subset(uv_data_report1 , FEATURE_TYPE == "timestamp" , select = FEATURE_NAME)$FEATURE_NAME)
  
  me_features_replace  <- c(me_ts_var_features)
  me_ts_features_data  <- create_features_ts(me_input_data1[,names(me_input_data1) %in% me_ts_var_features],me_ts_var_features)
  me_input_data2       <- data.frame(me_input_data1[,!(names(me_input_data1) %in% me_features_replace)],me_ts_features_data)
  
  me_data_exploration_report <- create_data_exploration_report(me_input_data2,iteration = 2,output_mode = 'CSV' )
  uv_data_report2            <- data.frame(me_data_exploration_report$uv_data_report)
  
  # TRANSFORM
  
  me_fill_NAs_features       <- as.character(subset(uv_data_report2 , NO_NAs > SYS_REQ_MIN_NUM_NAS & NO_NAs <= SYS_REQ_MAX_NUM_NAS , select = FEATURE_NAME)$FEATURE_NAME)
  me_fill_NAs_features_data  <- process_input_missing_data(me_input_data2[,names(me_input_data2) %in% me_fill_NAs_features])
  me_input_data3             <- data.frame(me_input_data2[,!(names(me_input_data2) %in% me_fill_NAs_features)],me_fill_NAs_features_data)
  
  me_data_exploration_report <- create_data_exploration_report(me_input_data3,iteration = 3,output_mode = 'CSV' )
  uv_data_report3            <- data.frame(me_data_exploration_report$uv_data_report)
  
  # REMOVE
  me_low_var_features  <- as.character(subset(uv_data_report3 , NO_DISTINCT <= SYS_MIN_REQ_DISTINCT_VALUES , select = FEATURE_NAME)$FEATURE_NAME)
  me_high_var_features <- as.character(subset(uv_data_report3 , NO_DISTINCT > SYS_MAX_REQ_DISTINCT_VALUES & FEATURE_TYPE == "categorical", select = FEATURE_NAME)$FEATURE_NAME)
  me_high_NAs_features <- as.character(subset(uv_data_report3 , NO_NAs > SYS_REQ_MAX_NUM_NAS , select = FEATURE_NAME)$FEATURE_NAME)
  
  me_features_remove <- c(me_low_var_features,me_high_var_features,me_high_NAs_features)
  me_input_data4     <- me_input_data3[,!(names(me_input_data3) %in% me_features_remove)]
  me_input_data4     <- data.frame(me_input_data4,target=me_input_target_data)
  
  setwd(SYSG_OUTPUT_MODELING_DIR)
  save(me_ts_var_features,file = paste0("dp_me_ts_var_features",".rda"))
  save(me_features_replace,file = paste0("dp_me_features_replace",".rda"))
  save(me_features_remove,file = paste0("dp_me_features_remove",".rda"))
  
  return(me_input_data4)
  
  
}

create_data_exploration_report <- function (input_data,iteration,output_mode)
{
  uv_data_report          <- NULL
  
  Sys.setlocale("LC_TIME", "C")
  
  for(i in 1:ncol(input_data)) {
    i_uv_data_report <- NULL
    i_FEATURE_NAME   <- colnames(input_data)[i]
    i_FEATURE_TYPE   <- if (is.numeric(input_data[,i])) {
      'numeric'
    } else if (!all(is.na(strptime(input_data[,i], "%d%B%y:%H:%M:%S")))) {
      'timestamp'
    } else 'categorical'
    
    i_NO_DISTINCT    <- length(unique(input_data[,i]))
    i_NO_NA          <- sum(is.na(input_data[,i]))
    
    i_uv_data_report <- cbind(i_FEATURE_NAME,i_FEATURE_TYPE,i_NO_DISTINCT,i_NO_NA)
    
    uv_data_report        <- rbind(uv_data_report,i_uv_data_report)
  }
  
  rownames(uv_data_report) <- 1:nrow(uv_data_report)
  colnames(uv_data_report) <- c("FEATURE_NAME","FEATURE_TYPE","NO_DISTINCT","NO_NAs")
  uv_data_report           <- data.frame(uv_data_report)
  uv_data_report           <- transform(uv_data_report,
                                        NO_DISTINCT = as.numeric(paste(NO_DISTINCT)),
                                        NO_NAs = as.numeric(paste(NO_NAs)))
  
  if(output_mode == 'CSV') {
    setwd(SYSG_OUTPUT_MODELING_DIR)
    write.csv(uv_data_report, file = paste0("data_exploration_report",iteration,".csv"), row.names = FALSE)
  }
  
  return (list(uv_data_report=uv_data_report))
}

create_model_assessment_data <- function (me_input_data,model_id)
{
  
  set.seed(998)
  m_indexes <- createDataPartition(me_input_data$target , p = .75, list = FALSE)
  m_input_data <- me_input_data[ m_indexes,]
  e_input_data <- me_input_data[-m_indexes,]
  
  m_input_data$target <- as.factor(paste0("t", m_input_data$target))
  e_input_data$target <- as.factor(paste0("t", e_input_data$target))
  
  m_input_data$target <- factor(m_input_data$target,levels(m_input_data$target)[c(2,1)])
  e_input_data$target <- factor(e_input_data$target,levels(e_input_data$target)[c(2,1)])
  
###############################################################################################################
  # "DOWN" , "SMOTE"
  SYS_ME_BALANCING <- 'down'
  set.seed(9560)
  # Class balancing
    # SMOTE
    if (SYS_ME_BALANCING == 'smote') {
    library(DMwR)
    m_input_data   <- SMOTE(target ~ ., data  = m_input_data)
    m_distribution <- table(m_input_data$target)
    save(m_distribution, file = "m_distribution.rda")
    }
    # DOWN SAMPLING
    if (SYS_ME_BALANCING == 'down') {
      m_input_data <- downSample(x = m_input_data[, names(m_input_data) !='target'],
                                 y = m_input_data$target , yname = "target")
      m_distribution <- table(m_input_data$target)
      save(m_distribution, file = "m_distribution.rda")
    }

  
  classification_formula <- as.formula(paste("target" ,"~",
                                             paste(names(m_input_data)[!names(m_input_data)=='target'],collapse="+")))
  
  CVfolds <- 5
  # CVreps  <- 4
  
  #Index for the trainControl()
  set.seed(1045481)
  tr_index <- createFolds(m_input_data$target, k=CVfolds)
  #Seeds for the trainControl()
  set.seed(1056)
  tr_seeds <- vector(mode = "list", length = CVfolds+1)
  for(i in 1:5) tr_seeds[[i]] <- sample.int(1000, 5)
  tr_seeds[[6]] <- sample.int(1000, 1)
  
  ma_control <- trainControl(method = "cv",
                             number = CVfolds,
                             index = tr_index,
                             seeds = tr_seeds,
                             # repeats = CVreps ,
                             # returnResamp = "final" ,
                             classProbs = T,
                             summaryFunction = twoClassSummary,
                             # sampling = SYS_ME_BALANCING,
                             allowParallel = TRUE , verboseIter = TRUE)
  
  
  
  ############################################################# MODEL CREATION #####################################
  
  classification_model <- NULL
  start_time <- proc.time()
  print(paste0(Sys.time()  , " ", model_id ," Model Assesment started"))
  write(paste0(Sys.time()  , " ", model_id ," Model Assesment started") , "log.txt" , append = TRUE)
  
  if(model_id == 'RF') {
  rf_tuneGrid = expand.grid(mtry = seq(100,300, length.out = 4))
  rf <- train(classification_formula , data = m_input_data , method = "rf", metric="ROC" ,
              trControl = ma_control, tuneGrid = rf_tuneGrid , ntree = 501 , nodesize = 2 )
  classification_model <- rf
  }
  
  if(model_id == 'GBM') {
    gbm_tuneGrid = expand.grid(interaction.depth = seq(2,4, length.out = 3),
                               n.trees = seq(200,400, length.out = 3),
                               shrinkage = 0.1 , n.minobsinnode = 2)
    gbm <- train(classification_formula , data = m_input_data , method = "gbm", metric="ROC" ,
                trControl = ma_control, tuneGrid = gbm_tuneGrid)
    classification_model <- gbm
  }
  
  end_time <- proc.time()
  runtime <- round(as.numeric((end_time - start_time)[3]),2)
  
  print(paste0(Sys.time()  , " ", SYS_MODEL_ID , " Model Assesment finished : " , runtime))
  write(paste0(Sys.time()  , " ", SYS_MODEL_ID , " Model Assesment finished : " , runtime) , "log.txt" , append = TRUE)
  
  
  #Plotting results
  
#   trellis.par.set(caretTheme())
#   print(plot(rf))
  
  create_pe_prediction_data(classification_model, m_input_data , e_input_data)
  
  return(classification_model)
}

# classification_model <- glmnet
# classification_model <- svmLinear
# classification_model <- rf
create_pe_prediction_data <- function (classification_model, m_input_data , e_input_data)
{
  
  e_input_data <- process_input_uknown_data(e_input_data , m_input_data)
  e_input_data <- process_input_missing_data(e_input_data)
  
  prediction_class  <- predict(classification_model,e_input_data , type = "raw")
  prediction_score  <- predict(classification_model,e_input_data , type = "prob")
  
  
  library(ROCR)
  prediction_class_score <- NULL
  
  for (i in 1:dim(e_input_data)[1]) {
    i_prediction_class_score <- ifelse(e_input_data$target[i]=='t1', prediction_score[i,"t1"], 1 - prediction_score[i,"t0"])
    prediction_class_score <- c(prediction_class_score,i_prediction_class_score)
  }
  
  prediction.obj <- prediction(prediction_class_score,  e_input_data$target , label.ordering = c("t0","t1"))
  auc <- performance(prediction.obj, measure = 'auc')@y.values
  
  print(paste0(Sys.time()  , " ", SYS_MODEL_ID , " Evaluation AUC : " , auc))
  write(paste0(Sys.time()  , " ", SYS_MODEL_ID , " Evaluation AUC : " , auc) , "log.txt" , append = TRUE)
  
}

create_p_prediction_data <- function (classification_model,p_input_data,m_input_data)
{
  
  p_input_data$target <- as.factor(paste0("t", p_input_data$target))
  # Assuming the same order of instances after input data processing for predictions
  p_input_data_ident  <- p_input_data[,SYS_IDENTIFIER_FEATURES]
  
  p_input_data <- process_input_uknown_data(p_input_data[,names(p_input_data)[-c(SYS_IDENTIFIER_FEATURES)]],m_input_data)
  p_input_data <- process_input_missing_data(p_input_data)

  prediction_class  <- predict(classification_model,p_input_data , type = "raw")
  prediction_score  <- predict(classification_model,p_input_data , type = "prob")
  
  prediction_class_score <- NULL
  
  for (i in 1:dim(p_input_data)[1]) {
    i_prediction_class_score <- ifelse(prediction_class[i]=='t1', prediction_score[i,"t1"], 1 - prediction_score[i,"t0"])
    prediction_class_score <- c(prediction_class_score,i_prediction_class_score)
  }
  
  prediction_data           <- cbind(p_input_data_ident,prediction_class_score)
  colnames(prediction_data) <- c("ID","target")
  
  return (prediction_data)
}

process_input_uknown_data <- function (p_input_data,m_input_data)
{
  
  input_data <- p_input_data
  
  for (i in 1:(dim(input_data)[2])) {
    if (is.character(input_data[,i])) { 
      id <- which(!(input_data[,i] %in% m_input_data[,i]))
      input_data[id,i] <- NA
    }
    if (is.factor(input_data[,i])) { 
      id <- which(!(input_data[,i] %in% levels(m_input_data[,i])))
      input_data[id,i] <- NA
    }
  }
  
  return(input_data)
}

process_input_missing_data <- function (ep_input_data)
{

  input_data <- ep_input_data
  
  for (i in 1:ncol(input_data)) {
    if (class(input_data[,i]) %in% c("numeric", "integer") ) {
      input_data[is.na(input_data[,i]),i] <- mean(input_data[,i], na.rm = TRUE)
    } else if (class(input_data[,i]) %in% c("character", "factor")) {
      input_data[is.na(input_data[,i]),i] <- names(sort(-table(input_data[,i])))[1]
    }
  }
  
  return (input_data)
}

create_features_ts <- function(me_ts_input_data,me_ts_var_features)
{
  
  # Create Day , Month and Hour features for time series features

  me_ts_output_data <- NULL
  
  for (i in 1:ncol(me_ts_input_data)) {
    date  <- strptime(me_ts_input_data[,i], "%d%B%y:%H:%M:%S")
    day   <- as.numeric(format(date, "%d"))
    month <- format(date, "%b")
    hour  <- as.numeric(format(date, "%H"))
    i_me_ts_output_data <- cbind(day,month,hour)
    colnames(i_me_ts_output_data) <- paste0(names(me_ts_input_data)[i],c("day","month","hour"))
    me_ts_output_data <- cbind(me_ts_output_data,i_me_ts_output_data)
  }
  
  return(data.frame(me_ts_output_data))
  
}
