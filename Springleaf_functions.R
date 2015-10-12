perform_data_preparation <- function()
{
  library(readr)
  #READ MODELING DATA
  SYSG_ME_DATA_FILENAME      <- "train.csv"
  setwd(SYSG_INPUT_DIR)
  me_input_data  <- read_csv(SYSG_ME_DATA_FILENAME)
  #READ PREDICTION DATA
  SYSG_P_DATA_FILENAME       <- "test.csv"
  p_input_data   <- read_csv(SYSG_P_DATA_FILENAME)
  
  me_target_index                  <- which(names(me_input_data) == SYS_TARGET_NAME)
  p_input_data_ident               <- p_input_data[[SYS_IDENTIFIER_FEATURES]]
  me_input_data[[SYS_TARGET_NAME]] <- as.factor(paste0("t", me_input_data[[SYS_TARGET_NAME]]))
  p_input_data [[SYS_TARGET_NAME]] <- as.factor(paste0("t", p_input_data[[SYS_TARGET_NAME]]))
  me_input_target_data             <- me_input_data[[SYS_TARGET_NAME]]
  
  ################################################# PREPARE MODELING AND EVALUATION DATA
  
  me_input_data1             <- me_input_data[,-c(SYS_IDENTIFIER_FEATURES,me_target_index)]
  me_data_exploration_report <- create_data_exploration_report(me_input_data1,iteration = 1,output_mode = 'CSV' )
  uv_data_report1            <- data.frame(me_data_exploration_report$uv_data_report)
  
  #PREPARE DATA
  SYS_MIN_REQ_DISTINCT_VALUES <- 2
  SYS_MAX_REQ_DISTINCT_VALUES <- 50
  SYS_REQ_MIN_NUM_NAS         <- 0
  SYS_REQ_MAX_NUM_NAS         <- 1000
  
  # CREATE NEW VALUE-BASED FEATURES 
  me_ts_var_features    <- as.character(subset(uv_data_report1 , FEATURE_TYPE == "timestamp" , select = FEATURE_NAME)$FEATURE_NAME)
  me_vbef_features      <- c(me_ts_var_features,"VAR_0241","VAR_0493")
  me_vbef_features_data <- create_vbef_features(me_input_data1[,me_vbef_features], me_ts_var_features)
  
  # REPLACE TIME SERIES and ZIP BASED FEATURES
  me_input_data2             <- data.frame(me_input_data1[,!(names(me_input_data1) %in% me_vbef_features)],me_vbef_features_data)
  
  me_data_exploration_report <- create_data_exploration_report(me_input_data2,iteration = 2,output_mode = 'CSV' )
  
  uv_data_report2            <- data.frame(me_data_exploration_report$uv_data_report)
  
  # FILL MISSING
  me_fill_NAs_features       <- as.character(subset(uv_data_report2 , NO_NAs > SYS_REQ_MIN_NUM_NAS & NO_NAs <= SYS_REQ_MAX_NUM_NAS , select = FEATURE_NAME)$FEATURE_NAME)
  me_fill_NAs_features_data  <- process_m_missing_data(me_input_data2[,me_fill_NAs_features],me_fill_NAs_features)
  me_input_data3             <- data.frame(me_input_data2[,!(names(me_input_data2) %in% me_fill_NAs_features)],me_fill_NAs_features_data)
  
  # CREATE NEW LEARNING-BASED FEATURES
  me_disc_features           <- c("VAR_1398", "VAR_1747")
  me_lbef_features_data      <- create_lbef_features(me_input_data3[,me_disc_features],
                                                me_input_target_data,
                                                me_disc_features)
  # ADD DISCRETIZATION FEATURES
  me_input_data3             <- data.frame(me_input_data3,me_lbef_features_data)
  
  me_data_exploration_report <- create_data_exploration_report(me_input_data3,iteration = 3,output_mode = 'CSV' )
  uv_data_report3            <- data.frame(me_data_exploration_report$uv_data_report)
  
  # REMOVE
  me_low_var_features  <- as.character(subset(uv_data_report3 , NO_DISTINCT <= SYS_MIN_REQ_DISTINCT_VALUES , select = FEATURE_NAME)$FEATURE_NAME)
  me_high_var_features <- as.character(subset(uv_data_report3 , NO_DISTINCT > SYS_MAX_REQ_DISTINCT_VALUES & FEATURE_TYPE == "categorical", select = FEATURE_NAME)$FEATURE_NAME)
  me_high_NAs_features <- as.character(subset(uv_data_report3 , NO_NAs > SYS_REQ_MAX_NUM_NAS , select = FEATURE_NAME)$FEATURE_NAME)
  
  # Combine features to remove
  me_features_remove  <- c(me_low_var_features,me_high_var_features,me_high_NAs_features,me_disc_features)
  # Add features back
  me_features_add_exc  <- c("VAR_0241_ZC","VAR_0493_GEN5")
  me_features_select   <- names(me_input_data3)[!(names(me_input_data3) %in% me_features_remove)]
  me_input_data4       <- me_input_data3[,c(me_features_select,me_features_add_exc)]
  
  me_data_exploration_report <- create_data_exploration_report(me_input_data4,iteration = 4,output_mode = 'CSV' )
  uv_data_report4            <- data.frame(me_data_exploration_report$uv_data_report)
  
  me_input_features   <- names(me_input_data4)
  # Assuming no data rows drop
  me_input_data4      <- data.frame(me_input_data4,target=me_input_target_data)
  
  ################################################# PREPARE PREDICTION DATA
  # Assuming the same set of input features in train and test data and same number 
  # and order of instances after processing for predictions
  
  p_vbef_features_data      <- create_vbef_features(p_input_data[,me_vbef_features],me_ts_var_features)
  p_input_data1             <- data.frame(p_input_data[,!(names(p_input_data) %in% me_vbef_features)],p_vbef_features_data)
  
  p_fill_NAs_features_data  <- process_p_missing_data(p_input_data1[,me_fill_NAs_features],me_fill_NAs_features)
  p_input_data2             <- data.frame(p_input_data1[,!(names(p_input_data1) %in% me_fill_NAs_features)],p_fill_NAs_features_data)
  
  p_lbef_features_data      <- process_lbef_features(p_input_data2[,me_disc_features],me_disc_features)
  p_input_data3             <- data.frame(p_input_data2,p_lbef_features_data)
  
  p_input_data3             <- p_input_data3[,c("ID",me_input_features)]
  
  for (f in me_input_features) {
    if (class(me_input_data4[[f]])=="factor" || class(me_input_data4[[f]])=="character") {
      levels               <- unique(c(as.character(me_input_data4[[f]]), as.character(p_input_data3[[f]])))
      me_input_data4[[f]]  <- factor(me_input_data4[[f]], levels=levels)
      p_input_data3[[f]]   <- factor(p_input_data3[[f]],  levels=levels)
    }
  }
  
  setwd(SYSG_SYSTEM_DIR)
  save(me_input_data4, file = paste0("me_data.rda"))
  save(p_input_data3, file = paste0("p_data.rda"))

 gc(T,T) 
 
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

create_model_assessment_data <- function (me_input_data,ma_run_id)
{
  
  set.seed(998)
  m_indexes <- createDataPartition(me_input_data$target , p = .75, list = FALSE)
  m_input_data <- me_input_data[ m_indexes,]
  e_input_data <- me_input_data[-m_indexes,]
  
  m_input_data$target <- factor(m_input_data$target,levels(m_input_data$target)[c(2,1)])
  e_input_data$target <- factor(e_input_data$target,levels(e_input_data$target)[c(2,1)])
  

  classification_formula <- as.formula(paste("target" ,"~",
                                             paste(names(m_input_data)[!names(m_input_data)=='target'],collapse="+")))
  
  # Initialize model assesment objects
  start_time           <- NULL
  end_time             <- NULL
  classification_model <- NULL
  assesment_grid       <- NULL
  start_time           <- proc.time()
  SYS_CV_NFOLDS        <- 5


  xgb_tuneGrid   <- expand.grid(nrounds = seq(300,500, length.out = 3) , 
                                  eta     = seq(0.02,0.02, length.out = 1) , 
                                  max_depth = seq(4,7, length.out = 4))
  assesment_grid <- xgb_tuneGrid
    
  xgb_tuneGrid   <- expand.grid(nrounds = seq(500 ,500, length.out = 1) , 
                                  eta     = seq(0.02,0.02, length.out = 1) , 
                                  max_depth = seq(10,10, length.out = 1))
  assesment_grid <- xgb_tuneGrid
    
    
  #Index for the trainControl()
  set.seed(1045481)
  tr_index <- createFolds(m_input_data$target, k=SYS_CV_NFOLDS)
  #Seeds for the trainControl()
  set.seed(1056)
  tr_seeds <- vector(mode = "list", length = SYS_CV_NFOLDS+1)
  for(i in 1:SYS_CV_NFOLDS) tr_seeds[[i]] <- sample.int(1000, dim(assesment_grid)[1]+SYS_CV_NFOLDS)
  set.seed(1056)
  tr_seeds[[SYS_CV_NFOLDS+1]] <- sample.int(1000, 1)
  
  ma_control <- trainControl(method          = "cv",
                             number          = SYS_CV_NFOLDS,
                             index           = tr_index,
                             seeds           = tr_seeds,
                             classProbs      = T,
                             summaryFunction = twoClassSummary,
                             allowParallel   = TRUE , 
                             verboseIter     = TRUE)
  
  ############################################################# MODEL CREATION #####################################
  
  create_log_entry("",paste0(ma_run_id ," Model Assesment started"),"SF")
  create_log_entry(names(assesment_grid),assesment_grid,"F")
  
  xgbc <- train(classification_formula , data = m_input_data , method = "xgbTree", 
                 metric="ROC" , trControl = ma_control, tuneGrid = assesment_grid , 
                 objective           = 'binary:logistic',
                 eval_metric       = "auc", 
                 min_child_weight    = 5,
                 subsample           = 0.6,
                 # colsample_bytree  = 0.6,
                 nthread             = 4
                 # ,early.stop.round = 50
                 )
    
  classification_model <- xgbc

  end_time <- proc.time() ; runtime <- round(as.numeric((end_time - start_time)[3]),2)
  
  create_log_entry("",paste0(ma_run_id , " Model Assesment finished : " , runtime),"SF")

  importance_data_obj <- varImp(classification_model,scale = FALSE)$importance
  importance_data     <- data.frame(Var = rownames(importance_data_obj),Imp = importance_data_obj$Overall,stringsAsFactors=FALSE)

  create_log_entry("",paste0(ma_run_id , " Feature Importance : "),"F")
  create_log_entry(names(importance_data),head(importance_data,50),"F")

  save(classification_model, file = paste0(ma_run_id,".rda"))
  
  create_pe_prediction_data(classification_model, m_input_data , e_input_data , ma_run_id)
  
  # Create final model
  m_control <- trainControl(method           = "none",
                            classProbs      = T,
                            summaryFunction = twoClassSummary,
                            allowParallel   = FALSE , 
                            verboseIter     = TRUE)
  
  opt_parameters <- classification_model$bestTune
  create_log_entry("",paste0(ma_run_id ," Optimal Model Creation started : "),"SF")
  create_log_entry(names(opt_parameters),classification_model$bestTune,"F")
  
  start_time <- proc.time()
  
  opt_xgbc <- train(classification_formula , data = me_input_data , method = "xgbTree", 
                      trControl = m_control , tuneGrid = classification_model$bestTune , 
                      objective = 'binary:logistic')
  
  opt_classification_model <- opt_xgbc
  
  end_time <- proc.time() ; runtime <- round(as.numeric((end_time - start_time)[3]),2)
  
  create_log_entry("",paste0(ma_run_id , " Optimal Model Creation finished : " , runtime),"SF")
  
  return(opt_classification_model)
}

# classification_model <- svmLinear

create_pe_prediction_data <- function (classification_model, m_input_data , e_input_data , ma_run_id)
{
  
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
  
  create_log_entry("",paste0(ma_run_id , " Evaluation AUC : " , auc),"SF")

}

create_p_prediction_data <- function (classification_model,p_input_data,m_input_data)
{
  
  # Assuming the same order of instances after input data processing for predictions
  p_input_data_ident  <- p_input_data[,SYS_IDENTIFIER_FEATURES]
  
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

process_m_missing_data <- function (m_input_data , m_missing_features)
{

  input_data        <- m_input_data
  m_missing_val       <- list()
  
  for (i in 1:ncol(input_data)) {
    if (class(input_data[,i]) %in% c("numeric", "integer") ) {
      i_mean <- mean(input_data[,i], na.rm = TRUE)
      input_data[is.na(input_data[,i]),i]    <- i_mean
      m_missing_val[[m_missing_features[i]]] <- i_mean
    } else if (class(input_data[,i]) %in% c("character", "factor")) {
      i_mode <- names(sort(-table(input_data[,i])))[1]
      input_data[is.na(input_data[,i]),i]    <- i_mode
      m_missing_val[[m_missing_features[i]]] <- i_mode
    }
  }
  
  setwd(SYSG_OUTPUT_MODELING_DIR)
  save(m_missing_val, file = paste0("m_missing_val.rda"))
  
  return (input_data)
}

process_p_missing_data <- function (p_input_data,p_missing_features)
{
  
  input_data <- p_input_data
  
  if (exists("m_missing_val"))              
    rm(m_missing_val)
  setwd(SYSG_OUTPUT_MODELING_DIR)
  m_missing_val           <- get(load("m_missing_val.rda"))
  
  for (i in 1:ncol(input_data)) {
        input_data[is.na(input_data[,i]),i] <- m_missing_val[[p_missing_features[i]]]
  }
  
  return (input_data)
}



create_vbef_features <- function(me_vbef_input,me_ts_var_features)
{
  
  # Create Day , Month and Hour features for time series features
  
  me_vbef_output    <- NULL
  
  me_ts_input_data <-  me_vbef_input[,names(me_vbef_input) %in% me_ts_var_features]
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
  
  # Create ZipCode based aggregated feature
  library(stringr)
  VAR_0241_ZC  <- paste0("ZC",str_sub(str_pad(me_vbef_input[["VAR_0241"]] ,5,pad = "0"),0,2))
  VAR_0493_GEN5 <- str_sub(me_vbef_input[["VAR_0493"]],1,5)
  
  # Replace source null values with NA
  me_vbef_output <- data.frame(me_ts_output_data,VAR_0241_ZC,VAR_0493_GEN5)
 
  return(me_vbef_output)
}

create_lbef_features <- function(me_lbef_input,input_target_data,me_disc_features)
{
  me_lbef_output    <- NULL
  
  # Create Discretizated features
  library(discretization)
  setwd(SYSG_SYSTEM_DIR)
  create_log_entry(""," Feature Discretization started","F")
  
  me_discr_output_data <- NULL
  me_discr_break       <- list()
  for(i in 1:length(me_disc_features)) {
    discr_model <- mdlp(cbind(me_lbef_input[[me_disc_features[i]]] ,input_target_data))
    breaks <- c(min(me_lbef_input[[me_disc_features[i]]]),discr_model$cutp[[1]],max(me_lbef_input[[me_disc_features[i]]]))
    i_me_discr_output_data <- cut(me_lbef_input[[me_disc_features[i]]], breaks = breaks, include.lowest = TRUE)
    me_discr_break[[me_disc_features[i]]] <- breaks
    me_discr_output_data <- cbind(me_discr_output_data,paste0("RNG",as.numeric(i_me_discr_output_data)))
  }
  me_discr_output_data <- data.frame(me_discr_output_data)
  names(me_discr_output_data) <- paste0(me_disc_features,"_D")
  
  create_log_entry(""," Feature Discretization Finished","F")
 
  me_lbef_output <- data.frame(me_discr_output_data)
  
  setwd(SYSG_OUTPUT_MODELING_DIR)
  save(me_discr_break, file = paste0("me_discr_break.rda"))
  
 return(me_lbef_output)
}

process_lbef_features <- function(p_input_data,me_disc_features)
{
  me_lbef_output    <- NULL
  
  # Create Discretizated features
  library(discretization)
  setwd(SYSG_SYSTEM_DIR)
  create_log_entry(""," Feature Discretization started","F")
  
  if (exists("me_discr_break"))              
    rm(me_discr_break)
  setwd(SYSG_OUTPUT_MODELING_DIR)
  me_discr_break           <- get(load("me_discr_break.rda"))
  
  p_discr_output_data <- NULL
  
  for(i in 1:length(me_disc_features)) {
    breaks <- sort(me_discr_break[[me_disc_features[i]]])
    i_p_discr_output_data <- findInterval(p_input_data[,me_disc_features[i]], breaks)
    p_discr_output_data <- cbind(p_discr_output_data,paste0("RNG",as.numeric(i_p_discr_output_data)))
  }
  
  p_discr_output_data <- data.frame(p_discr_output_data)
  names(p_discr_output_data) <- paste0(me_disc_features,"_D")
  
  create_log_entry(""," Feature Discretization Finished","F")
  
  return(p_discr_output_data)
}

create_log_entry <- function(message_title = "", message , log_mode)

{
  current_library <- getwd()
  
  setwd(SYSG_SYSTEM_DIR)

  if (regexpr("S",log_mode)>0) {
    print(message_title , row.names = FALSE)
    print(message , row.names = FALSE)
  }
  
  if (regexpr("F",log_mode)>0) {
    write.table(message_title , "log.txt", append = TRUE,col.names = FALSE ,  row.names = FALSE , quote = FALSE)
    write.table(paste0(Sys.time(), " : " , message) , "log.txt", append = TRUE, col.names = FALSE ,  row.names = FALSE , quote = FALSE,sep = ",")
  }
  
  setwd(current_library)
  
}  
  
