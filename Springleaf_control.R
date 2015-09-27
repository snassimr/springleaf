######################################## Springleaf_control.R

SYSG_INPUT_DIR             <- "/home/rstudio/Dropbox/springleaf/input/"
SYSG_ME_DATA_FILENAME      <- "train.csv"
SYSG_OUTPUT_MODELING_DIR   <- "/home/rstudio/output_modeling"

#READ MODELING DATA
setwd(SYSG_INPUT_DIR)
me_input_file  <- file(SYSG_ME_DATA_FILENAME)
me_input_data  <- read.csv(me_input_file, header = TRUE , sep = "," , na.string="NA")

#REMOVE INDENTIFIERS FEATURES BY COLUMN INDEX
SYS_IDENTIFIER_FEATURES <- 1
target_index         <- which(names(me_input_data) == "target")
me_input_target_data <- me_input_data[,target_index]
me_input_data1       <- me_input_data[,-c(SYS_IDENTIFIER_FEATURES,target_index)]

#CREATE DATA EXPLORATION REPORT
source("/home/rstudio/Springleaf_functions.R")
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


me_input_data  <- NULL
me_input_data1 <- NULL
me_input_data2 <- NULL
me_input_data3 <- NULL

library(doMC)
closeAllConnections()
registerDoMC(cores=4)

library(caret)

set.seed(1234)
m_sample_indexes <- createDataPartition(me_input_data4$target , p = .5, list = FALSE)

me_input_data5 <- me_input_data4[m_sample_indexes,]

setwd(SYSG_OUTPUT_MODELING_DIR)
source("/home/rstudio/Springleaf_functions.R")
modeling_run_id         <- "rf_2"
me_classification_model <- create_model_assessment_data(me_input_data5)

setwd(SYSG_OUTPUT_MODELING_DIR)
save(me_classification_model, file = paste0(modeling_run_id,".rda"))

#READ PREDICTION DATA
SYSG_P_DATA_FILENAME       <- "test.csv"
SYSG_OUTPUT_PREDICTION_DIR <- "/home/rstudio/Dropbox/springleaf/output/"

setwd(SYSG_INPUT_DIR)
write(paste0(Sys.time()  , " Starting loading prediction data") , "log.txt" , append = TRUE)
p_input_file   <- file(SYSG_P_DATA_FILENAME)
p_input_data   <- read.csv(p_input_file, header = TRUE , sep = "," , na.string="NA")

if (exists("p_classification_model"))              
  rm(p_classification_model)

setwd(SYSG_OUTPUT_MODELING_DIR)
p_classification_model <- get(load(paste0(modeling_run_id,".rda")))

# CREATE FEATURES 
p_ts_features_data   <- create_features_ts(p_input_data[,(names(p_input_data) %in% me_ts_var_features)],me_ts_var_features)
p_input_data2        <- data.frame(p_input_data[,!(names(p_input_data) %in% me_features_replace)],p_ts_features_data)
p_input_data3        <- p_input_data2[,!(names(p_input_data2) %in% me_features_remove)]

p_input_data  <- NULL
p_input_data2 <- NULL

source("/home/rstudio/Springleaf_functions.R")
write(paste0(Sys.time()  , " Starting prediction on data") , "log.txt" , append = TRUE)
prediction_data <- create_p_prediction_data(p_classification_model, p_input_data3 , me_input_data5)

setwd(SYSG_OUTPUT_PREDICTION_DIR)
options(scipen=10)
write.csv(prediction_data, file = "submission.csv", row.names = FALSE)
options(scipen=0)





  

