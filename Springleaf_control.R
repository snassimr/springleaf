######################################## Springleaf_control.R ######################################
SYSG_SYSTEM_DIR            <- "/home/rstudio/springleafpj/"
SYSG_INPUT_DIR             <- "/home/rstudio/Dropbox/springleaf/input/"
SYSG_OUTPUT_MODELING_DIR   <- "/home/rstudio/output_modeling"
# P   - run data preparation including read data , prepare modelling , evaluation and prediction data + new features
# DP - Data Preparation , ME - Modelling and Evaluation , P - Prediction and Submission
SYS_RUN_MODE               <- "P" 
SYS_IDENTIFIER_FEATURES    <- 1
SYS_TARGET_NAME            <- "target"

source("/home/rstudio/springleafpj/Springleaf_functions.R")
library(caret)
library(stringr)

create_log_entry("", "Starting run ....................................","SF")

############################################ DATA PREPARATION ############################################
# Run to prepare data and save data locally
if (SYS_RUN_MODE == "DP") {
   create_log_entry("", "Starting prepare data","SF")
   perform_data_preparation()
   gc(T,T)
   stop ("Data preparation finished ... ")
}

############################################ MODEL ASSESSMENT ############################################

SYS_MODEL_ID            <- "XGBC" 
SYS_INPUT_FRACTION_SEED <- 1234
SYS_INPUT_DATA_FRACTION <-  1   # (0;1])

if (SYS_RUN_MODE == "ME") {
create_log_entry("", "Starting load data","SF")  

# PREPARE MODEL ASSESSMENT
if (exists("me_data"))  rm(me_data)
setwd(SYSG_SYSTEM_DIR)
me_data                <- get(load("me_data.rda"))
create_log_entry("","Finsihed load data","SF")

library(doMC)
closeAllConnections()
registerDoMC(cores=4)

# START MODEL ASSESSMENT
create_log_entry("", "Starting model assessment","SF")

ma_model_id <- paste0("MA_","#",SYS_INPUT_FRACTION_SEED,"#",SYS_INPUT_DATA_FRACTION,"#",SYS_MODEL_ID,"#",Sys.time())

set.seed(SYS_INPUT_FRACTION_SEED)
m_sample_indexes <- createDataPartition(me_data$target , p = SYS_INPUT_DATA_FRACTION , list = FALSE)

me_data_sample <- me_data[m_sample_indexes,]

setwd(SYSG_OUTPUT_MODELING_DIR)
me_classification_model <- create_model_assessment_data(me_data_sample,ma_model_id)
}

############################################ PREDICTION #####################################################
# source("/home/rstudio/springleafpj/Springleaf_functions.R")
ma_run_id <- "MA_#1234#1#XGBC#2015-10-14 14:23:36"
if (SYS_RUN_MODE == "P") {
  create_log_entry("", "Starting prediction on data","SF")
  opt_model_id <- paste0("MODEL_","#",SYS_INPUT_FRACTION_SEED,"#",SYS_INPUT_DATA_FRACTION,"#",SYS_MODEL_ID,"#",Sys.time())
  if (exists("me_data"))  rm(me_data)
  setwd(SYSG_SYSTEM_DIR)
  me_data                <- get(load("me_data.rda"))
  if (exists("opt_parameters"))         rm(opt_parameters)
  setwd(SYSG_OUTPUT_MODELING_DIR)
  opt_parameters         <- get(load(paste0("OM_",ma_run_id,".rda")))
  
  create_p_model(opt_model_id,opt_parameters,me_data)
  
  if (exists("p_data"))                 rm(p_data)
  if (exists("p_classification_model")) rm(p_classification_model)
  gc(T,T)
  setwd(SYSG_SYSTEM_DIR)
  p_data                 <- get(load("p_data.rda"))
  setwd(SYSG_OUTPUT_MODELING_DIR)
  p_classification_model <- get(load(paste0(opt_model_id,".rda")))
  
  
  prediction_data        <- create_p_prediction_data(p_classification_model, p_data , me_data)
  
  setwd(SYSG_SYSTEM_DIR)
  options(scipen=10)
  write.csv(prediction_data, file = paste0("submission_", opt_model_id ,".csv"), row.names = FALSE)
  options(scipen=0)
  create_log_entry("","Finished prediction on data","SF")
  gc(T,T)
  
}

create_log_entry("","Finished run ....................................","SF")
if (SYS_RUN_MODE %in% c("DP","ME","P"))
  stop ("Illegal SYS_RUN_MODE :" , SYS_RUN_MODE)






