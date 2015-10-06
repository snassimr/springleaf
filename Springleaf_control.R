######################################## Springleaf_control.R ######################################
SYSG_SYSTEM_DIR            <- "/home/rstudio/springleafpj/"
SYSG_INPUT_DIR             <- "/home/rstudio/Dropbox/springleaf/input/"
SYSG_OUTPUT_MODELING_DIR   <- "/home/rstudio/output_modeling"
SYS_RUN_MODE               <- "PMEP" # P , PMEP
SYS_IDENTIFIER_FEATURES <- 1
SYS_TARGET_NAME         <- "target"

source("/home/rstudio/springleafpj/Springleaf_functions.R")

# Run to prepare data and save data locally
if (SYS_RUN_MODE == "P") {
   create_log_entry("", " Starting run ....................................","SF")
   create_log_entry("", "Starting prepare data","SF")
   perform_data_preparation()
   gc(T,T)
   stop (SYS_RUN_MODE)
}

############################################ MODEL ASSESSMENT

# source("/home/rstudio/springleafpj/Springleaf_functions.R")
create_log_entry("", " Starting run ....................................","SF")

#PREPARE MODELING AND EVALUATION DATA
create_log_entry("", "Starting load data","SF")
if (exists("me_data"))              
  rm(me_data)
setwd(SYSG_SYSTEM_DIR)
me_data           <- get(load("me_data.rda"))
create_log_entry("","Finsihed load data","SF")

# PREPARE MODEL ASSESSMENT
library(doMC)
closeAllConnections()
registerDoMC(cores=4)
library(caret)
library(stringr)

# START MODEL ASSESSMENT
# RF , GBM , XGB , XGBC
SYS_MODEL_ID      <- "XGBC"
SYS_INPUT_FRACTION_SEED <- 1234
# (0;1])
SYS_INPUT_DATA_FRACTION <-  1.0

ma_run_id <- paste0("MA_","#",SYS_INPUT_FRACTION_SEED,"#",SYS_INPUT_DATA_FRACTION,"#",SYS_MODEL_ID,"#",Sys.time())

set.seed(SYS_INPUT_FRACTION_SEED)
m_sample_indexes <- createDataPartition(me_data$target , p = SYS_INPUT_DATA_FRACTION , list = FALSE)

me_data_sample <- me_data[m_sample_indexes,]

setwd(SYSG_OUTPUT_MODELING_DIR)
me_classification_model <- create_model_assessment_data(me_data_sample,ma_run_id)

model_id <- paste0("MODEL_","#",SYS_INPUT_FRACTION_SEED,"#",SYS_INPUT_DATA_FRACTION,"#",SYS_MODEL_ID,"#", Sys.time())
save(me_classification_model, file = paste0(model_id,".rda"))

############################################ PREDICTION

# source("/home/rstudio/springleafpj/Springleaf_functions.R")
create_log_entry("", "Starting prediction on data","SF")
if (exists("p_data"))              
  rm(p_data)
if (exists("p_classification_model"))              
  rm(p_classification_model)
setwd(SYSG_SYSTEM_DIR)
p_data                 <- get(load("p_data.rda"))
setwd(SYSG_OUTPUT_MODELING_DIR)
p_classification_model <- get(load(paste0(model_id,".rda")))

prediction_data <- create_p_prediction_data(p_classification_model, p_data , me_data)

setwd(SYSG_SYSTEM_DIR)
options(scipen=10)
write.csv(prediction_data, file = paste0("submission_", model_id ,".csv"), row.names = FALSE)
options(scipen=0)
create_log_entry("","Finished prediction on data","SF")
gc(T,T)
create_log_entry("","Finished run ....................................","SF")




