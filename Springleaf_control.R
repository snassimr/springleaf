######################################## Springleaf_control.R ######################################
SYSG_SYSTEM_DIR            <- "/home/rstudio/springleafpj/"
SYSG_INPUT_DIR             <- "/home/rstudio/Dropbox/springleaf/input/"
SYSG_ME_DATA_FILENAME      <- "train.csv"
SYSG_OUTPUT_MODELING_DIR   <- "/home/rstudio/output_modeling"

source("/home/rstudio/springleafpj/Springleaf_functions.R")
create_log_entry("", " Starting run ....................................","SF")
create_log_entry("", "Starting prepare data","SF")
#READ MODELING DATA
setwd(SYSG_INPUT_DIR)
me_input_file  <- file(SYSG_ME_DATA_FILENAME)
me_input_data  <- read.csv(me_input_file, header = TRUE , sep = "," , na.string="NA")

#PREPARE NODELLING DATA
SYS_IDENTIFIER_FEATURES <- 1
me_input_data4 <- perform_data_preparation(me_input_data)
create_log_entry("","Finsihed prepare data","SF")

# PREPARE FOR MODEL ASSESSMENT
library(doMC)
closeAllConnections()
registerDoMC(cores=4)

library(caret)

# RF , GBM
SYS_MODEL_ID      <- "GBM"
SYS_INPUT_FRACTION_SEED <- 1234
# (0;1])
SYS_INPUT_DATA_FRACTION <-  1

ma_run_id <- paste0("MA_","#",SYS_INPUT_FRACTION_SEED,"#",SYS_INPUT_DATA_FRACTION,
                 "#",SYS_MODEL_ID , "#" , Sys.time())

set.seed(SYS_INPUT_FRACTION_SEED)
m_sample_indexes <- createDataPartition(me_input_data4$target , p = SYS_INPUT_DATA_FRACTION , list = FALSE)

me_input_data5 <- me_input_data4[m_sample_indexes,]

setwd(SYSG_OUTPUT_MODELING_DIR)
source("/home/rstudio/springleafpj/Springleaf_functions.R")

me_classification_model <- create_model_assessment_data(me_input_data5,ma_run_id)

setwd(SYSG_OUTPUT_MODELING_DIR)
model_run_id <- paste0("MODEL_","#",SYS_INPUT_FRACTION_SEED,"#",SYS_INPUT_DATA_FRACTION,
                    "#",SYS_MODEL_ID , "#" , Sys.time())
save(me_classification_model, file = paste0(model_run_id,".rda"))

#READ PREDICTION DATA
SYSG_P_DATA_FILENAME       <- "test.csv"
SYSG_OUTPUT_PREDICTION_DIR <- "/home/rstudio/springleafpj/"
setwd(SYSG_INPUT_DIR)

create_log_entry("", "Starting prediction on data","SF")
p_input_file   <- file(SYSG_P_DATA_FILENAME)
p_input_data   <- read.csv(p_input_file, header = TRUE , sep = "," , na.string="NA")

if (exists("p_classification_model"))              
  rm(p_classification_model)
if (exists("me_ts_var_features"))              
  rm(me_ts_var_features)
if (exists("me_features_replace"))              
  rm(me_features_replace)
if (exists("me_features_remove"))              
  rm(me_features_remove)

setwd(SYSG_OUTPUT_MODELING_DIR)
p_classification_model <- get(load(paste0(model_run_id,".rda")))
me_ts_var_features     <- get(load("dp_me_ts_var_features.rda"))
me_features_replace    <- get(load("dp_me_features_replace.rda"))
me_features_remove     <- get(load("dp_me_features_remove.rda"))

# CREATE FEATURES 
p_ts_features_data   <- create_features_ts(p_input_data[,(names(p_input_data) %in% me_ts_var_features)],me_ts_var_features)
p_input_data2        <- data.frame(p_input_data[,!(names(p_input_data) %in% me_features_replace)],p_ts_features_data)
p_input_data3        <- p_input_data2[,!(names(p_input_data2) %in% me_features_remove)]

p_input_data  <- NULL
p_input_data2 <- NULL

source("/home/rstudio/springleafpj/Springleaf_functions.R")
prediction_data <- create_p_prediction_data(p_classification_model, p_input_data3 , me_input_data5)

setwd(SYSG_OUTPUT_PREDICTION_DIR)
options(scipen=10)
write.csv(prediction_data, file = paste0("submission_", model_run_id ,".csv"), row.names = FALSE)
options(scipen=0)
create_log_entry("","Finished prediction on data","SF")
create_log_entry("","Finished run ....................................","SF")



  

