######################################## Springleaf_control.R ######################################

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

source("/home/rstudio/springleafpj/Springleaf_functions.R")
me_input_data4 <- perform_data_preparation(me_input_data)

# me_input_data  <- NULL
# me_input_data1 <- NULL
# me_input_data2 <- NULL
# me_input_data3 <- NULL

library(doMC)
closeAllConnections()
registerDoMC(cores=3)

library(caret)

set.seed(1234)
m_sample_indexes <- createDataPartition(me_input_data4$target , p = .5, list = FALSE)

me_input_data5 <- me_input_data4[m_sample_indexes,]

setwd(SYSG_OUTPUT_MODELING_DIR)
source("/home/rstudio/springleafpj/Springleaf_functions.R")

# RF , GBM
SYS_MODEL_ID <- "GBM"

me_classification_model <- create_model_assessment_data(me_input_data5,SYS_MODEL_ID)

setwd(SYSG_OUTPUT_MODELING_DIR)
save(me_classification_model, file = paste0(SYS_MODEL_ID,".rda"))

#READ PREDICTION DATA
SYSG_P_DATA_FILENAME       <- "test.csv"
SYSG_OUTPUT_PREDICTION_DIR <- "/home/rstudio/springleafpj/"

setwd(SYSG_INPUT_DIR)
write(paste0(Sys.time()  , " Starting loading prediction data") , "log.txt" , append = TRUE)
p_input_file   <- file(SYSG_P_DATA_FILENAME)
p_input_data   <- read.csv(p_input_file, header = TRUE , sep = "," , na.string="NA")

if (exists("p_classification_model"))              
  rm(p_classification_model)
if (exists("me_ts_var_features"))              
  rm(me_ts_var_features)
if (exists("me_features_remove"))              
  rm(me_features_remove)

setwd(SYSG_OUTPUT_MODELING_DIR)
p_classification_model <- get(load(paste0(SYS_MODEL_ID,".rda")))
me_ts_var_features     <- get(load("dp_me_ts_var_features.rda"))
me_features_remove     <- get(load("dp_me_features_remove.rda"))

# CREATE FEATURES 
p_ts_features_data   <- create_features_ts(p_input_data[,(names(p_input_data) %in% me_ts_var_features)],me_ts_var_features)
p_input_data2        <- data.frame(p_input_data[,!(names(p_input_data) %in% me_features_replace)],p_ts_features_data)
p_input_data3        <- p_input_data2[,!(names(p_input_data2) %in% me_features_remove)]

p_input_data  <- NULL
p_input_data2 <- NULL

source("/home/rstudio/springleafpj/Springleaf_functions.R")
write(paste0(Sys.time()  , " Starting prediction on data") , "log.txt" , append = TRUE)
prediction_data <- create_p_prediction_data(p_classification_model, p_input_data3 , me_input_data5)

setwd(SYSG_OUTPUT_PREDICTION_DIR)
options(scipen=10)
write.csv(prediction_data, file = "submission.csv", row.names = FALSE)
options(scipen=0)





  

