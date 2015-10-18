source("/home/rstudio/springleafpj/Springleaf_functions.R")
create_log_entry(names(importance_data),head(importance_data,50),"F")


SYSG_SYSTEM_DIR            <- "/home/rstudio/springleafpj/"

if (exists("me_data"))              
  rm(me_data)
setwd(SYSG_SYSTEM_DIR)
me_data           <- get(load("me_data.rda"))

library(smbinning)
library(stringr)

################################### DISCRETIZATION

# me_features_to_discr <- c("VAR_0070", "VAR_0071","VAR_0881","VAR_0087")

library(discretization)
me_disc_features <- c("VAR_0571","VAR_1398")
me_disc_features <- c("VAR_1656","VAR_1657","VAR_1658","VAR_1659","VAR_1660")
subset_data       <- me_input_data4[c(me_disc_features,"target")]
setwd(SYSG_SYSTEM_DIR)
create_log_entry(""," Feature Discretization started","F")
me_discr_output_data <- NULL
me_discr_break       <- list()
for(i in 1:length(me_disc_features)) {
    discr_model <- mdlp(cbind(subset_data[me_disc_features[i]] ,subset_data$target))
    breaks <- c(min(subset_data[me_disc_features[i]]),discr_model$cutp[[1]],max(subset_data[me_disc_features[i]]))
    i_me_discr_output_data <- cut(subset_data[[me_disc_features[i]]], breaks = breaks, include.lowest = TRUE)
    me_discr_break[[me_disc_features[i]]] <- breaks
    me_discr_output_data <- cbind(me_discr_output_data,paste0("RNG",as.numeric(i_me_discr_output_data)))
  }
   me_discr_output_data <- data.frame(me_discr_output_data)
   names(me_discr_output_data) <- paste0(me_disc_features,"_D")
   
create_log_entry(""," Feature Discretization Finished","F")

library(doMC)
closeAllConnections()
registerDoMC(cores=2)

me_disc_features <- c("VAR_0648")

library(discretization)
d <- function(feature , data) {
  discr_model <- mdlp(cbind(data[feature] ,data$target))
  breaks <- c(min(data[feature]),discr_model$cutp[[1]],max(data[feature]))
}


foreach(n = 1:1 , .combine = list) %dopar% d(me_disc_features[n], me_data)



   # Apply discretization on prediction data
p_input_data <- p_input_data3
p_features_to_discr <- c("VAR_0571","VAR_1398")
p_discr_output_data <- NULL

for(i in 1:length(p_features_to_discr)) {
  breaks <- sort(me_discr_break[[p_features_to_discr[i]]])
  i_p_discr_output_data <- findInterval(p_input_data[,p_features_to_discr[i]], breaks)
  p_discr_output_data <- cbind(p_discr_output_data,paste0("RNG",as.numeric(i_p_discr_output_data)))
}
   
   p_discr_output_data <- data.frame(p_discr_output_data)
   names(p_discr_output_data) <- paste0(p_features_to_discr,"_D")

   
   
################################### TREAT SOURCE NULL VALUES   

  table(me_input_data4[["VAR_1656"]])
  table(me_input_data4[["VAR_1657"]]) 
  table(me_input_data4[["VAR_1658"]]) 
  table(me_input_data4[["VAR_1659"]]) 
  table(me_input_data4[["VAR_0710"]]) 

  # 0.0003666976 , 0.00009440456 , 0.0001092637
  vars_to_remove_NA <- c("VAR_1656","VAR_1657","VAR_1658","VAR_1659","VAR_1660")
  null_value        <- c(98,99)
  subset_data       <- me_input_data4[vars_to_remove_NA]
  subset_data[apply(subset_data,2,function(c) c %in% null_value)] <- NA
    
#  me_discr_output_data contains  range number
#  need to save cut points for test data
####################################################################################################################

  me_input_data6$target <- as.integer(str_replace(me_input_data4$target,"t",""))
  
for(i in 1:length(me_features_to_discr)) {
  discr_model <- smbinning (df = me_input_data6 , y = "target" , x = me_features_to_discr[i], p = 0.01)
  if (class(discr_model) != "character") {
    i_me_discr_output_data <- cut(me_input_data6[,me_features_to_discr[i]], breaks = discr_model$cuts)
    # colnames(i_me_discr_output_data) <- me_features_to_discr[i]
    me_discr_output_data <- cbind(me_discr_output_data,i_me_discr_output_data)
  }
}
colnames(me_discr_output_data) <- me_features_to_discr

#########   Value based feature extraction

library(readr)

SYSG_INPUT_DIR             <- "/home/rstudio/Dropbox/springleaf/input/"
setwd(SYSG_INPUT_DIR)
#READ PREDICTION DATA
SYSG_ME_DATA_FILENAME      <- "train.csv"
me_input_data_vbef  <- read_csv(SYSG_ME_DATA_FILENAME)  

table(me_input_data_vbef$VAR_1748)

str(me_input_data_vbef[,1101:1200])

str(me_input_data_vbef[,"VAR_0316"])

cor(me_input_data_vbef[,"VAR_0316"],me_input_data_vbef[,"VAR_0241"])

nearZeroVar(me_input_data_vbef$VAR_0274, saveMetrics= TRUE)


################################## Variable Importance

  library(caret)
SYSG_OUTPUT_MODELING_DIR   <- "/home/rstudio/output_modeling"
  if (exists("classification_model"))              
    rm(classification_model)
  setwd(SYSG_OUTPUT_MODELING_DIR)
  classification_model     <- get(load("MA_#1234#0.5#XGBC#2015-10-13 19:37:34.rda"))
importance_data_obj <- varImp(classification_model,scale = FALSE)$importance
importance_data     <- data.frame(Var = rownames(importance_data_obj),Imp = importance_data_obj$Overall,stringsAsFactors=FALSE)
# head(importance_data,50)
# importance_data[importance_data$Var == "VAR_1657",]

importance_data[str_detect(importance_data$Var, "VAR_1748"),]

# "VAR_1750", "VAR_0909", "VAR_0304","VAR_1748"
#  0.002530912 , 0.002497614 , 0.002424872 , 0.003879594 

############################ Check predictions equvalence
library(readr)
SYSG_SYSTEM_DIR            <- "/home/rstudio/springleafpj/"
setwd(SYSG_SYSTEM_DIR)
f1  <- read_csv("submission_MODEL_#1234#1#XGBC#2015-10-18 10:12:46.csv")
f2  <- read_csv("submission_MODEL_#1234#1#XGBC#2015-10-18 11:25:44.csv")
all.equal(f1,f2)

############################ Load final data sets


# #   #PREPARE MODELLING DATA
#   if (exists("me_data"))              
#     rm(me_data)
#   setwd(SYSG_SYSTEM_DIR)
#   me_data           <- get(load("me_data.rda"))
#   
#   if (exists("p_data"))              
#     rm(p_data)
#   setwd(SYSG_SYSTEM_DIR)
#   p_data           <- get(load("p_data.rda"))



# str(me_input_data[,1200:1300])
# summary(me_input_data[,"VAR_0295"])
# length(unique(me_input_data[,"VAR_0237"]))
# head(me_input_data[,228])
# 
# unique(me_input_data[["VAR_0305"]])
# sum(me_input_data[,227]==me_input_data[,228],na.rm = TRUE)