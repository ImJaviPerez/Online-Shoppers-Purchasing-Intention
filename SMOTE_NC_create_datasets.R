# Script to create some SMOTE_NC data training sets
#

# Install package containing SMOTE_NC()
if (FALSE){
  # RUN THIS IN A TERMINAL: 
  #    sudo apt-get install libssl-dev
  #    sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev
  install.packages("devtools")
  devtools::install_github("dongyuanwu/RSBID")
  install.packages("remotes")
  remotes::install_github("dongyuanwu/RSBID")
}


library(log4r)
# Create a new logger object with create.logger().
logger <- create.logger()
# Set the logger's file output.
logfile(logger) <- 'o_s_SMOTE_NC_datasets.log'
# Set the current level of the logger.
level(logger) <- "DEBUG"
#level(logger) <- "INFO"
# At priority level INFO, a call to debug() won't print anything.
# debug(logger, 'A Debugging Message')
info(logger, 'Starting script')
# warn(logger, 'A Warning Message')
# error(logger, 'An Error Message')
# fatal(logger, 'A Fatal Error Message')


library(RSBID)
# Load variables environment previously saved in Rmd page
o_s_environment <- file.path(getwd(), "data", "o_s_environment_4_SMOTE.RData")
if (file.exists(o_s_environment)) {
  load(o_s_environment)
}
info(logger, paste("File", o_s_environment, "exists =", file.exists(o_s_environment)))

# To avoid the Warning message:
#  1: In RSBID::SMOTE_NC(data = o_s_training_df, outcome = "Revenue") :
#   The outcome is not a factor or character.
online_shoppers_df$Revenue <- as.factor(online_shoppers_df$Revenue)


info(logger, paste("training_partition_size =", training_partition_size))
info(logger, paste("number_of_tests =", number_of_tests))

# The algorithm SMOOTE_NC() spends around 15 minutes to create each training dataset,
# then to implement 100 datasets it could spend over 25 hours.
# We are able to start the SMOTE algorithm from the beginning, building 100 datasets.
# START_SMOTE_NC_FROM_BEGINNING <- TRUE
# Or we can reload previous data and continue this script from that point.
START_SMOTE_NC_FROM_BEGINNING <- FALSE

info(logger, paste("START_SMOTE_NC_FROM_BEGINNING =", START_SMOTE_NC_FROM_BEGINNING))

# File to save SMOTE_NC datasets
smote_datasets_file <- file.path(getwd(), "data", "o_s_SMOTE_NC_datasets.RData")

debug(logger, paste("smote_datasets_file (", smote_datasets_file, ") exists =", file.exists(smote_datasets_file)))

# Pattern to be used by grepl()
pattern_vars <- "SMOTE_NC_(train|test)_data"

if(START_SMOTE_NC_FROM_BEGINNING | !file.exists(smote_datasets_file)){
  debug(logger, "first_n_seed <- 1")
  # for() loop initialization
  first_n_seed <- 1
  # Create an empty vector to save every SMOTE_NC_train_data
  SMOTE_NC_train_data <- list()
  # Create an empty vector to save every SMOTE_NC_test_data
  SMOTE_NC_test_data <- list()
}else{
  # Load previous saved data
  if (file.exists(smote_datasets_file)) {
    debug(logger, "Load previous saved data")
    load(smote_datasets_file)
    # for() loop initialization
    first_n_seed <- length(SMOTE_NC_train_data) + 1
    if(first_n_seed > number_of_tests){
      log4r::warn(logger, paste("File", smote_datasets_file, "has", number_of_tests, "records yet. Can not create any more"))
      log4r::warn(logger, "Stoping the process")
      stop(paste("File", smote_datasets_file, "has", number_of_tests, "records yet. Can not create any more"))
      log4r::debug(logger, "YOU CAN NOT SEE THIS LINE -------------------")
    }
  }
}

# Create every training set
for (n_seed in first_n_seed:number_of_tests) {
  # Save init time
  init_time <- Sys.time()
  log4r::info(logger, paste("Init creation SMOTE training dataset number:", n_seed, "of", number_of_tests))
  
  cat(paste("\nCreate SMOTE", n_seed, "of", number_of_tests, "\n"))
  
  # Set seed for reproducible results
  set.seed(seeds[n_seed])
  # Create partition Revenue == TRUE
  o_s_T_training_rows <- sample(o_s_T_rows, training_partition_size*length(o_s_T_rows))
  # Create partition Revenue == FALSE
  o_s_F_training_rows <- sample(o_s_F_rows, training_partition_size*length(o_s_F_rows))
  # Create training set
  o_s_training_df <- online_shoppers_df[c(o_s_T_training_rows,o_s_F_training_rows),c(mod3_variables, "Revenue")]
  # Create testing set
  test_data <- online_shoppers_df[-c(o_s_T_training_rows,o_s_F_training_rows),c(mod3_variables, "Revenue")]
  
  require(RSBID)
  # Create a balanced dataset using the SMOTE-NC algorithm.
  # It returns the balanced training data 
  train_data <- RSBID::SMOTE_NC(data = o_s_training_df , outcome = "Revenue")
  
  # table(train_data$Revenue)
  # table(test_data$Revenue)
  
  # Undo the initial change
  train_data$Revenue <- as.logical(train_data$Revenue)
  test_data$Revenue <- as.logical(test_data$Revenue)
  
  
  # Append train_data and test_data
  SMOTE_NC_train_data[[n_seed]] <- train_data
  SMOTE_NC_test_data[[n_seed]] <- test_data
  
  # Print spent time
  spent_time <- print(Sys.time() - init_time)
  log4r::info(logger, paste("End of SMOTE training dataset number:", n_seed, "of", number_of_tests))
  log4r::info(logger, paste("             Spent", round(difftime(Sys.time(), init_time, units = "mins"), digits = 2) , "minutes" ))
  
  # Save every SMOTE training and testing data sets in an RData file.
  # It will be used later in the Rmd file.
  save(list = ls()[grepl(pattern_vars, ls())], file=smote_datasets_file)
  log4r::debug(logger, paste("Saved SMOTE dataset", n_seed, "of", number_of_tests))
}

#  Undo the initial change
online_shoppers_df$Revenue <- as.logical(online_shoppers_df$Revenue)

cat("\nEnd of SMOTE_NC\n")


# Show variables that we will use in SMOTE_NC_create_datasets.R
# ls()[grepl(pattern_vars, ls())]


if(file.exists(smote_datasets_file)){
  cat(paste("\nSaved file: (", smote_datasets_file, ")\n"))
}else{
  cat(paste("\nERROR. NOT SAVED  file: (", smote_datasets_file, ")\n"))
}

