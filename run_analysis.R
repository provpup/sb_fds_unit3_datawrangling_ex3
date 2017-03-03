# install.packages('dplyr')
# install.packages('tidyr')
# install.packages('readr')
library(dplyr)
library(tidyr)
library(readr)

compute_average_summary_by_activity <- function (data) {
  summary <- data %>%
    group_by(ActivityLabel) %>%
    summarise_all(mean)
  return(summary)
}

compute_average_summary_by_subject <- function (data) {
  summary <- data %>%
    group_by(Subject) %>%
    mutate(activity_code = NA) %>%
    summarise_all(mean)
  return(summary)
}

# Clean data function for a particular data set
clean_data <- function (dataset_name, columns, activity_codes, directory_root) {
  # Read the measurement data using the column names computed from the features file
  directory <- paste(directory_root, dataset_name, '/', sep = "")
  x_file <- paste(directory, 'X_', dataset_name, '.txt', sep = "")
  data <- read_fwf(x_file, fwf_empty(x_file, col_names = columns))
  # Keep only those columns whose name contains either 'mean' or 'std'
  data <- data[, grep('mean|std', colnames(data))]
  
  # Combine together the activity data and the subject data
  training_activity <- scan(paste(directory, 'y_', dataset_name, '.txt', sep = ""),
                            what = list(activity_code = 0))
  training_activity <- tbl_df(training_activity)
  subjects <- scan(paste(directory, 'subject_', dataset_name, '.txt', sep = ""),
                   what = list(Subject = ""))
  subjects <- tbl_df(subjects)
  training_activity <- training_activity %>%
    inner_join(activity_codes, by = "activity_code") %>%
    bind_cols(subjects)
  
  # Combine the measurement data with the activity + subject data
  data <- data %>%
    tbl_df() %>%
    bind_cols(training_activity)
  
  # Compute the average values by subject and average values by activity
  # and then combine them into one data set
  data <- bind_rows(compute_average_summary_by_activity(data),
                    compute_average_summary_by_subject(data)) %>%
    mutate(dataset = dataset_name)
  
  return(data)
}


directory <- './UCI HAR Dataset/'
all_activity_codes <- scan(paste(directory, 'activity_labels.txt', sep = ""),
                           what = list(activity_code = 0, ActivityLabel = ""))
all_activity_codes <- tbl_df(all_activity_codes)

# Create unique column names from the features data
features <- scan(paste(directory, 'features.txt', sep = ""), what = list(number = 0, name = character()))
measurement_columns <- make.names(features$name, unique = TRUE)

training_data <- clean_data('train', measurement_columns, all_activity_codes, directory)
test_data <- clean_data('test', measurement_columns, all_activity_codes, directory)

write_csv(training_data, './training_data_mean.csv')
write_csv(test_data, './test_data_mean.csv')
