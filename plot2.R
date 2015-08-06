# filters the records and writes them to a temporary file
# input : file with all records
# output : temporary file which contains only records for 1/2/2007 and 2/2/2007

require(lubridate)
require(dplyr)

# the data.set

filter_lines <- function(input) {
  result <- tempfile()
  in_con <- file(input, "rt")
  out_con <- file(result, "wt")
  a_line <- readLines(in_con, n = 1)
  wrote_first_line <- 0
  while(length(a_line) == 1) {
    if (sum(grep("^[12]/2/2007", x = a_line)) > 0) {
      writeLines(a_line, out_con)
    } 
    if (wrote_first_line == 0) {
      writeLines(a_line, out_con)
      wrote_first_line <- 1
    }
    a_line <- readLines(in_con, n = 1)
  }
  close(in_con)
  close(out_con)
  return(result)
}

create_dataset <- function(fileName) {
  # create base dataset
  power.ds <- read.csv(filtered_data_filename, sep = ";", na.strings = "?")
  # convert date columns
  power.ds <- mutate(power.ds, t = dmy_hms(paste(Date, Time)))
}

create_plot <- function(dataset, fileName) {
  png(fileName, bg = "transparent")
  # plot the graph
  plot(dataset$t, dataset$Global_active_power, 
       type = "l",
       xlab = "",
       ylab = "Global Active Power (kilowats)")
  dev.off()
}

# create a temporary file with data for the first 2 days in Feb, 2007
filtered_data_filename <- filter_lines("household_power_consumption.txt")
# create the dataset
power.ds <- create_dataset(filtered_data_filename)
create_plot(power.ds, "plot2.png")
