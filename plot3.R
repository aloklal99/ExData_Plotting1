require(lubridate)
require(dplyr)

## filters the records and writes them to a temporary file
# input : file with all records
# output : temporary file which contains only records for 1/2/2007 and 2/2/2007
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

## reads the file into a dataset and then messages the dataset to get it ready for plotting
# input: fileName - name of the file to read in
# result: dataset
create_dataset <- function(fileName) {
  # create base dataset
  power.ds <- read.csv(filtered_data_filename, sep = ";", na.strings = "?")
  # convert date columns
  power.ds <- mutate(power.ds, t = dmy_hms(paste(Date, Time)))
}

## Takes the dataset along with the output file name in which the plot is to be created.
create_plot <- function(dataset, fileName) {
  png(fileName, bg = "transparent")
  # plot the graph
  plot(dataset$t, dataset$Sub_metering_1, 
       type = "l", # Don't draw it yet
       xlab = "",
       ylab = "Energy Sub metering")
  lines(x = power.ds$t, y = power.ds$Sub_metering_1, type = 'l')
  lines(x = power.ds$t, y = power.ds$Sub_metering_2, type = 'l', col='red')
  lines(x = power.ds$t, y = power.ds$Sub_metering_3, type = 'l', col='blue')
  legend("topright", 
         c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
         col = c("black", "red", "blue"), 
         lty = c(1, 1, 1))
  dev.off()
}

# create a temporary file with data for the first 2 days in Feb, 2007
filtered_data_filename <- filter_lines("household_power_consumption.txt")
# create the dataset
power.ds <- create_dataset(filtered_data_filename)
create_plot(power.ds, "plot3.png")
