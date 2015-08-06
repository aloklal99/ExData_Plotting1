require(lubridate)
require(dplyr)

## Reads in a file and returns only as an array lines only records for 1/2/2007 and 2/2/2007
# input : file with all records
# output : character array which are for 1st 2 days of Feb 2007 as required by the problem
filter_lines <- function(input) {
  result <- list(); i <- 1
  in_con <- file(input, "rt")
  a_line <- readLines(in_con, n = 1)
  first_line_captured <- 0
  while(length(a_line) == 1) {
    if (sum(grep("^0?[12]/2/2007", x = a_line)) > 0) {
      result[i] <- a_line
      i <- i + 1
    } 
    if (first_line_captured == 0) {
      result[i] <- a_line
      i <- i + 1
      first_line_captured <- 1
    }
    a_line <- readLines(in_con, n = 1)
  }
  close(in_con)
  return(as.character(result))
}

## reads the file into a dataset and then messages the dataset to get it ready for plotting
# input: fileName - name of the file to read in
# result: dataset
create_dataset <- function(an_array) {
  # create base dataset
  power.ds <- read.csv(textConnection(an_array), sep = ";", na.strings = "?")
  # convert date columns
  power.ds <- mutate(power.ds, t = dmy_hms(paste(Date, Time)))
}

# functions that plot each of the 4 plots
create_plot_11 <- function(dataset) {
  plot(dataset$t, dataset$Global_active_power, 
       type = "l",
       xlab = "",
       ylab = "Global Active Power (kilowats)")
}

create_plot_12 <- function(dataset) {
  plot(dataset$t, dataset$Voltage, 
       type = "l",
       xlab = "datetime",
       ylab = "Voltage")
}

create_plot_21 <- function(dataset) {
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
}

create_plot_22 <- function(dataset) {
  plot(dataset$t, dataset$Global_reactive_power, 
       type = "l",
       xlab = "datetime",
       ylab = "Global_reactive_power")
}

# create a temporary file with data for the first 2 days in Feb, 2007
filtered_lines <- filter_lines("household_power_consumption.txt")
# create the dataset
power.ds <- create_dataset(filtered_lines)

# plot the data
png('plot4.png', bg = "transparent")
par(mfrow = c(2, 2))
create_plot_11(power.ds)  # 1st row, 1st plot
create_plot_12(power.ds)  # 1st row, 2nd plot
create_plot_21(power.ds)  # 2nd row, 3rd plot
create_plot_22(power.ds)  # 2nd row, 4th plot
dev.off()
