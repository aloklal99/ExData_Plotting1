# filters the records and writes them to a temporary file
# input : file with all records
# output : temporary file which contains only records for 1/2/2007 and 2/2/2007
filter_lines <- function(input) {
  result <- tempfile()
  in_con <- file(input, "rt")
  out_con <- file(result, "wt")
  a_line <- readLines(in_con, n = 1)
  wrote_first_line <- 0
  while(length(a_line) == 1) {
    # if it is 1st two days in February, 2007 then write them out
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

# create a temporary file with data for just the 2 days
filtered_data_filename <- filter_lines("household_power_consumption.txt")
# create the dataset
power.ds <- read.csv(filtered_data_filename, sep = ";", na.strings = "?")
# plot the histogram
png("plot1.png", bg = "transparent")
hist(power.ds$Global_active_power, 
     col = "red", 
     main = "Global Active Power",
     xlab = "Global Active Power (kilowats)")
dev.off()
