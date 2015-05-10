plot1 <- function() {
  # Read data file
  data <- read.table("Data/household_power_consumption.txt", header = TRUE, sep = ";")
  
  # Combine date and time columns
  data$datetime <- paste(data$Date, data$Time, sep = " ")
  
  # Convert the combined datatime column into the Date/Time class
  data$datetime <- strptime(data$datetime, "%d/%m/%Y %H:%M:%S")
  
  # Defining bounds of data rows used
  datebound <- strptime( c("2007-02-01","2007-02-03"), "%Y-%m-%d")
  
  # Remove data from outside bounds
  data <- subset(data, data$datetime >= datebound[1])
  data <- subset(data, data$datetime < datebound[2])
  
  # Convert Global_active_power data from factor to numeric
  data$Global_active_power <- as.numeric(as.character(data$Global_active_power))
  
  # Start png device
  png(filename = "plot1.png", width = 480, height = 480, units = "px", bg = "white")
  
  # plot 
  hist(data$Global_active_power, col = "red", xlab ="Global Active Power (kilowatts)", main = "Global Active Power")
  
  # Save PNG
  dev.off()
  
}