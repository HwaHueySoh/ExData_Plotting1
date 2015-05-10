plot2 <- function() {
  # Read data file
  data <- read.table("Data/household_power_consumption.txt", header = TRUE, sep = ";")
  
  # Combine date and time columns
  data$datetime <- paste(data$Date, data$Time, sep = " ")
  
  # Convert the combined datatime column into the Date/Time class
  data$datetime <- strptime(data$datetime, "%d/%m/%Y %H:%M:%S")
  
  # Defining bounds of data rows used
  datebound <- strptime( c("2007-02-01","2007-02-03"), "%Y-%m-%d")
  
  # prepare series for x axis labelling
  xtics <- seq(as.Date(datebound[1]), as.Date(datebound[2]), "days")
  xlabs <- strftime( xtics, "%a")
  
  # Remove data from outside bounds
  data <- subset(data, data$datetime >= datebound[1])
  data <- subset(data, data$datetime < datebound[2])
  
  # Convert Global_active_power data from factor to numeric
  data$Global_active_power <- as.numeric(as.character(data$Global_active_power))
  
  # Start png device
  png(filename = "plot2.png", width = 480, height = 480, units = "px", bg = "white")
  
  # create empty plot 
  plot( data$datetime, data$Global_active_power, type = "n", xlab = "", ylab = "Global Active Power (kilowatts)")
  
  # add data line
  lines(data$datetime, data$Global_active_power, type = "l")
  
  # change x axis format
  axis.Date(1, at = xtics, labels = xlabs)
  
  # Save PNG
  dev.off()
}