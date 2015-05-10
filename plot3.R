plot3 <- function() {
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
  
  # Convert Sub metering data from factor to numeric
  data$Sub_metering_1 <- as.numeric(as.character(data$Sub_metering_1))
  data$Sub_metering_2 <- as.numeric(as.character(data$Sub_metering_2))
  data$Sub_metering_3 <- as.numeric(as.character(data$Sub_metering_3))
  
  # Start png device
  png(filename = "plot3.png", width = 480, height = 480, units = "px", bg = "white")
  
  # create empty plot 
  plot( data$datetime, data$Sub_metering_1, type = "n", xlab = "", ylab = "Energy Sub Metering")
  
  # add data line
  lines(data$datetime, data$Sub_metering_1, type = "l")
  lines(data$datetime, data$Sub_metering_2, type = "l", col = "red")
  lines(data$datetime, data$Sub_metering_3, type = "l", col = "blue")
  
  # change x axis format
  axis.Date(1, at = xtics, labels = xlabs)
  
  # create legend
  legend( x = "topright", 
          legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
          col = c("black","red","blue"),
          lty = par("lty"),
          bty = "0",
          box.col = "black"
          )
  
  # Save PNG
  dev.off()
}