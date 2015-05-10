plot4 <- function() {
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
  
  # Convert data from factor to numeric
  data$Global_active_power <- as.numeric(as.character(data$Global_active_power))
  data$Sub_metering_1 <- as.numeric(as.character(data$Sub_metering_1))
  data$Sub_metering_2 <- as.numeric(as.character(data$Sub_metering_2))
  data$Sub_metering_3 <- as.numeric(as.character(data$Sub_metering_3))
  data$Global_reactive_power <- as.numeric(as.character(data$Global_reactive_power))
  data$Voltage <- as.numeric(as.character(data$Voltage))
  
  # Start png device
  png(filename = "plot4.png", width = 480, height = 480, units = "px", bg = "white")
  
  # split plot area into 4
  par(mfcol = c(2,2))
  
  # Top left plot
    # create empty plot 
    plot( data$datetime, data$Global_active_power, type = "n", xlab = "", ylab = "Global Active Power (kilowatts)")
  
    # add data line
    lines(data$datetime, data$Global_active_power, type = "l")
    
    # change x axis format
    axis.Date(1, at = xtics, labels = xlabs)
  
  # Bottom Left plot
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
            bty = "n"
            )
  
  # Top right plot
    # plot line
    plot( data$datetime, data$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
  
    #change x axis format
    axis.Date(1, at = xtics, labels = xlabs)
  
  # Bottom Right plot
    # create empty plot
    plot( data$datetime, data$Global_reactive_power, type = "l", ylab = "Global_reactive_power", xlab = "datetime")
  
  
  # Save PNG
  dev.off()
}