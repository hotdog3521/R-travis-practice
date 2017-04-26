plot_measured_reference_raw <- function(time_series_measurements){

  filename <- paste0("output/measured_reference_raw",as.numeric(Sys.time()), ".pdf")
  pdf(filename, width=21, height=21)
  fileConn<-file("output.txt")
  writeLines(c(filename), fileConn)
  close(fileConn)
  
  par(mfrow=c(7,1))

    plot(time_series_measurements$time, time_series_measurements$measured_M0,type='l', xlab="Time (s)", ylab="M loadcell (N,green) | Motor Cmd (V, blue)", ylim=c(0,20))
    lines(time_series_measurements$time, time_series_measurements$reference_M0, col="green")
    lines(time_series_measurements$time, time_series_measurements$command_M0, col="lightblue")

    plot(time_series_measurements$time, time_series_measurements$measured_M1,type='l', xlab="Time (s)", ylab="M loadcell (N,green) | Motor Cmd (V, blue)", ylim=c(0,20))
    lines(time_series_measurements$time, time_series_measurements$reference_M1, col="green")
    lines(time_series_measurements$time, time_series_measurements$command_M1, col="lightblue")

    plot(time_series_measurements$time, time_series_measurements$measured_M2,type='l', xlab="Time (s)", ylab="M loadcell (N,green) | Motor Cmd (V, blue)", ylim=c(0,20))
    lines(time_series_measurements$time, time_series_measurements$reference_M2, col="green")
    lines(time_series_measurements$time, time_series_measurements$command_M2, col="lightblue")

    plot(time_series_measurements$time, time_series_measurements$measured_M3,type='l', xlab="Time (s)", ylab="M loadcell (N,green) | Motor Cmd (V, blue)", ylim=c(0,20))
    lines(time_series_measurements$time, time_series_measurements$reference_M3, col="green")
    lines(time_series_measurements$time, time_series_measurements$command_M3, col="lightblue")

    plot(time_series_measurements$time, time_series_measurements$measured_M4,type='l', xlab="Time (s)", ylab="M loadcell (N,green) | Motor Cmd (V, blue)", ylim=c(0,20))
    lines(time_series_measurements$time, time_series_measurements$reference_M4, col="green")
    lines(time_series_measurements$time, time_series_measurements$command_M4, col="lightblue")

    plot(time_series_measurements$time, time_series_measurements$measured_M5,type='l', xlab="Time (s)", ylab="M loadcell (N,green) | Motor Cmd (V, blue)", ylim=c(0,20))
    lines(time_series_measurements$time, time_series_measurements$reference_M5, col="green")
    lines(time_series_measurements$time, time_series_measurements$command_M5, col="lightblue")

    plot(time_series_measurements$time, time_series_measurements$measured_M6,type='l', xlab="Time (s)", ylab="M loadcell (N,green) | Motor Cmd (V, blue)", ylim=c(0,20))
    lines(time_series_measurements$time, time_series_measurements$reference_M6, col="green")
    lines(time_series_measurements$time, time_series_measurements$command_M6, col="lightblue")

  dev.off()
}

subsample_every_xth_row <- function(dataframe, n) {
  total_length <- length(dataframe[,1])
  return(dataframe[seq(1,total_length, by=n),])
}

#takes in a time series dataframe. make sure it has JR3 columns labelled correctly
plot_JR3_data <- function(a){

  par(mfcol=c(6,1))
  plot(a$time, offset_removal(a$JR3.FX,100), ylab="FX [N]", xlab="Time (s)", type='l', col="darkgrey", ylim=c(-0.5,0.5))
  plot(a$time, offset_removal(a$JR3.FY,100), ylab="FY [N]", xlab="Time (s)", type='l', col="darkgrey", ylim=c(-0.5,0.5))
  plot(a$time, offset_removal(a$JR3.FZ,100), ylab="FZ [N]", xlab="Time (s)", type='l', col="darkgrey", ylim=c(-0.5,0.5))
  plot(a$time, offset_removal(a$JR3.MX,100), ylab="MX [N]", xlab="Time (s)", type='l', col="darkgrey", ylim=c(-0.5,0.5))
  plot(a$time, offset_removal(a$JR3.MY,100), ylab="MY [N]", xlab="Time (s)", type='l', col="darkgrey", ylim=c(-0.5,0.5))
  plot(a$time, offset_removal(a$JR3.MZ,100), ylab="MZ [N]", xlab="Time (s)", type='l', col="darkgrey", ylim=c(-0.5,0.5))
}

#time series subsampling functions
offset_removal <- function(vector, n) {
  return(vector - median_of_first_values(vector,n))
}

median_of_first_values <- function(timeseries,n){
  return(median(timeseries[1:n]))
}


#error calculation functions
normalized_mean_squared_error <- function(timeseries) {
  return(sum(lapply(timeseries, squared_abs_diff)))
}

squared_abs_diff <- function(reference, loadcell) {
  return(abs(reference - loadcell)^2)
}

nonzero <- function(x) {
  return(x!=0)
}

#get time series of changed from index vector
get_time_from_index <- function(time, index_vector) {
  time_series <- vector()
  for(i in 1:length(index_vector)) {
    time_series <- c(time_series, index_vector[i])
  }
}

#combine three vectors to return as one vector
#1-boolean vector, 2-positive_index_vector, 3-negative_index_vector
combine_vectors <- function(mask, positive_index_vector, negative_index_vector) {
  combine_vector <- matrix(list(), nrow = 3, ncol = 1)
  combine_vector[[1,1]] <- mask
  combine_vector[[2,1]] <- positive_index_vector
  combine_vector[[3,1]] <- negative_index_vector
  combine
}
nonzero <- function(x) {
  return(x!=0)
}
detect_changes_index <- function(unidimensional_timeseries) {
  positive_index <- vector()
  negative_index <- vector()
  shifted_vector <- c(0,unidimensional_timeseries)
  held_vector <- c(unidimensional_timeseries, 0)
  difference_vector <- shifted_vector - held_vector
  difference_vector1 <- difference_vector[seq(2, length(difference_vector)-1)]
  #if it goes positive, it is going down next (time)
  #iterate through difference_vector1 to find if value is not 0
  for(i in 1:length(difference_vector1)) {
    if(difference_vector1[i] > 0) {
      positive_index <- c(positive_index, i)
    }else{
      negative_index <- c(negative_index, i)
    }
  }
  mask_of_nonzeros_with_padding <- do.call(c, lapply(difference_vector, nonzero))
  #return true when there's changes(either going to negative or positive)
  mask <- mask_of_nonzeros_with_padding[seq(2,length(mask_of_nonzeros_with_padding)-1)]
  #return vector contains three different vectors 
  return_vector <- combine_vectors(mask, positive_index_vector, negative_index_vector)
  return_vector
}
#only works for piecewise constant signals
detect_changes <- function(unidimensional_timeseries) {
  shifted_vector <- c(0,unidimensional_timeseries)
  held_vector <- c(unidimensional_timeseries, 0)
  difference_vector <- shifted_vector - held_vector
  mask_of_nonzeros_with_padding <- do.call(c, lapply(difference_vector, nonzero))
  mask <- mask_of_nonzeros_with_padding[seq(2,length(mask_of_nonzeros_with_padding)-1)]
  mask
}
analyze <- function(filename){
  dir.create("output", showWarnings = FALSE)
  #time_series_measurements <- read.csv(filepath, header=TRUE)
  subsampled_time_series <- subsample_every_xth_row(filename, 10)
  plot_measured_reference_raw(subsampled_time_series)

}
#Getting file from google drive
convert_file_readable <- function(address) {
	#creating new environment to store data
	env <- new.env()
	#donwload data
	data_from_google_drive <- load(url(address), env)[1]
	env[[data_from_google_drive]]
}
main <- function(address) {
	data <- convert_file_readable(address)
	analyze(data)
}
filenames_list <- c(
  "https://ndownloader.figshare.com/files/7853047"
  )
lapply(filenames_list, main)