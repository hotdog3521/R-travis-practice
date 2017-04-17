require(RGoogleDocs)

upload_to_google_drive <- function(file) {
	#get connection to google drive
	auth <- getGoogleAuth("hotdog3521@gmail.com", "XOrua434!@!")
	con  <- getGoogleDocsConnection(auth)
	#upload data(pdf) to google drive
	done <- uploadDoc("", con, name = "boo3", type = "pdf")
     

	done <- uploadDoc(content = pdf, con = con, name = file, type = "pdf")
}



plot_measured_reference_raw <- function(time_series_measurements){

  pdf(paste0("measured_reference_raw",as.numeric(Sys.time()), ".pdf"), width=21, height=21)
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
	upload_to_google_drive()
}
filenames_list <- c(
  "https://ndownloader.figshare.com/files/7853047"
  )
lapply(filenames_list, main)

#Creating directory 

