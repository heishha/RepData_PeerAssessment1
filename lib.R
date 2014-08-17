checkDownloadFile <- function( url, dataDir = "./exdata", saveFilename  ) {
  if ( !file.exists(dataDir) ) {
    dir.create( dataDir )
  }

  ## download and unzip data file if it doesn't already exist
  destFilename <- paste( dataDir, "/", saveFilename, sep = "" )
  timestampFilename <- paste( dataDir, "/", saveFilename, ".downloadTimestamp", sep = "" )
  if ( !file.exists(destFilename) ) {
    fileUrl <- url
    download.file( fileUrl, destFilename, method = "curl" )
    ## record timestamp
    dateDownloaded <- format(Sys.time(), "%Y-%m-%d.%H:%M")
    write( dateDownloaded, file = timestampFilename )
  }
  destFilename
}

checkDownloadUnzipFile <- function( url, dataDir = "./exdata", saveFilename  ) {
  destFilename <- checkDownloadFile( url, dataDir, saveFilename )
  unzip( destFilename, exdir = dataDir )

  destFilename
}
