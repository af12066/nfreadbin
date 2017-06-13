# Read a Binary File Generated from 0751

#' Parse a binary data
#'
#' @param filename filepath of a binary file
#' @export
#' 
read.nfBin <- function(filename) {
  get_filesize <- function(f) {
    if (file.exists(f)) {
      file.info(f)$size
    }
  }

  # バイナリを各サイズごとに抽出する関数
  read_string <- function(con, length=1L) {
    readChar(con, length, useBytes = TRUE)
  }
  read_long <- function(con, n=1L) {
    readBin(con, integer(), n = n, size = 4L, signed = TRUE, endian = "little")
  }
  read_int16 <- function(con, n=1L) {
    readBin(con, integer(), n = n, size = 2L, signed = TRUE, endian = "little")
  }
  read_double <- function(con, n=1L) {
    readBin(con, double(), n = n, signed = TRUE, endian = "little")
  }

  connection <- file(filename, "rb")
  filesize <- get_filesize(filename)

  read_string(connection, 12L) # "0751"
  read_string(connection, 6L) # Device Version
  read_string(connection, 18L) # "NF Corporation"
  Header4 <- read_long(connection) # 1000
  read_string(connection, 12L) # YYYY/MM/DD
  read_string(connection, 10L) # HH/mm/ss
  read_int16(connection) # number of available channels
  Header8 <- c() # empty vector
  for (i in 1:8) {
    Header8[i] <- read_string(connection, 6L) # "CH01", "CH02", ...
  }
  Header9 <- c()
  for (i in 1:8) {
    Header9[i] <- read_double(connection)
  }
  Header10 <- c()
  for (i in 1:8) {
    Header10[i] <- read_double(connection)
  }
  Header11 <- c()
  for (i in 1:8) {
    Header11[i] <- read_double(connection)
  }
  read_string(connection, 8L)
  read_string(connection, 8L)
  Header14 <- c()
  for (i in 1:8) {
    Header14[i] <- read_string(connection, 18L)
  }
  Header15 <- c()
  for (i in 1:8) {
    Header15[i] <- read_double(connection)
  }
  Header16 <- c()
  for (i in 1:8) {
    Header16[i] <- read_string(connection, 4L)
  }
  Header17 <- c()
  for (i in 1:8) {
    Header17[i] <- read_string(connection, 1L)
  }
  Header17 <- as.numeric(eval(parse(text = gsub("\\", "", deparse(Header17), fixed = TRUE))))
  read_string(connection, 200L)
  headersize <- seek(connection, origin = "current")
  sampling_rate <- Header4
  AD <- Header9
  offset <- Header10
  gain <- Header11
  EOgain <- Header15
  data_type <- Header17
  active_ch <- which(!is.na(data_type))
  number_of_ch <- length(active_ch)
  sampling_point <- (filesize - headersize) / 2 / number_of_ch
  Data <- matrix(0, nrow = number_of_ch, ncol = sampling_point)
  data_nf <- matrix(0, nrow = number_of_ch, ncol = sampling_point)
  for (i in 1:sampling_point) {
    for (j in 1:number_of_ch) {
      Data[j, i] <- read_int16(connection)
    }
  }

  for (ich in active_ch) {
    if (data_type[ich] == 1) {
      data_nf[ich, ] <- AD[ich] * (t(Data[ich, ]) * gain[ich] + offset[ich])
    }
    else if (data_type[ich] == 2) {
      data_nf[ich, ] <- t(Data[ich, ])
    } else {
      print("error")
      break()
    }
  }

  if (seek(connection, origin = "current") == filesize) {
    print("End successfully")
  } else {
    print("Unsuccessed! Check file!")
  }

  close(connection)
  data_nf <- as.data.frame(t(data_nf))
  colnames(data_nf) <- Header8[1:number_of_ch]
  return(data_nf)
}
