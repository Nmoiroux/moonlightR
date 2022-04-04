library(dplyr)
library(lubridate)

addZero <- function(x){
  rep("0",4-nchar(x)) %>%
    paste(collapse = "") %>%
    paste0(x)
}

lon <- 35.5
lat <- 46
date <- "1986-07-03"
hour <- "-1"
TZ <- 1
sky <- 1

LO <- lon # lon
F <- lat # lat
date <- lubridate::ymd(date)
IY <- lubridate::year(date) # Year
IM <- lubridate::month(date) # month
ID <- lubridate::day(date) # day
Z <- TZ # 0 -> TIME IS UT (GMT) ; 1 -> TIME IS STANDARD (ZONE) TIME' ; 2 <-> TIME IS LOCAL MEAN TIME'
H <- as.integer(hour) # hhmm (hour)
SK <- sky # INPUT SKY CONDITION' 1 <-> SUN/MOON VISIBLE, SKY < 70% OVERCAST''2 => SUN/MOON OBSCURED BY THIN CLOUDS '3 => SUN/MOON OBSCURED BY AVERAGE CLOUDS   '1O => SUN/MOON OBSCURED BY DARK STRATUS CLOUDS'

paramConn <- file("parameters.txt")
writeLines(c(as.character(LO),
             as.character(F),
             as.character(IY),
             as.character(IM),
             as.character(ID),
             as.character(H),
             as.character(Z),
             as.character(SK)), paramConn)
close(paramConn)

system("./a.out")

resConn <- file("results.txt")
res <- readLines(resConn) %>%
  gsub(pattern = " ",replacement = "", x = .)
close(resConn)

res_df <- data.frame(matrix(ncol = length(res), nrow = 0))
res_df[1,] <- res

if (length(res) == 8) {
  cname <- c("Solar Azimuth",
              "Solar Altitude",
              "Solar Illuminance",
              "Lunar Azimuth",
              "Lunar Altitude",
              "Lunar Illuminance",
              "% Moon illuminated",
              "Total Illuminance")
  res_df <- res_df %>%
    mutate_at(.vars = c(1,2,4,5,7), .funs=as.integer) %>%
    mutate_at(.vars = c(3,6,8), .funs=as.numeric)
  } else if (length(res) == 13){
    cname <- c("Time Sun Meridian",
               "Altitude Sun Meridian",
               "Time Sunrise",
               "Time Sunset",
               "Duration Daylight",
               "Beg. Civil Twilight",
               "End Civil Twilight",
               "Beg. Nautical Twilight",
               "End Nautical Twilight",
               "Time Lunar Meridian",
               "Altitude Lunar Meridian",
               "Time Moonrise",
               "Time Moonset")
    res_df <- res_df %>%
      mutate_at(.vars = c(2,11), .funs = as.integer) %>%
      mutate_at(.vars = -c(2,11), .funs = addZero )
  }

colnames(res_df) <- cname
