# functions called and used by sunmoon_ill.r

#' Convert input value to degrees
#' 
#' This function converts the input value to degrees based on the specified conversion formula.
#'
#' @param x A numeric value to be converted to degrees
#' @return A numeric value representing the input value in degrees
#' @export
#' @examples
#' in_f_DEG(x=35.6)
#' degrees <- in_f_DEG(x=75.4)
#' degrees
#' 
#' # Output:
#' # [1] 75.6

in_f_DEG <- function(x){ # Function in_f_DEG created based on DEG function in the original FORTRAN code
	y <- as.integer(x)+((x-as.integer(x))*10.0)/6.0
	return(y)
	}

#' Calculate sun position
#' 
#' This function calculates the sun's position based on the input values.
#'
#' @param D A numeric value representing the number of days
#' @param DR A numeric value representing the conversion factor from degrees to radians
#' @param RD A numeric value representing the conversion factor from radians to degrees
#' @param CE A numeric value representing the cosine of the obliquity of the ecliptic
#' @param SE A numeric value representing the sine of the obliquity of the ecliptic
#' @return A list containing the following calculated values:
#' \itemize{
#'   \item T: The sun's true anomaly
#'   \item G: The sun's mean anomaly
#'   \item LS: The sun's ecliptic longitude
#'   \item AS: The sun's right ascension
#'   \item SD: The sine of the sun's declination
#'   \item DS: The sun's declination
#' }
#' @export
#' @examples
#' sun_position <- sun(D=1, DR=pi/180, RD=180/pi, CE=0.91775, SE=0.39715)

sun <- function(D,DR,RD,CE,SE){
	T <- 280.46+0.98565*D
	T <- T-as.integer(T/360.0)*360.0
	if (T < 0.0) T <- T+360.0
	G <- (357.5+0.98560*D)*DR
	LS <- (T+1.91*sin(G))*DR
	AS <- atan(CE*tan(LS))*RD
	Y <- cos(LS)
	if (Y < 0.0) AS <- AS+180.0
	SD <- SE*sin(LS)
	DS <- asin(SD)
	T <- T-180.0
	return(c(T, G, LS, AS, SD, DS))
	}

#' Calculate moon position
#' 
#' This function calculates the moon's position based on the input values.
#'
#' @param D A numeric value representing the number of days
#' @param G A numeric value representing the mean anomaly of the sun
#' @param CE A numeric value representing the cosine of the obliquity of the ecliptic
#' @param SE A numeric value representing the sine of the obliquity of the ecliptic
#' @param RD A numeric value representing the conversion factor from radians to degrees
#' @param DR A numeric value representing the conversion factor from degrees to radians
#' @return A vector containing the following calculated values:
#' \itemize{
#'   \item V: The moon's true anomaly
#'   \item SD: The sine of the moon's declination
#'   \item AS: The moon's right ascension
#'   \item DS: The moon's declination
#'   \item CB: The cosine of the moon's latitude
#' }
#' @export
#' @examples
#' moon_position <- moon(D=1, G=1, CE=0.91775, SE=0.39715, RD=180/pi, DR=pi/180)
#' moon_position
#' 

moon <- function(D,G,CE,SE,RD,DR){
	V <- 218.32+13.1764*D
	V <- V-as.integer(V/360.0)*360.0
	if (V < 0.0) V <- V+360.0
	Y <- (134.96+13.06499*D)*DR
	O <- (93.27+13.22935*D)*DR
	W <- (235.7+24.38150*D)*DR
	SB <- sin(Y)
	CB <- cos(Y)
	X <- sin(O)
	S <- cos(O)
	SD <- sin(W)
	CD <- cos(W)
	V <- (V+(6.29-1.27*CD+0.43*CB)*SB+(0.66+1.27*CB)*SD-0.19*sin(G)-0.23*X*S)*DR
	Y <- ((5.13-0.17*CD)*X+(0.56*SB+0.17*SD)*S)*DR
	SV <- sin(V)
	SB <- sin(Y)
	CB <- cos(Y)
	Q <- CB*cos(V)
	P <- CE*SV*CB-SE*SB
	SD <- SE*SV*CB+CE*SB
	AS <- atan(P/Q)*RD
	if (Q < 0.0) AS <- AS+180.0
	DS <- asin(SD)
	return(c(V, SD, AS, DS, CB)) 
	}

#' Calculate altaz
#' 
#' This function calculates the altitude and azimuth of the sun or moon based on the input values.
#'
#' @param DS A numeric value representing the declination of the sun or moon
#' @param H A numeric value representing the hour angle of the sun or moon
#' @param SD A numeric value representing the sine of the declination of the sun or moon
#' @param CI A numeric value representing the cosine of the latitude of the observer
#' @param SI A numeric value representing the sine of the latitude of the observer
#' @param DR A numeric value representing the conversion factor from degrees to radians
#' @param RD A numeric value representing the conversion factor from radians to degrees
#' @return A vector containing the calculated values for H and AZ
#' @export
#' @examples
#' altaz(DS=1, H=1, SD=1, CI=1, SI=1, DR=pi/180, RD=180/pi)
#' altaz_values <- altaz(DS=1, H=1, SD=0.39715, CI=0.91775, SI=0.39715, DR=pi/180, RD=180/pi)
#' altaz_values

altaz <- function(DS,H,SD,CI,SI,DR,RD){
	CD <- cos(DS)
	CS <- cos(H*DR)
	Q <- SD*CI-CD*SI*CS
	P <- -CD*sin(H*DR)
	AZ <- atan(P/Q)*RD
	if (Q < 0.0) AZ <- AZ+180.0
	if (AZ < 0.0) AZ <- AZ+360.0
	AZ <- as.integer(AZ+0.5)
	H <- asin(SD*SI+CD*CI*CS)*RD
	return(c(H,AZ))
	}


#' Calculate refraction correction for solar or lunar altitude
#' 
#' This function calculates the correction for refraction of the solar or lunar altitude based on the input values.
#'
#' @param H A numeric value representing the solar or lunar altitude
#' @param DR A numeric value representing the conversion factor from degrees to radians
#' @return A numeric value representing the correction for refraction of the solar or lunar altitude
#' @export
#' @examples
#' refr(H=45, DR=0.0174533)
#' refraction_correction <- refr(H=20, DR=0.0174533)
#' refraction_correction
#' 
#' # Output:
#' # [1] 25.634

refr <- function(H,DR){
  if (H < (-5.0/6.0)) {
    HA <- H
  } else {
    HA <- H+1.0/(tan((H+8.6/(H+4.42))*DR))/60.0	
  }
	return(HA)
	}


#' Calculate atmospheric refraction correction
#' 
#' This function calculates the correction for atmospheric refraction based on the input values.
#'
#' @param HA A numeric value representing the solar or lunar altitude
#' @param DR A numeric value representing the conversion factor from degrees to radians
#' @return A numeric value representing the correction for atmospheric refraction
#' @export
#' @examples
#' atmos(HA=45, DR=0.0174533)
#' atmospheric_refraction_correction <- atmos(HA=20, DR=0.0174533)
#' atmospheric_refraction_correction
#' 
#' # Output:
#' # [1] 0.4857363

atmos <- function(HA,DR){
	U <- sin(HA*DR)
	X <- 753.66156
	S <- asin(X*cos(HA*DR)/(X+1.0))                                                            
	M <- X*(cos(S)-U)+cos(S)
	M <- exp(-0.21*M)*U+0.0289*exp(-0.042*M)*(1.0+(HA+90.0)*U/57.29577951)
	return(M)
	}

