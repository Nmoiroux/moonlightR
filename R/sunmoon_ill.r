#' Get sun and moon illuminance
#'
#' @param lon Longitude in decimal degrees, numeric
#' @param lat Latitude in decimal degrees, numeric
#' @param date Date (including hour of the day) in a format that is recognized by function ymd_hms of the lubridate package, character string or date
#' time in Universal Time (UTC) should be preferred (and parameter TZ set to 0)
#' @param TZ Time Zone of date parameter (0 => UTC ; 1 => standard zone time ; 2=> local mean time), integer
#' @param sky Sky condition (1 => sun/moon visible, cloud cover < 70\% ; 2 => sun/moon obscured by thin clouds ; 3 => sun/moon obscured by average clouds ; (...) 10=> sun/moon obscured by dark stratus clouds)
#' illuminance is divided by the number provided.
#'
#' @return
#' a list with the following elements:
#' \itemize{
#'   \item $sun_illuminance : sun illuminance (lux)
#'   \item $sun_azimuth : sun azimuth (deg)
#'   \item $sun_alt : sun altitude (deg)
#'   \item $moon_illuminance : moon illuminance (lux)
#'   \item $moon_azimuth : moon azimuth (deg)
#'   \item $moon_alt : moon altitude (deg)
#'   \item $moon_pct_ill : proportion of moon illuminated (\%)
#'   \item $total_illuminance : total illuminance (lux)
#' }
#'
#' @import lubridate
#'
#' @export
#'
#' @examples
#' # Test cases for program certification (see https://ui.adsabs.harvard.edu/abs/1987USNOC.171.....J p.15)
#' sunmoonIlluminance(-135.8,-23.4,"19861218210000",0,1)
#' sunmoonIlluminance(39.5,21.3,"19880813183100",2,1)
#'
#' # various date format
#' sunmoonIlluminance(-135.8,-23.4,"1986-12-18 21:00:00",0,1)
#' sunmoonIlluminance(-135.8,-23.4,"1986-12-18-21-00-00",0,1)
#' sunmoonIlluminance(-135.8,-23.4,"1986/12/18 21:0000",0,1)
#'
#'
#'
sunmoonIlluminance <- function(lon,lat,date,TZ = 0,sky = 1){
  # parameters

  LO <- lon # lon
  F <- lat # lat
  date <- lubridate::ymd_hms(date)
  IY <- lubridate::year(date) # Year
  IM <- lubridate::month(date) # month
  ID <- lubridate::day(date) # day
  Z <- TZ # 0 -> TIME IS UT (GMT) ; 1 -> TIME IS STANDARD (ZONE) TIME' ; 2 <-> TIME IS LOCAL MEAN TIME'
  H <- lubridate::hour(date)*100+lubridate::minute(date) # hhmm (hour)
  SK <- sky # INPUT SKY CONDITION' 1 <-> SUN/MOON VISIBLE, SKY < 70% OVERCAST''2 => SUN/MOON OBSCURED BY THIN CLOUDS '3 => SUN/MOON OBSCURED BY AVERAGE CLOUDS   '1O => SUN/MOON OBSCURED BY DARK STRATUS CLOUDS'

  # constant values
  RD <- 57.29577951
  CE <- 0.91775
  SE <- 0.39715
  C <- 360.0

  # run program
  DR <- 1.0/RD
  LI <- abs(LO)
  FO <- F
  F <- F*DR
  SI <- sin(F)
  CI <- cos(F)

  J <- 367*IY-as.integer(7*(IY+as.integer((IM+9)/12))/4)+as.integer(275*IM/9)+ID-730531

  DT <- 0
  if(Z == 0.0) DT <- -LO/360.0
  if(Z == 1.0) DT <- (LI-15.0*as.integer((LI+7.5)/15.0))/C * sign(-LO)*1


  IH <- as.integer(H)
  E <- in_f_DEG(H/100.0)/24.0-DT-LO/360.0
  D <- J-0.5+E

  ###### sun calculations
  N <- 1

  out_sun <- sun(D,DR,RD,CE,SE)

  T <- out_sun[1]
  G <- out_sun[2]
  LS <- out_sun[3]
  AS <- out_sun[4]
  SD <- out_sun[5]
  DS <- out_sun[6]

  T <- T+360.0*E+LO
  H <- T-AS

  out_altaz <- altaz(DS,H,SD,CI,SI,DR,RD)
  H <- out_altaz[1]
  AZ <- out_altaz[2]

  Z <- H*DR
  H <- H-0.95*(N-1)*cos(H*DR)


  HA <- refr(H,DR)

  M <- atmos(HA,DR)

  HA <- sign(HA)*as.integer(abs(HA)+0.5)
  SIS <- IS <- 133775.0*M/SK # SOLAR ILLUMINANCE (LUX)
  SIAZ <- IAZ <- as.integer(AZ) # SOLAR AZIMUTH (DEG.)
  SIHA <- IHA <- as.integer(HA) # SOLAR ALTITUDE (DEG.)

  ###### moon calculations
  N<-2


  moon_out <- moon(D,G,CE,SE,RD,DR)
  V <- moon_out[1]
  SD <- moon_out[2]
  AS <- moon_out[3]
  DS <- moon_out[4]
  CB <- moon_out[5]

  H <- T-AS

  out_altaz <- altaz(DS,H,SD,CI,SI,DR,RD)
  H <- out_altaz[1]
  AZ <- out_altaz[2]


  Z <- H*DR
  H <- H-0.95*(N-1)*cos(H*DR)

  HA <- refr(H,DR)

  M <- atmos(HA,DR)

  HA <- sign(HA)*as.integer(abs(HA)+0.5)
  E <- acos(cos(V-LS)*CB)
  P <- 0.892*exp(-3.343/((tan(E/2.0))^0.632))+0.0344*(sin(E)-E*cos(E))
  P <- 0.418*P/(1.0-0.005*cos(E)-0.03*sin(Z))
  IL <- P*M/SK # LUNAR ILLUMINANCE (LUX)
  IS <- IS+IL+0.0005/SK # TOTAL ILLUMINANCE (LUX)
  IAZ <- as.integer(AZ) # LUNAR AZIMUTH (DEG.)

  IHA <- as.integer(HA) #  LUNAR ALTITUDE (DEG.)
  IHA_pct <- as.integer(50.*(1.0-cos(E))+0.5) # % OF MOON IS ILLUMINATED)

  return(list(sun_illuminance = SIS, sun_azimuth = SIAZ, sun_alt = SIHA,
              moon_illuminance = IL, moon_azimuth = IAZ, moon_alt = IHA, moon_pct_ill = IHA_pct,
              total_illuminance = IS))

  }




