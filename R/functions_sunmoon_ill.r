# functions called and used by sunmoon_ill.r

in_f_DEG <- function(x){ # Function in_f_DEG created based on DEG function in the original FORTRAN code
	y <- as.integer(x)+((x-as.integer(x))*10.0)/6.0
	return(y)
	}

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

refr <- function(H,DR){
  if (H < (-5.0/6.0)) {
    HA <- H
  } else {
    HA <- H+1.0/(tan((H+8.6/(H+4.42))*DR))/60.0	
  }
	return(HA)
	}

atmos <- function(HA,DR){
	U <- sin(HA*DR)
	X <- 753.66156
	S <- asin(X*cos(HA*DR)/(X+1.0))                                                            
	M <- X*(cos(S)-U)+cos(S)
	M <- exp(-0.21*M)*U+0.0289*exp(-0.042*M)*(1.0+(HA+90.0)*U/57.29577951)
	return(M)
	}

