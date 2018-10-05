# Code to obtain DOP (ENU) from Almanac Data
# By Yuen Ming Fatt
# Last updated on 27 Feb 2009
#**********************************************************************
#**********************************************************************
#clear all
#clc
#format long g

#*********************************************************
#USER INPUTS
#*********************************************************
#Target's Latitude, Longitude and Altitude Input
#lat_deg = 0; #Latitude (degree) (user input)#####
#lon_deg = 90; #Longitude (degree) (user input)#####
#alt = 0; #Altitude (meter) (user input)#####
#Obstruction = 10; #Obstruction to the Field of View (deg) (user input)#####
#
#Input time from user#####
#y = 2018; #Year
#m = 10; #Month
#d = 4; #Date
#h = 0; #Hours
#mi = 0; #Minutes
#sec = 0; #Seconds
#Inputs from original matlab code - currently not handled by ymdhms2gps
#timezone = 0; #Timezone (Eastern Standard Time (North America) = -5hr)
#summertime = 0; #To account for daylight saving. If summer, 1 = Yes, 0 = No #currently ignored!
#leapsec = 0; #leap second #currently ignored!



#' Calculate expected DOP for a location
#'
#' @param lat_deg target position latitude in degrees N
#' @param lon_deg target position longitude in degrees E
#' @param alt altitude (above sealevel) in meters
#' @param y year
#' @param m month
#' @param d day of month
#' @param h hour
#' @param mi minute
#' @param sec second
#' @param Obstruction obstruction angle to horizon in degrees. this is currently assumed to be uniform around the target location
#' @param almanac_file path to an appropriate almanace file. If NULL (default)
#'
#' @return a named list
#' @export
#'
#' @examples
#' calculate_dop(0,90,0,2018,10,4,0,0,0)
#'
calculate_dop <- function(lat_deg, lon_deg, alt, y, m, d, h, mi, sec, Obstruction = 0, almanac_file = NULL){
#**********************************************************************
#Convert Target's Latitude, Longitude and Altitude to ECEF Coordinates
#**********************************************************************
#constants to be deprecated
sealevel = 0;
#angular conversion
lat = lat_deg*pi/180; #Latitude (rad)
lon = lon_deg*pi/180; #Longitude (rad)
#-------------------------
#WGS84 ellipsoid constants
a = 6378137;
es = 8.1819190842622e-2;
#-------------------------
#Intermediate calculation
N = a/sqrt(1-es^2*sin(lat)^2); #Prime vertical radius of curvature
#Results
xTgt = (N+alt)*cos(lat)*cos(lon); #Target x ECEF coordinate (meter)
yTgt = (N+alt)*cos(lat)*sin(lon); #Target y ECEF coordinate (meter)
zTgt = ((1-es^2)*N + alt)*sin(lat); #Target z ECEF coordinate (meter)
xLocalRef = (N+sealevel)*cos(lat)*cos(lon); #ENU Local ref pt x ECEF coordinate (meter)
yLocalRef = (N+sealevel)*cos(lat)*sin(lon); #ENU Local ref pt y ECEF coordinate (meter)
zLocalRef = ((1-es^2)*N + sealevel)*sin(lat); #ENU Local ref pt z ECEF coordinate (meter)
#End of conversion
#**********************************************************************
#Convert Almanac Data to Satellite Position in ECEF Coordinates and check the Line of Sight to Target
#**********************************************************************
#Constants
io = 0.3*pi; #Inclination angle @ ref. time (rad)
mju = 3.986005e14; #WGS 84 value of the Earth's universal gravitational parameter for GPS user (meters^3/sec^2)
OMEGAdote = 7.2921151467e-5; #WGS 84 value of the Earth's rotation rate (rad/sec)

#--------------------------
gps_timestamp = ymdhms2gps(y, m, d, h, mi, sec);#, timezone, summertime);
gps_week = gps_timestamp$gps_week;
sec_of_week = gps_timestamp$sec_of_week;
#julian_date = 367*y - floor(7/4*(y + floor((m + 9)/12))) - floor(3/4*floor(y + ((m-9)/7)/100)+1)+floor(275*m/9)+d+1721028.5+h/24+mi/1440+sec/86400 - (timezone + summertime)/24 + leapsec/86400;
#gps_week = floor(
#-------------------------
#Almanac Data from Satellite
if (!is.null(almanac_file)){
  if(!file.exists(almanac_file)) stop(paste('almanac file not found at ',  almanac_file))
  fid = file(almanac_file, 'r'); #Open source file "current.al3"
} else {
  fid = url(get_almanac_url(y, m, d) )
}

#Common Data
NumSV_name = scan(fid, nlines = 1, what = character(), quiet = TRUE);
NumSV = as.numeric(NumSV_name[1]); #Number of Satellites
name = NumSV_name[2];
data  = scan(fid, quiet = TRUE);
count = length(data);
close(fid);
wn = data[1]; #GPS week no.
toa = data[2]; #Time of Applicability of Almanac(sec) (range: 0 to 604,784)



Total_weeks = gps_week; #Total number of weeks since 6 Jan 1980
gps_week = gps_week %% 1024;
tk = (gps_week - wn)*604800 + (sec_of_week - toa); #Time since toa(sec) (range: -302,400 to 302,400)
if (gps_week < wn) stop('Almanac file used is incorrect. Please use almanac file for week',gps_week)
if (gps_week > wn) stop('Almanac file is outdated. Please use almanac file for week',gps_week)

#---------------------------
#Satellite Specific Data
num = 0;
SVcount = 1;
#pre-allocate result data structures
xk <- yk <- zk <- numeric(NumSV);
xTgttoSV <- yTgttoSV <- zTgttoSV <- numeric(NumSV);
mag_TgttoSV <- numeric(NumSV);
AngleFromTgt <- numeric(NumSV);
LOS <- numeric(NumSV);

while (SVcount <= NumSV){
  #print(SVcount);
  PRN = data[num+3]; #PRN number
  SVN = data[num+4]; #Satllite number
  URA = data[num+5]; # Average URA number
  ec = data[num+6]; #Eccentricity (dimensionless) (range: 0-0.03)
  del_ik = data[num+7]*pi; #Inclination correction (rad)
  OMEGAdot = data[num+8]*pi; #Rate of right ascension (rad/sec)
  sqrtA = data[num+9]; #Sqr root semi-major axis (m^1/2)
  OMEGAo = data[num+10]*pi; #Right ascension @ ref. time (rad)
  omega = data[num+11]*pi; #Argument of perigee (rad)
  Mo = data[num+12]*pi; #Mean Anomaly @ ref. time (rad)
  Af0 = data[num+13]; #Clock offset (sec)
  Af1 = data[num+14]; #Clock drift (sec/sec)
  Health = data[num+15]; #Satellite Health; 0=healthy
  num = num+14;
  #End of data extraction
  #-------------------------
  #Calculations
  A = sqrtA^2; #Orbit semi-major axis (meter)
  n = sqrt(mju/(A^3)); #Computed mean motion (rad/sec)
  Mk = Mo+tk*n; #Mean anomaly (rad)
  #Start values for iterative solution of Kepler eq.
  Ek = Mk;
  Eold=0;
  while (abs(Ek-Eold)>=1.0e-10) {
    Eold = Ek;
    Ek = Mk+ec*sin(Ek);
  }
  #End of iteration
  vk = atan2((sqrt(1-ec^2)*sin(Ek))/(1-ec*cos(Ek)),(cos(Ek)-ec)/(1-ec*cos(Ek))); #True anomaly (rad)
  Ek = acos((ec+cos(vk))/(1+ec*cos(vk)));
  uk = omega+vk; #Argument of latitude (rad)
  rk = A*(1-ec*cos(Ek)); #Corrected radius (meter)
  ik = io+del_ik; #Corrected inclination (rad)
  xk1 = rk*cos(uk); #x position in orbital plane (meter)
  yk1 = rk*sin(uk); #y position in orbital plane (meter)
  OMEGAk = OMEGAo+(OMEGAdot-OMEGAdote)*tk-OMEGAdote*(toa);
  #Corrected longitude of ascending node (rad)
  #-------------------------
  #Calculations for ECEF coordinates
  xk[SVcount] = xk1*cos(OMEGAk)-yk1*cos(ik)*sin(OMEGAk);
  #Satellite x ECEF coordinate (meter)
  yk[SVcount] = xk1*sin(OMEGAk)+yk1*cos(ik)*cos(OMEGAk);
  #Satellite y ECEF coordinate (meter)
  zk[SVcount] = yk1*sin(ik); #Satellite z ECEF coordinate (meter)
  #End of ECEF coordinates conversion from Almanac Data
  #-------------------------
  #Covert ECEF coordinates to East-North-Up Coordinates
  East = -sin(lon)*(xk-xLocalRef) + cos(lon)*(yk-yLocalRef);
  North = -sin(lat)*cos(lon)*(xk-xLocalRef) - sin(lat)*sin(lon)*(yk-yLocalRef) + cos(lat)*(zk-zLocalRef);
  Up = cos(lat)*cos(lon)*(xk-xLocalRef) + cos(lat)*sin(lon)*(yk-yLocalRef) + sin(lat)*(zk-zLocalRef);
  #-------------------------
  #Algorithm to determine Line of Sight between Target and Satellite
  mag_Tgt = sqrt(xTgt^2+yTgt^2+zTgt^2); #Distance of Target from Earth center (meter)
  xTgttoSV[SVcount] = xk[SVcount] - xTgt;
  yTgttoSV[SVcount] = yk[SVcount] - yTgt;
  zTgttoSV[SVcount] = zk[SVcount] - zTgt;
  mag_TgttoSV[SVcount] = sqrt((xTgttoSV[SVcount])^2+(yTgttoSV[SVcount])^2+(zTgttoSV[SVcount])^2); #Distance from Target to Satellite
  AngleFromTgt[SVcount] = acos(((xTgttoSV[SVcount]*xTgt) + (yTgttoSV[SVcount]*yTgt) + (zTgttoSV[SVcount]*zTgt))/(mag_TgttoSV[SVcount]*mag_Tgt));
  #if mag_SVproj[SVcount]>mag_Tgt && AngleTOS[SVcount]<(pi/2) && AngleFromTgt[SVcount]<(pi/2-(Obstruction*pi/180)) && Health ==0
  if (AngleFromTgt[SVcount]<(pi/2-(Obstruction*pi/180)) && Health ==0){
      Los = 1; #There is Line of Sight
     } else {
       Los = 0; #There is NO Line of Sight
     }
  LOS[SVcount] = Los;
  SVcount = SVcount+1;
}
#**********************************************************************
#Assign coordinates to Target and valid Satellites (i.e. those with LOS with Target)
#**********************************************************************
num3 = 1;
num4 = 1;
SV <- matrix(NA, nrow = sum(LOS), ncol = 3)
while (num3 <= NumSV) {
  if (LOS[num3] == 1){
    SV[num4,1:3] = c(East[num3], North[num3], Up[num3]); #Assigning coordinates to respective valid satellites
    num4 = num4+1;
  }
  num3 = num3+1;
}
NumValidSV = num4-1; #should equal sum LOS
stopifnot(sum(LOS) == NumValidSV);
#**********************************************************************
#Calculate PDOP with the valid satellites i.e. those within the LOS of Tgt
#**********************************************************************
#Pseudo-Range and Directional Derivative Loop
r <- numeric(NumValidSV);
Dx <- Dy <- Dz <- Dt <- numeric(NumValidSV);
for (num4 in 1:NumValidSV){
  #Calculate pseudo-ranges from reciever position to other vehicles
  r[num4] = sqrt((SV[num4,1])^2 + (SV[num4,2])^2 + (SV[num4,3]-alt)^2);
  #Calculate directional derivatives for X,Y,Z, and Time
  Dx[num4] = (SV[num4,1]-0)/r[num4]; #x-coordinates of Tgt in ENU frame is zero
  Dy[num4] = (SV[num4,2]-0)/r[num4]; #y-coordinates of Tgt in ENU frame is zero
  Dz[num4] = (SV[num4,3]-alt)/r[num4]; #z-coordinates of Tgt in ENU frame is the altitude
  Dt[num4] = -1;
}
#Produce the Covariance Matrix from the Directional Derivatives
Alp = matrix(0, nrow = NumValidSV,ncol = 4);
for (num5 in 1:NumValidSV){
  Alp[num5,1] = Dx[num5];
  Alp[num5,2] = Dy[num5];
  Alp[num5,3] = Dz[num5];
  Alp[num5,4] = Dt[num5];
}

Brv = t(Alp);
Chl = Brv %*% Alp;
Dlt = solve(Chl);
# Calculate DOPs from the diagonal elements of the Covariance Matrix
GDOP = sqrt(Dlt[1,1] + Dlt[2,2] + Dlt[3,3] + Dlt[4,4]);
PDOP = sqrt(Dlt[1,1] + Dlt[2,2] + Dlt[3,3]);
HDOP = sqrt(Dlt[1,1] + Dlt[2,2]);
TDOP = sqrt(Dlt[4,4]);
VDOP = sqrt(Dlt[3,3]);
YDOP = sqrt(Dlt[2,2]);
XDOP = sqrt(Dlt[1,1]);

Results = list(NumValidSV = NumValidSV, GDOP = GDOP, PDOP = PDOP, HDOP = HDOP, TDOP = TDOP, VDOP = VDOP, YDOP = YDOP, XDOP = XDOP)
#End of Code :)
return(Results)
}

