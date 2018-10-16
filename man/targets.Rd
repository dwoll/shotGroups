\name{targets}
\alias{targets}
\docType{data}
\title{List containing definitions of several circular target types from the shooting federations ISSF, NRA, DSB, BDS, BDMP, DSU}
\description{
List containing definitions of several circular german (DSB, BDS, BDMP, DSU), ISSF, and NRA target types.
}
\usage{data(targets)}
\format{
  A list with the following components, each defining one target type.
  \describe{
  \item{\code{ISSF_10mAR}}{ISSF 10m Air Rifle.}
  \item{\code{ISSF_10mAP}}{ISSF 10m Air Pistol.}
  \item{\code{ISSF_25mPP}}{ISSF 25m Precision Pistol, 50m Pistol.}
  \item{\code{ISSF_25mRFP}}{ISSF 25m Rapid Fire Pistol.}
  \item{\code{ISSF_50m}}{ISSF 50m Rifle.}
  \item{\code{ISSF_100m}}{ISSF 100m Rifle (same as \code{ISSF_25mPP}).}
  \item{\code{ISSF_300m}}{ISSF 300m Rifle.}

  \item{\code{ISSF_25ydPP}}{ISSF 25m and 50m Precision Pistol target adapted to 25yd.}
  \item{\code{ISSF_50ydPP}}{ISSF 25m and 50m Precision Pistol target adapted to 50yd.}
  \item{\code{ISSF_50ftPP}}{ISSF 25m and 50m Precision Pistol target adapted to 50ft.}
  \item{\code{ISSF_50ftSP}}{ISSF 25m and 50m Precision Pistol target adapted to 50ft Sport Pistol.}
  \item{\code{ISSF_25ydRFP}}{ISSF 25m Rapid Fire Pistol target adapted to 25yd.}
  \item{\code{ISSF_50ftRFP}}{ISSF 25m Rapid Fire Pistol target adapted to 50ft.}
  \item{\code{ISSF_50ft}}{ISSF 50m Rifle target adapted to 50ft.}
  \item{\code{ISSF_50yd}}{ISSF 50m Rifle target adapted to 50yd.}
  \item{\code{ISSF_100yd}}{ISSF 300m Rifle target adapted to 100yd.}
  \item{\code{ISSF_200yd}}{ISSF 300m Rifle target adapted to 200yd.}
  \item{\code{ISSF_300yd}}{ISSF 300m Rifle target adapted to 300yd.}

  \item{\code{NRA_HPR_SR}}{NRA 200yd High Powered Rifle SR: Military Target, Rifle Competition, Short Range.}
  \item{\code{NRA_HPR_SR3}}{NRA 300yd High Powered Rifle SR-3.}
  \item{\code{NRA_P_B16}}{NRA 25yd Pistol Slow Fire B-16.}
  \item{\code{NRA_P_B6}}{NRA 50yd Standard American B-6.}
  \item{\code{NRA_MR-1}}{NRA MR-1.}
  \item{\code{NRA_MR-63}}{NRA MR-63 300yd.}
  \item{\code{NRA_MR-63FC}}{NRA MR-63 F-class.}
  \item{\code{NRA_MR-65}}{NRA MR-65 500yd.}
  \item{\code{NRA_MR-65FC}}{NRA MR-65 F-class.}
  \item{\code{NRA_LRFC}}{NRA LRFC F-class.}

  \item{\code{DSB1}}{DSB 10m Luftgewehr (same as \code{ISSF_10mAR}).}
  \item{\code{DSB2}}{DSB 15m Zimmerstutzen.}
  \item{\code{DSB3}}{DSB 50m Kleinkalibergewehr (same as \code{ISSF_50m}).}
  \item{\code{DSB4}}{DSB 100m Kleinkalibergewehr, 25m Pistole-Praezision, 25m Standardpistole, 50m Pistole (same as \code{ISSF_25mPP}).}
  \item{\code{DSB5}}{DSB 300m Gewehr/Vorderlader Freigewehr (same as \code{ISSF_300m}).}
  \item{\code{DSB6}}{DSB 50m Muskete Luntenschlossgewehr.}
  \item{\code{DSB7}}{DSB 10m Luftpistole (same as \code{ISSF_10mAP}).}
  \item{\code{DSB9}}{DSB 25m Schnellfeuerpistole, 25m Pistole Duell (same as \code{ISSF_25mRFP}).}
  \item{\code{DSB11}}{DSB 10m Laufende Scheibe.}

  \item{\code{BDS1}}{BDS 100m.}
  \item{\code{BDS2}}{BDS 50m Zielfernrohr.}
  \item{\code{BDS3}}{BDS 50m (same as \code{ISSF_50m}).}
  \item{\code{BDS4}}{BDS 100m Zielfernrohr.}
  \item{\code{BDS5}}{BDS Pistole 25m, Pistole 50m (same as \code{ISSF_100m}).}
  \item{\code{BDS7}}{BDS 300m (same as \code{ISSF_300m}).}
  \item{\code{BDS8}}{BDS 300m Zielfernrohr.}
  \item{\code{BDS9}}{BDS 25m Kurzwaffe.}
  \item{\code{BDS13}}{BDS 10m Luftgewehr (same as \code{ISSF_10mAR}).}
  \item{\code{BDS14}}{BDS 10m Luftpistole (same as \code{DSB7}).}

  \item{\code{BDMP1_25m}}{BDMP 25m .30 M1 Carbine.}
  \item{\code{BDMP1_50m}}{BDMP 50m .30 M1 Carbine.}
  \item{\code{BDMP1_100m}}{BDMP 100m SG 1, CISM-Gewehr, .30 M1 Carbine.}
  \item{\code{BDMP2}}{BDMP 300m SG 2, CISM-Gewehr, DG 2, FG 1, PVDG 1, PHDG 1, SDG 1, SDG 2, PFG 1, SPPDG 1 (same as \code{ISSF_300m}).}
  \item{\code{BDMP3}}{BDMP ZG 1.}
  \item{\code{BDMP4}}{BDMP ZG 2, ZG 3, ZG 4.}
  \item{\code{BDMP5}}{BDMP 300m DG 3, DG 4.}

  \item{\code{DSUa2}}{DSU a2.}
  \item{\code{DSUb2}}{DSU b2.}
  \item{\code{DSUb3}}{DSU b3.}
  \item{\code{DSUb4}}{DSU b4.}
  \item{\code{DSUb5}}{DSU b5.}
  \item{\code{DSUb5P}}{DSU b5 Praezision.}
  \item{\code{DSU_200mP}}{DSU 200m Praezision.}
  \item{\code{DSU_UITP}}{DSU UIT Praezision (same as \code{ISSF_100m}).}
  \item{\code{DSU_KKI}}{DSU KK international (same as \code{ISSF_50m}).}}
}

\details{
Each target is defined by (at least) the following parameters
\itemize{
  \item \code{name}: target name
  \item \code{unitTarget}: measurement unit for ring diameters and radii
  \item \code{nRings}: number of rings
  \item \code{maxCount}: highest ring count for scoring
  \item \code{ringD10}: diameter of ring number 10 (highest-valued ring)
  \item \code{ringD10i}: diameter of sub-division of ring number 10 (Innenzehn). If target has no sub-division, equal to \code{ringD10}
  \item \code{ringW}: width of the remaining rings number 9, 8, 7, ...
  \item \code{cols}: \code{nRings}+1 colors of the rings - right half of the target, starting with the sub-division of ring number 10 and going outwards
  \item \code{colsTxt}: \code{nRings}-1 colors of the ring numbers, starting with ring number 9 and going outwards
}
A target may have more parameters, e.g., \code{draw} to indicate the drawing function that should be used, or \code{countMouche} if the inner 10 (mouche) should be counted extra in scoring.
}
\seealso{
\code{\link[shotGroups]{drawTarget}},
\code{\link[shotGroups]{drawGroup}}
}
\examples{
data(targets)
names(targets)

targets$ISSF_25mPP
}
\keyword{datasets}
