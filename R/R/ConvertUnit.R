# convert unit
# area are in m2
ConvertUnit <- function(inN = 1, inUnit = "m3/s", outUnit = "mm/day", areaCat = 1)
{
  if ((inUnit == "m3/s") && (outUnit == "mm/day")) outN <- inN * 86400 * 1000 / areaCat
  return (outN)

      
}