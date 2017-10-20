#This R-file conisist of functions calculating the CPUEs of interest.

#' calcCPUEgfa
#' @description Calculates CPUE per length class in a given redfish area.
#' @param RFA Roundfish area number.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param data the datras-data needed.
#' @param ALK the ALK.
#' @export
#' @return Returns the CPUE per length class in the given roundfish area, TODO, include the ALK to return per age class given ALK instead?
#' @examples
calcCPUEgfa = function(RFA,species,year, quarter, data, ALK = NULL)
{
  #TODO
}




#' calcCPUEstatRec
#' @description Calculates CPUE per length class in a given statistical rectangle.
#' @param statRec Statistical area.
#' @param species The species of interest.
#' @param year The year of interest.
#' @param quarter The quarter of interest.
#' @param data the datras-data needed.
#' @param percentOfAreaRepresentative the percentage of the statical recangle within sea depth intervall
#' @export
#' @return Returns the CPUE per length class in the given statistical rectangle TODO, include the ALK to return per age class given ALK instead?
#' @examples
calcCPUEstatRec = function(statRec,species,year, quarter, data, ALK = NULL,percentOfAreaRepresentative = NULL)
{
  #TODO
}
