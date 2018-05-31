#' confALK
#' @description returns Configurations needed for the ALK
#' @param species The species of interest.
#' @param quarter The quarter of interest.
#' @return A dataframe with configurations needed for constructing the ALK, i.e. maximum age, minimum length and maximium length.
#' @export
#' @examples
confALK = function(species,quarter)
{
  if(species == "Gadus morhua")
  {
    maxAge = 6
    minLength = 7
    maxLength = 110
    lengthClassIntervallLengths = 1
    if(quarter == 1)
    {
      minLength = 15
      maxLength = 90
    }
  }else if(species == "Pollachius virens"){
    maxAge = 6
    minLength = 7
    maxLength = 110
    lengthClassIntervallLengths = 1
    if(quarter == 1)
    {
      minLength = 25
      maxLength = 90
    }
  }else{
    #See Annex 1 in datras procedure document for configurations regarding ALK for different species
  }

  conf = list()
  conf$maxAge = maxAge
  conf$minLength = minLength
  conf$maxLength = maxLength
  conf$lengthClassIntervallLengths = lengthClassIntervallLengths
  return(conf)
}
