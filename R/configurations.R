#' confALK
#' @description returns Configurations needed for the ALK
#' @param species The species of interest.
#' @param quarter The quarter of interest.
#' @return A dataframe with configurations needed for constructing the ALK, i.e. maximum age, minimum length and maximium length.
#' @export
#' @examples
confALK = function(species,quarter)
{
  minAge = 0
  if(species == "Gadus morhua")
  {
    maxAge = 6
    minLength = 7
    maxLength = 110
    lengthClassIntervallLengths = 1
    if(quarter == 1)
    {
      minAge = 1
      minLength = 15
      maxLength = 90
    }
  }else if(species == "Pollachius virens"){
    maxAge = 10
    minLength = 7
    maxLength = 110
    lengthClassIntervallLengths = 1
    if(quarter == 1)
    {
      minAge = 1
      minLength = 25
      maxLength = 90
    }
  }else{
    maxAge = 6
    minLength = 7
    maxLength = 110
    lengthClassIntervallLengths = 1
    if(quarter == 1)
    {
      minAge = 1
      minLength = 15
      maxLength = 110
    }
    print("Warning, dont know the max age or min age")
  }

  conf = list()
  conf$minAge = minAge
  conf$maxAge = maxAge
  conf$minLength = minLength
  conf$maxLength = maxLength
  conf$lengthClassIntervallLengths = lengthClassIntervallLengths
  return(conf)
}
