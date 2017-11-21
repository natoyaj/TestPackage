#Calculate confidence intervalls of each elemtent in the ALK-matrix-----------------------------------------
year = 2015
RFA = 9
quarter = 1
B = 20
species = "Gadus morhua"
simulatedALK = simulateALK(RFA = RFA, species = species, year = year, quarter = quarter, dataCA = ca_hh,bootstrapProcedure = "simple", B = B)
simulatedALK = simulateALK(RFA = RFA, species = species, year = year, quarter = quarter, dataCA = ca_hh,bootstrapProcedure = "stratified", B = B)

LQMatrix = simulatedALK[[1]]
LQMatrix[,2:dim(LQMatrix)[2]]=0
UQMatrix = LQMatrix
MQMatrix = LQMatrix
for(i in 1:dim(LQMatrix)[1])
{
  for(j in 2:dim(LQMatrix)[2])
  {
    tmp = rep(0,B)
    for(simIndeks in 1:B)
    {
      tmp[simIndeks] = tmp[simIndeks] + simulatedALK[[simIndeks]][i,j]/sum(simulatedALK[[simIndeks]][i,2:dim(MQMatrix)[2]])
    }
    LQMatrix[i,j] = quantile(tmp,0.1)
    UQMatrix[i,j] = quantile(tmp,0.9)
    MQMatrix[i,j] = mean(tmp)
  }
}
#LQMatrix, UQMatrix and MQMatrix give now an 80% P.I. and mean for each element i the ALK.
#--------------------------------------------------------------


