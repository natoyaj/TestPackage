library(xtable)

#Construct table for weights------------------------
n = dim(tab)[1]
nn = ceiling(n/5)
xtable(tab)

tab = dat$weightStatRec
tmp = tab[1:nn,]
tmp$StatRec2 = tab[(nn+1):(2*nn),1]
tmp$Weight2 = tab[(nn+1):(2*nn),2]
tmp$StatRec3 = tab[(2*nn+1):(3*nn),1]
tmp$Weight3 = tab[(2*nn+1):(3*nn),2]
tmp$StatRec4 = tab[(3*nn+1):(4*nn),1]
tmp$Weight4 = tab[(3*nn+1):(4*nn),2]
tmp$StatRec5 = tab[(4*nn+1):(5*nn),1]
tmp$Weight5 = tab[(4*nn+1):(5*nn),2]

xtable(tmp,display = rep("s",11))
#-----------------------------------------------------


#Construct table for results--------------------------
tab  = mCPUEHaulBasedStratifiedHLandCA
xtable(tab,digits = 2)
#-----------------------------------------------------



