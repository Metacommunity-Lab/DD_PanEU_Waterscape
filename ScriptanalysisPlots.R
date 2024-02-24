
##### ##### ##### #####  Loading data ##### ##### ##### ##### 
load("Paisajes_Ensamble_Europa.RData")

# Complete matrix with all the values for each freshwater type: EF(1).T(2).P(3) and Land (0)
M.final.UK # Values obtained for the Ecoregion corresponding to the UK

# Different countries/ecoregions can be obtained
unique(M.final.UK$ISO3_CODE);dim(M.final.UK)
unique(M.final.SWE$ISO3_CODE); dim(M.final.SWE)
unique(M.final.FIN$ISO3_CODE)
unique(M.final_FRA.LUX.BEL.NLD$ISO3_CODE)
unique(M.final.NOR$ISO3_CODE)
unique(M.final.FW.UK$ISO3_CODE)
unique(M.final.FW.SWE$ISO3_CODE)
unique(M.final.FW.ESP.PRT$ISO3_CODE)
unique(M.final.FW.FIN$ISO3_CODE)
unique(M.final.FW_FRA.LUX.BEL.NLD$ISO3_CODE)
unique(M.final.FW.NOR$ISO3_CODE)

#______________________________________________________________________________________________________
# PLOT IN DEGREE ______________________________________________________________________________________ ####
# "SINK-MASS EFECT (in-degree; 4Km)"
par(mar=c(2.5,2.5,1.5,1.5),bty="l",cex=1.6)
par(mfrow=c(2,2),oma = c(0, 0, 4, 0))

#  Ephenmeral freshwaters: EF(1).T(2).P(3) == 1 ____ ####
plot(M.Europa.J$CENTROID_Y~M.Europa.J$CENTROID_X,cex=0.1,col="light gray",pch=15, main="Ephemeral (4Km)")
points(M.Europa.J$CENTROID_Y[which(M.Europa.J$ISO3_CODE=="")]~M.Europa.J$CENTROID_X[which(M.Europa.J$ISO3_CODE=="")],cex=0.1,col="dark gray",pch=15)
mtext("Mass effect \n (in-degree)", outer = TRUE, cex = 1.5,padj = 0,col="black")
## UK
M.final.UK->A
ii <- cut(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)
###SWE
M.final.SWE->A
ii <- cut(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)
###ESP.PRT
M.final.ESP.PRT->A
ii <- cut(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)

M.final.FIN->A
ii <- cut(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)

M.final_FRA.LUX.BEL.NLD->A
ii <- cut(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)

M.final.NOR->A
ii <- cut(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$grado.in.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)

#  Temporal freshwaters: EF(1).T(2).P(3) == 2 ____ ####
plot(M.Europa.J$CENTROID_Y~M.Europa.J$CENTROID_X,cex=0.1,col="light gray",pch=15, main="Temporal (4Km)")
points(M.Europa.J$CENTROID_Y[which(M.Europa.J$ISO3_CODE=="")]~M.Europa.J$CENTROID_X[which(M.Europa.J$ISO3_CODE=="")],cex=0.1,col="black",pch=15)
## UK
M.final.UK->AA
ii <- cut(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)
## SWE
M.final.SWE->AA
ii <- cut(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)
## ESP.PRT
M.final.ESP.PRT->AA
ii <- cut(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)

## FIN
M.final.FIN->AA
ii <- cut(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)

## FRA.LUX.BEL.NLD
M.final_FRA.LUX.BEL.NLD->AA
ii <- cut(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)

## NOR
M.final.NOR->AA
ii <- cut(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$grado.in.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)

#  Permanent freshwaters: EF(1).T(2).P(3) == 3 ____ ####
plot(M.Europa.J$CENTROID_Y~M.Europa.J$CENTROID_X,cex=0.1,col="light gray",pch=15, main="Permanent (4Km)")
points(M.Europa.J$CENTROID_Y[which(M.Europa.J$ISO3_CODE=="")]~M.Europa.J$CENTROID_X[which(M.Europa.J$ISO3_CODE=="")],cex=0.1,col="black",pch=15)
## UK
M.final.UK->AAA
ii <- cut(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

## SWE
M.final.SWE->AAA
ii <- cut(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

## ESP.PRT
M.final.ESP.PRT->AAA
ii <- cut(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

## FIN
M.final.FIN->AAA
ii <- cut(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

## FRA.LUX.BEL.NLD
M.final_FRA.LUX.BEL.NLD->AAA
ii <- cut(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

## NOR
M.final.NOR->AAA
ii <- cut(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$grado.in.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

#  All freshwaters: EF(1).T(2).P(3) == 0 ____ ####
plot(M.Europa.J$CENTROID_Y~M.Europa.J$CENTROID_X,cex=0.1,col="light gray",pch=15, main="All-freshwater (4Km)")
points(M.Europa.J$CENTROID_Y[which(M.Europa.J$ISO3_CODE=="")]~M.Europa.J$CENTROID_X[which(M.Europa.J$ISO3_CODE=="")],cex=0.1,col="black",pch=15)
## UK
M.final.FW.UK->B
ii <- cut(B$grado.in.D50.4, 
          breaks = seq(min(B$grado.in.D50.4), 
                       max(B$grado.in.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
## SWE
M.final.FW.SWE->B
ii <- cut(B$grado.in.D50.4, 
          breaks = seq(min(B$grado.in.D50.4), 
                       max(B$grado.in.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
## ESP.PRT
M.final.ESP.PRT->B
ii <- cut(B$grado.in.D50.4, 
          breaks = seq(min(B$grado.in.D50.4), 
                       max(B$grado.in.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
## NOR
M.final.FW.NOR->B
ii <- cut(B$grado.in.D50.4, 
          breaks = seq(min(B$grado.in.D50.4), 
                       max(B$grado.in.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
## FRA.LUX.BEL.NLD
M.final_FRA.LUX.BEL.NLD->B
ii <- cut(B$grado.in.D50.4, 
          breaks = seq(min(B$grado.in.D50.4), 
                       max(B$grado.in.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
## FIN
M.final.FW.FIN->B
ii <- cut(B$grado.in.D50.4, 
          breaks = seq(min(B$grado.in.D50.4), 
                       max(B$grado.in.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)


#______________________________________________________________________________________________________
# PLOT OUT DEGREE ______________________________________________________________________________________ ####
# "SOURCE EFECT (in-degree; 4Km)"
par(mar=c(2.5,2.5,1.5,1.5),bty="l",cex=1.6)
par(mfrow=c(2,2),oma = c(0, 0, 4, 0))

#  Ephemeral freshwaters: EF(1).T(2).P(3) == 1 ____ ####
plot(M.Europa.J$CENTROID_Y~M.Europa.J$CENTROID_X,cex=0.1,col="light gray",pch=15, main="Ephemeral (4Km)")
points(M.Europa.J$CENTROID_Y[which(M.Europa.J$ISO3_CODE=="")]~M.Europa.J$CENTROID_X[which(M.Europa.J$ISO3_CODE=="")],cex=0.1,col="dark gray",pch=15)
mtext("Source patches \n (out-degree)", outer = TRUE, cex = 1.5,padj = 0,col="black")

## UK
M.final.UK->A
ii <- cut(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)
###SWE
M.final.SWE->A
ii <- cut(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)
###ESP.PRT
M.final.ESP.PRT->A
ii <- cut(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)
###FIN
M.final.FIN->A
ii <- cut(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)

###FRA.LUX.BEL.NLD
M.final_FRA.LUX.BEL.NLD->A
ii <- cut(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)

###NOR
M.final.NOR->A
ii <- cut(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$grado.out.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)

#  Temporal freshwaters: EF(1).T(2).P(3) == 2 ____ ####
plot(M.Europa.J$CENTROID_Y~M.Europa.J$CENTROID_X,cex=0.1,col="light gray",pch=15, main="Temporal (4Km)")
points(M.Europa.J$CENTROID_Y[which(M.Europa.J$ISO3_CODE=="")]~M.Europa.J$CENTROID_X[which(M.Europa.J$ISO3_CODE=="")],cex=0.1,col="black",pch=15)
## UK
M.final.UK->AA
ii <- cut(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)
## SWE
M.final.SWE->AA
ii <- cut(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)
## ESP.PRT
M.final.ESP.PRT->AA
ii <- cut(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)
## FIN
M.final.FIN->AA
ii <- cut(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)
## FRA.LUX.BEL.NLD
M.final_FRA.LUX.BEL.NLD->AA
ii <- cut(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)
## NOR
M.final.NOR->AA
ii <- cut(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$grado.out.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)

#  Permanent freshwaters: EF(1).T(2).P(3) == 3 ____ ####
plot(M.Europa.J$CENTROID_Y~M.Europa.J$CENTROID_X,cex=0.1,col="light gray",pch=15, main="Permanent (4Km)")
points(M.Europa.J$CENTROID_Y[which(M.Europa.J$ISO3_CODE=="")]~M.Europa.J$CENTROID_X[which(M.Europa.J$ISO3_CODE=="")],cex=0.1,col="black",pch=15)
## UK
M.final.UK->AAA
ii <- cut(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

## SWE
M.final.SWE->AAA
ii <- cut(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

## ESP.PRT
M.final.ESP.PRT->AAA
ii <- cut(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

## FIN
M.final.FIN->AAA
ii <- cut(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)
## FRA.LUX.BEL.NLD
M.final_FRA.LUX.BEL.NLD->AAA
ii <- cut(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)
## NOR
M.final.NOR->AAA
ii <- cut(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$grado.out.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

#  All freshwaters: EF(1).T(2).P(3) == 0 ____ ####
plot(M.Europa.J$CENTROID_Y~M.Europa.J$CENTROID_X,cex=0.1,col="light gray",pch=15, main="All-freshwater (4Km)")
points(M.Europa.J$CENTROID_Y[which(M.Europa.J$ISO3_CODE=="")]~M.Europa.J$CENTROID_X[which(M.Europa.J$ISO3_CODE=="")],cex=0.1,col="black",pch=15)
## UK
M.final.FW.UK->B
ii <- cut(B$grado.in.D50.4, 
          breaks = seq(min(B$grado.out.D50.4), 
                       max(B$grado.out.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
## SWE
M.final.FW.SWE->B
ii <- cut(B$grado.in.D50.4, 
          breaks = seq(min(B$grado.out.D50.4), 
                       max(B$grado.out.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
## ESP.PRT
M.final.ESP.PRT->B
ii <- cut(B$grado.in.D50.4, 
          breaks = seq(min(B$grado.out.D50.4), 
                       max(B$grado.out.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)

## FIN
M.final.FIN->B
ii <- cut(B$grado.in.D50.4, 
          breaks = seq(min(B$grado.out.D50.4), 
                       max(B$grado.out.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
## FRA.LUX.BEL.NLD
M.final_FRA.LUX.BEL.NLD->B
ii <- cut(B$grado.in.D50.4, 
          breaks = seq(min(B$grado.out.D50.4), 
                       max(B$grado.out.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
## NOR
M.final.NOR->B
ii <- cut(B$grado.in.D50.4, 
          breaks = seq(min(B$grado.out.D50.4), 
                       max(B$grado.out.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)


#______________________________________________________________________________________________________
# PLOT BETWEENNESS ______________________________________________________________________________________ ####
# "STEPPING STONE (BC-4Km)"
par(mar=c(2.5,2.5,1.5,1.5),bty="l",cex=1.6)
par(mfrow=c(2,2),oma = c(0, 0, 4, 0))

#  Ephemeral freshwaters: EF(1).T(2).P(3) == 1 ____ ####
plot(M.Europa.J$CENTROID_Y~M.Europa.J$CENTROID_X,cex=0.1,col="light gray",pch=15, main="Ephemeral (4Km)")
points(M.Europa.J$CENTROID_Y[which(M.Europa.J$ISO3_CODE=="")]~M.Europa.J$CENTROID_X[which(M.Europa.J$ISO3_CODE=="")],cex=0.1,col="dark gray",pch=15)
mtext("Stepping stone patches \n (betweenness)", outer = TRUE, cex = 1.5,padj = 0,col="black")

## UK
M.final.UK->A
ii <- cut(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)
###SWE
M.final.SWE->A
ii <- cut(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)
###ESP.PRT
M.final.ESP.PRT->A
ii <- cut(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)

###NOR
M.final.NOR->A
ii <- cut(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)

###FRA.LUX.BEL.NLD
M.final_FRA.LUX.BEL.NLD->A
ii <- cut(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)

###FIN
M.final.FIN->A
ii <- cut(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)], 
          breaks = seq(min(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), 
                       max(A$bc.D50.4[which(A$`EF(1).T(2).P(3)`==1)]), len = 600), include.lowest = TRUE)
points(A$CENTROID_Y[which(A$`EF(1).T(2).P(3)`==1)]~A$CENTROID_X[which(A$`EF(1).T(2).P(3)`==1)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,A)

#  Temporal freshwaters: EF(1).T(2).P(3) == 2 ____ ####
plot(M.Europa.J$CENTROID_Y~M.Europa.J$CENTROID_X,cex=0.1,col="light gray",pch=15, main="Temporal (4Km)")
points(M.Europa.J$CENTROID_Y[which(M.Europa.J$ISO3_CODE=="")]~M.Europa.J$CENTROID_X[which(M.Europa.J$ISO3_CODE=="")],cex=0.1,col="black",pch=15)
## UK
M.final.UK->AA
ii <- cut(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)
## SWE
M.final.SWE->AA
ii <- cut(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)
## ESP.PRT
M.final.ESP.PRT->AA
ii <- cut(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)
## NOR
M.final.NOR->AA
ii <- cut(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)
## M.final_FRA.LUX.BEL.NLD
M.final_FRA.LUX.BEL.NLD->AA
ii <- cut(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)
## FIN
M.final.FIN->AA
ii <- cut(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)], 
          breaks = seq(min(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), 
                       max(AA$bc.D50.4[which(AA$`EF(1).T(2).P(3)`==2)]), len = 600), include.lowest = TRUE)
points(AA$CENTROID_Y[which(AA$`EF(1).T(2).P(3)`==2)]~AA$CENTROID_X[which(AA$`EF(1).T(2).P(3)`==2)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AA)

#  Permanent freshwaters: EF(1).T(2).P(3) == 3 ____ ####
plot(M.Europa.J$CENTROID_Y~M.Europa.J$CENTROID_X,cex=0.1,col="light gray",pch=15, main="Permanent (4Km)")
points(M.Europa.J$CENTROID_Y[which(M.Europa.J$ISO3_CODE=="")]~M.Europa.J$CENTROID_X[which(M.Europa.J$ISO3_CODE=="")],cex=0.1,col="black",pch=15)
## UK
M.final.UK->AAA
ii <- cut(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

## SWE
M.final.SWE->AAA
ii <- cut(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

## ESP.PRT
M.final.ESP.PRT->AAA
ii <- cut(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

## FIN
M.final.FIN->AAA
ii <- cut(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

## NOR
M.final.NOR->AAA
ii <- cut(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)
## FRA.LUX.BEL.NLD
M.final_FRA.LUX.BEL.NLD->AAA
ii <- cut(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)], 
          breaks = seq(min(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), 
                       max(AAA$bc.D50.4[which(AAA$`EF(1).T(2).P(3)`==3)]), len = 600), include.lowest = TRUE)
points(AAA$CENTROID_Y[which(AAA$`EF(1).T(2).P(3)`==3)]~AAA$CENTROID_X[which(AAA$`EF(1).T(2).P(3)`==3)],cex=0.4,
       col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,AAA)

#  All freshwaters: EF(1).T(2).P(3) == 0 ____ ####
plot(M.Europa.J$CENTROID_Y~M.Europa.J$CENTROID_X,cex=0.1,col="light gray",pch=15, main="All-freshwater (4Km)")
points(M.Europa.J$CENTROID_Y[which(M.Europa.J$ISO3_CODE=="")]~M.Europa.J$CENTROID_X[which(M.Europa.J$ISO3_CODE=="")],cex=0.1,col="black",pch=15)
## UK
M.final.FW.UK->B
ii <- cut(B$bc.D50.4, 
          breaks = seq(min(B$bc.D50.4), 
                       max(B$bc.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
## SWE
M.final.FW.SWE->B
ii <- cut(B$bc.D50.4, 
          breaks = seq(min(B$bc.D50.4), 
                       max(B$bc.D50.4), len = 600), include.lowest = TRUE)
points(B$CENTROID_Y~B$CENTROID_X,cex=0.4,col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
## ESP.PRT
M.final.ESP.PRT->B
ii <- cut(B$bc.D50.4, 
          breaks = seq(min(B$bc.D50.4), 
                       max(B$bc.D50.4), len = 600), include.lowest = TRUE)

points(B$CENTROID_Y~B$CENTROID_X,cex=0.4, col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
## NOR
M.final.NOR->B
ii <- cut(B$bc.D50.4, 
          breaks = seq(min(B$bc.D50.4), 
                       max(B$bc.D50.4), len = 600), include.lowest = TRUE)

points(B$CENTROID_Y~B$CENTROID_X,cex=0.4, col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
##FRA.LUX.BEL.NLD 
M.final_FRA.LUX.BEL.NLD->B
ii <- cut(B$bc.D50.4, 
          breaks = seq(min(B$bc.D50.4), 
                       max(B$bc.D50.4), len = 600), include.lowest = TRUE)

points(B$CENTROID_Y~B$CENTROID_X,cex=0.4, col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
##FIN
M.final.FIN->B
ii <- cut(B$bc.D50.4, 
          breaks = seq(min(B$bc.D50.4), 
                       max(B$bc.D50.4), len = 600), include.lowest = TRUE)

points(B$CENTROID_Y~B$CENTROID_X,cex=0.4, col = colorRampPalette(c("yellow", "orange", "red"))(599)[ii],pch=15)
rm(ii,B)
