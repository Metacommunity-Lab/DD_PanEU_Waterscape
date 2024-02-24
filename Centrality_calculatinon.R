###### ###### ###### ###### ###### ###### CENTRALIDADES ###### ###### ###### ###### ###### ###### ###### ###### 
setwd("~/Documents/Analisis_Datos/A_H2020_Networks/EUROPA_EN_10X10/PanEuropean_Ecoregions/Paisajes_2022")
load("~/Documents/Analisis_Datos/A_H2020_Networks/EUROPA_EN_10X10/PanEuropean_Ecoregions/Paisajes_2022/Paisajes_Ensamble_Europa.RData")


###### MATRICES #####
M.final.ID_0->R; M.final.FW.ID_0->R2
M.final.ID_402->R; M.final.FW.ID_402->R2
M.final.ID_403->R; M.final.FW.ID_403->R2
M.final.ID_404->R; M.final.FW.ID_404->R2
M.final.ID_405->R; M.final.FW.ID_405->R2
M.final.ID_406->R; M.final.FW.ID_406->R2
M.final.ID_407->R; M.final.FW.ID_407->R2
M.final.ID_408->R; M.final.FW.ID_408->R2
M.final.ID_409->R; M.final.FW.ID_409->R2
M.final.ID_410->R; M.final.FW.ID_410->R2
M.final.ID_412->R; M.final.FW.ID_412->R2
M.final.ID_413->R; M.final.FW.ID_413->R2
M.final.ID_414->R; M.final.FW.ID_414->R2
M.final.ID_415->R; M.final.FW.ID_415->R2
M.final.ID_416->R; M.final.FW.ID_416->R2
M.final.ID_417->R; M.final.FW.ID_417->R2
M.final.ID_418->R; M.final.FW.ID_418->R2
M.final.ID_419->R; M.final.FW.ID_419->R2
M.final.ID_420->R; M.final.FW.ID_420->R2
M.final.ID_421->R; M.final.FW.ID_421->R2
M.final.ID_422->R; M.final.FW.ID_422->R2
M.final.ID_423->R; M.final.FW.ID_423->R2
M.final.ID_424->R; M.final.FW.ID_424->R2
M.final.ID_425->R; M.final.FW.ID_425->R2
M.final.ID_426->R; M.final.FW.ID_426->R2
M.final.ID_427->R; M.final.FW.ID_427->R2
M.final.ID_428->R; M.final.FW.ID_428->R2
M.final.ID_429->R; M.final.FW.ID_429->R2
M.final.ID_430->R; M.final.FW.ID_430->R2
M.final.ID_431->R; M.final.FW.ID_431->R2
M.final.ID_432->R; M.final.FW.ID_432->R2
M.final.ID_433->R; M.final.FW.ID_433->R2
M.final.ID_436->R; M.final.FW.ID_436->R2
M.final.ID_437->R; M.final.FW.ID_437->R2
M.final.ID_440->R; M.final.FW.ID_440->R2
M.final.ID_441->R; M.final.FW.ID_441->R2
M.final.ID_442->R; M.final.FW.ID_442->R2
########


## R3<-cuantiles_grado.OUT(R,R2,18); 
## RR.out<-rbind(RR.out,R3)
## R3<-cuantiles_grado.IN(R,R2,17)
## RR.in<-rbind(RR.in,R3)
## R3<-cuantiles_BC(R,R2,19)
## RR.bc<-rbind(RR.bc,R3)
RR.bc



################################################## BETWENNESS ###########################################################################
# cuantiles_BC(R,R2,19)
cuantiles_BC<-function(R,R2,centralidad_en=19){
  out<-NULL
  max.fw<-max(R2$bc.D50.4[R2$`EF(1).T(2).P(3).`==100])
  max.ef<-max(R$bc.D50.4[R$`EF(1).T(2).P(3).`==1])
  max.temp<-max(R$bc.D50.4[R$`EF(1).T(2).P(3).`==2])
  max.perm<-max(R$bc.D50.4[R$`EF(1).T(2).P(3).`==3])
  bc.fw<-R2[which(R2$`EF(1).T(2).P(3).`==100),centralidad_en]; (bc.fw/max.fw)->bc.fw_st
  bc.ef<-R[which(R$`EF(1).T(2).P(3).`==1),centralidad_en]; (bc.ef/max.ef)->bc.ef_st
  bc.temp<-R[which(R$`EF(1).T(2).P(3).`==2),centralidad_en]; (bc.temp/max.temp)->bc.temp_st
  bc.perm<-R[which(R$`EF(1).T(2).P(3).`==3),centralidad_en]; (bc.perm/max.perm)->bc.perm_st
  
  cv.fw<-sd(R2[which(R2$`EF(1).T(2).P(3).`==100),centralidad_en],na.rm = FALSE)/mean(R2[which(R2$`EF(1).T(2).P(3).`==100),centralidad_en],na.rm = FALSE)
  cv.ef<-sd(R[which(R$`EF(1).T(2).P(3).`==1),centralidad_en],na.rm = FALSE)/mean(R[which(R$`EF(1).T(2).P(3).`==1),centralidad_en],na.rm = FALSE)
  cv.temp<-sd(R[which(R$`EF(1).T(2).P(3).`==2),centralidad_en],na.rm = FALSE)/mean(R[which(R$`EF(1).T(2).P(3).`==2),centralidad_en],na.rm = FALSE)
  cv.perm<-sd(R[which(R$`EF(1).T(2).P(3).`==3),centralidad_en],na.rm = FALSE)/mean(R[which(R$`EF(1).T(2).P(3).`==3),centralidad_en],na.rm = FALSE)
  
  ee<-quantile(bc.fw_st,c(0.05,0.5,0.95))
  aa<-quantile(bc.ef_st,c(0.05,0.5,0.95))
  bb<-quantile(bc.temp_st,c(0.05,0.5,0.95))
  dd<-quantile(bc.perm_st,c(0.05,0.5,0.95))
  
  Land_cell<-table(R2$`EF(1).T(2).P(3).`)[[1]] ## solo celdas de land
  FW_cell<-table(R2$`EF(1).T(2).P(3).`)[[2]] ## solo celdas de FW
  Size_region_cell<-(Land_cell+FW_cell) ## tamaño en celdas de la region
  Efim_cell<-table(R$`EF(1).T(2).P(3).`)[[1]]
  Temp_cell<-table(R$`EF(1).T(2).P(3).`)[[2]]
  Perm_cell<-table(R$`EF(1).T(2).P(3).`)[[3]]
  median_porcentaje_FW<-100-median(R2$LAND) ## porecentaje total de FW en la region entre 0 y 100 (no son celdas)
  
  GG<-cbind(mean(R$CENTROID_X),mean(R$CENTROID_Y),as.numeric(unique(R$EcoR_ID)),Land_cell,
            FW_cell,Size_region_cell,cv.fw,ee[[1]],ee[[2]],ee[[3]],
            Efim_cell,cv.ef,aa[[1]],aa[[2]],aa[[3]],
            Temp_cell,cv.temp,bb[[1]],bb[[2]],bb[[3]],
            Perm_cell,cv.perm,dd[[1]],dd[[2]],dd[[3]])
  colnames(GG)<-c("mean_CENTROID_X","mean_CENTROID_Y","EcoR_ID","Land_cell",
                  "FW_cell","Size_region_cell","cv.fw","fw.bc_q1","fw.bc_q2","fw.bc_q3",
                  "Efim_cell","cv.ef","ef.bc_q1","ef.bc_q2","ef.bc_q3",
                  "Temp_cell","cv.temp","temp.bc_q1","temp.bc_q2","temp.bc_q3",
                  "Perm_cell","cv.perm","perm.bc_q1","perm.bc_q2","perm.bc_q3")
  out<-rbind(out,GG)
  out
}



################################################## GRADO IN ###########################################################################
# cuantiles_grado.IN(R,R2,17)
cuantiles_grado.IN<-function(R,R2,centralidad_en=17){
  out<-NULL
  max.fw<-max(R2$grado.in.D50.4[R2$`EF(1).T(2).P(3).`==100])
  max.ef<-max(R$grado.in.D50.4[R$`EF(1).T(2).P(3).`==1])
  max.temp<-max(R$grado.in.D50.4[R$`EF(1).T(2).P(3).`==2])
  max.perm<-max(R$grado.in.D50.4[R$`EF(1).T(2).P(3).`==3])
  in.fw<-R2[which(R2$`EF(1).T(2).P(3).`==100),centralidad_en]; (in.fw/max.fw)->in.fw_st
  in.ef<-R[which(R$`EF(1).T(2).P(3).`==1),centralidad_en]; (in.ef/max.ef)->in.ef_st
  in.temp<-R[which(R$`EF(1).T(2).P(3).`==2),centralidad_en]; (in.temp/max.temp)->in.temp_st
  in.perm<-R[which(R$`EF(1).T(2).P(3).`==3),centralidad_en]; (in.perm/max.perm)->in.perm_st
  
  cv.fw<-sd(R2[which(R2$`EF(1).T(2).P(3).`==100),centralidad_en],na.rm = FALSE)/mean(R2[which(R2$`EF(1).T(2).P(3).`==100),centralidad_en],na.rm = FALSE)
  cv.ef<-sd(R[which(R$`EF(1).T(2).P(3).`==1),centralidad_en],na.rm = FALSE)/mean(R[which(R$`EF(1).T(2).P(3).`==1),centralidad_en],na.rm = FALSE)
  cv.temp<-sd(R[which(R$`EF(1).T(2).P(3).`==2),centralidad_en],na.rm = FALSE)/mean(R[which(R$`EF(1).T(2).P(3).`==2),centralidad_en],na.rm = FALSE)
  cv.perm<-sd(R[which(R$`EF(1).T(2).P(3).`==3),centralidad_en],na.rm = FALSE)/mean(R[which(R$`EF(1).T(2).P(3).`==3),centralidad_en],na.rm = FALSE)
  
  ee<-quantile(in.fw_st,c(0.05,0.5,0.95))
  aa<-quantile(in.ef_st,c(0.05,0.5,0.95))
  bb<-quantile(in.temp_st,c(0.05,0.5,0.95))
  dd<-quantile(in.perm_st,c(0.05,0.5,0.95))
  
  Land_cell<-table(R2$`EF(1).T(2).P(3).`)[[1]] ## solo celdas de land
  FW_cell<-table(R2$`EF(1).T(2).P(3).`)[[2]] ## solo celdas de FW
  Size_region_cell<-(Land_cell+FW_cell) ## tamaño en celdas de la region
  Efim_cell<-table(R$`EF(1).T(2).P(3).`)[[1]]
  Temp_cell<-table(R$`EF(1).T(2).P(3).`)[[2]]
  Perm_cell<-table(R$`EF(1).T(2).P(3).`)[[3]]
  median_porcentaje_FW<-100-median(R2$LAND) ## porecentaje total de FW en la region entre 0 y 100 (sin celdas)
  
  GG<-cbind(mean(R$CENTROID_X),mean(R$CENTROID_Y),as.numeric(unique(R$EcoR_ID)),Land_cell,
            FW_cell,Size_region_cell,cv.fw,ee[[1]],ee[[2]],ee[[3]],
            Efim_cell,cv.ef,aa[[1]],aa[[2]],aa[[3]],
            Temp_cell,cv.temp,bb[[1]],bb[[2]],bb[[3]],
            Perm_cell,cv.perm,dd[[1]],dd[[2]],dd[[3]])
  colnames(GG)<-c("mean_CENTROID_X","mean_CENTROID_Y","EcoR_ID","Land_cell",
                  "FW_cell","Size_region_cell","cv.fw","fw.in_q1","fw.in_q2","fw.in_q3",
                  "Efim_cell","cv.ef","ef.in_q1","ef.in_q2","ef.in_q3",
                  "Temp_cell","cv.temp","temp.in_q1","temp.in_q2","temp.in_q3",
                  "Perm_cell","cv.perm","perm.in_q1","perm.in_q2","perm.in_q3")
  out<-rbind(out,GG)
  out
}


################################################## GRADO OUT ###########################################################################
# cuantiles_grado.OUT(R,R2,18)
cuantiles_grado.OUT<-function(R,R2,centralidad_en=18){
  out<-NULL
  max.fw<-max(R2$grado.out.D50.4[R2$`EF(1).T(2).P(3).`==100])
  max.ef<-max(R$grado.out.D50.4[R$`EF(1).T(2).P(3).`==1])
  max.temp<-max(R$grado.out.D50.4[R$`EF(1).T(2).P(3).`==2])
  max.perm<-max(R$grado.out.D50.4[R$`EF(1).T(2).P(3).`==3])
  out.fw<-R2[which(R2$`EF(1).T(2).P(3).`==100),centralidad_en]; (out.fw/max.fw)->out.fw_st
  out.ef<-R[which(R$`EF(1).T(2).P(3).`==1),centralidad_en]; (out.ef/max.ef)->out.ef_st
  out.temp<-R[which(R$`EF(1).T(2).P(3).`==2),centralidad_en]; (out.temp/max.temp)->out.temp_st
  out.perm<-R[which(R$`EF(1).T(2).P(3).`==3),centralidad_en]; (out.perm/max.perm)->out.perm_st
  
  cv.fw<-sd(R2[which(R2$`EF(1).T(2).P(3).`==100),centralidad_en],na.rm = FALSE)/mean(R2[which(R2$`EF(1).T(2).P(3).`==100),centralidad_en],na.rm = FALSE)
  cv.ef<-sd(R[which(R$`EF(1).T(2).P(3).`==1),centralidad_en],na.rm = FALSE)/mean(R[which(R$`EF(1).T(2).P(3).`==1),centralidad_en],na.rm = FALSE)
  cv.temp<-sd(R[which(R$`EF(1).T(2).P(3).`==2),centralidad_en],na.rm = FALSE)/mean(R[which(R$`EF(1).T(2).P(3).`==2),centralidad_en],na.rm = FALSE)
  cv.perm<-sd(R[which(R$`EF(1).T(2).P(3).`==3),centralidad_en],na.rm = FALSE)/mean(R[which(R$`EF(1).T(2).P(3).`==3),centralidad_en],na.rm = FALSE)
  
  ee<-quantile(out.fw_st,c(0.05,0.5,0.95))
  aa<-quantile(out.ef_st,c(0.05,0.5,0.95))
  bb<-quantile(out.temp_st,c(0.05,0.5,0.95))
  dd<-quantile(out.perm_st,c(0.05,0.5,0.95))
  
  Land_cell<-table(R2$`EF(1).T(2).P(3).`)[[1]] ## solo celdas de land
  FW_cell<-table(R2$`EF(1).T(2).P(3).`)[[2]] ## solo celdas de FW
  Size_region_cell<-(Land_cell+FW_cell) ## tamaño en celdas de la region
  Efim_cell<-table(R$`EF(1).T(2).P(3).`)[[1]]
  Temp_cell<-table(R$`EF(1).T(2).P(3).`)[[2]]
  Perm_cell<-table(R$`EF(1).T(2).P(3).`)[[3]]
  median_porcentaje_FW<-100-median(R2$LAND) ## porecentaje total de FW en la region entre 0 y 100 (sin celdas)
  
   # if(length(EF.T_grado.out)) {MM[1,1]<-EF.T_grado.out} 
  #  if(length(EF.P_grado.out)){MM[1,2]<-EF.P_grado.out} 
   # if(length(T.P_grado.out)) {MM[1,3]<-T.P_grado.out}
  GG<-cbind(mean(R$CENTROID_X),mean(R$CENTROID_Y),as.numeric(unique(R$EcoR_ID)),Land_cell,
            FW_cell,Size_region_cell,cv.fw,ee[[1]],ee[[2]],ee[[3]],
            Efim_cell,cv.ef,aa[[1]],aa[[2]],aa[[3]],
            Temp_cell,cv.temp,bb[[1]],bb[[2]],bb[[3]],
            Perm_cell,cv.perm,dd[[1]],dd[[2]],dd[[3]])
  colnames(GG)<-c("mean_CENTROID_X","mean_CENTROID_Y","EcoR_ID","Land_cell",
                  "FW_cell","Size_region_cell","cv.fw","fw.out_q1","fw.out_q2","fw.out_q3",
                  "Efim_cell","cv.ef","ef.out_q1","ef.out_q2","ef.out_q3",
                  "Temp_cell","cv.temp","temp.out_q1","temp.out_q2","temp.out_q3",
                  "Perm_cell","cv.perm","perm.out_q1","perm.out_q2","perm.out_q3")
    out<-rbind(out,GG)
  out
}
