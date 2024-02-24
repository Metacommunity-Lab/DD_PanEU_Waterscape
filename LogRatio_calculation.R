## Centralidad log ratio = Centr_LR
M.final.ID_410->R
table(R$`EF(1).T(2).P(3).`)

###### GRADO_IN; misma centralidad es 0; si no hay alguno de los cuerpos de agua es 10000; sino un valor
log_ratio_grado.IN<-function(R,centralidad_en){
out<-NULL
id.page.name<-(unique(R$PageName))
max.ef<-max(R$grado.in.D50.4[R$`EF(1).T(2).P(3).`==1])
max.temp<-max(R$grado.in.D50.4[R$`EF(1).T(2).P(3).`==2])
max.perm<-max(R$grado.in.D50.4[R$`EF(1).T(2).P(3).`==3])
  for(i in 1:length(id.page.name)){
    R2<-R[which(R$PageName==id.page.name[i]),]
    MM<-matrix(10000,1,3)
  #log((R2[which(R2$`EF(1).T(2).P(3).`==1),centralidad_en]+1)/(R2[which(R2$`EF(1).T(2).P(3).`==2),centralidad_en]+1),2)->EF.T_grado.in 
    in.ef<-R2[which(R2$`EF(1).T(2).P(3).`==1),centralidad_en]; (in.ef/max.ef)->in.ef_st
    in.temp<-R2[which(R2$`EF(1).T(2).P(3).`==2),centralidad_en]; (in.temp/max.temp)->in.temp_st
    EF.T_grado.in<-log(((in.ef_st+1)/(in.temp_st+1)),2)
  #log((R2[which(R2$`EF(1).T(2).P(3).`==1),centralidad_en]+1)/(R2[which(R2$`EF(1).T(2).P(3).`==3),centralidad_en]+1),2)->EF.P_grado.in
    in.perm<-R2[which(R2$`EF(1).T(2).P(3).`==3),centralidad_en]; (in.perm/max.perm)->in.perm_st
    EF.P_grado.in<-log((in.ef_st+1)/(in.perm_st+1),2)
  #log((R2[which(R2$`EF(1).T(2).P(3).`==2),centralidad_en]+1)/(R2[which(R2$`EF(1).T(2).P(3).`==3),centralidad_en]+1),2)->T.P_grado.in
    T.P_grado.in<-log((in.temp_st+1)/(in.perm_st+1),2)
    
    if(length(EF.T_grado.in)) {MM[1,1]<-EF.T_grado.in} 
    if(length(EF.P_grado.in)){MM[1,2]<-EF.P_grado.in} 
    if(length(T.P_grado.in)) {MM[1,3]<-T.P_grado.in}
    MM2<-cbind(R2[1,2],R2[1,3],as.numeric(R2[1,9]),MM)
    colnames(MM2)<-c("CENTROID_X","CENTROID_Y","EcoR_ID","EF.T_grado.in","EF.P_grado.in","T.P_grado.in")
    rownames(MM2)<-c(id.page.name[i])
    out<-rbind(out,MM2)
}
out
}
  
#### chequeo
head(R)
(1+R[which(R$PAGENAME=="DL530"),17][1]/max(R$grado.in.D50.4[R$`EF(1).T(2).P(3).`==1]))->a
(1+R[which(R$PAGENAME=="DL530"),17][3]/max(R$grado.in.D50.4[R$`EF(1).T(2).P(3).`==3]))->b
#log_ratio_grado.IN(R,centralidad_en = 17)->R.in_st
log(a/b,2)
head(R.in_st)

#####
###### GRADO_out; misma centralidad es 0; si no hay alguno de los cuerpos de agua es 10000; sino un valor pos o neg
log_ratio_grado.OUT<-function(R,centralidad_en){
  out<-NULL
  id.page.name<-(unique(R$PageName))
  max.ef<-max(R$grado.out.D50.4[R$`EF(1).T(2).P(3).`==1])
  max.temp<-max(R$grado.out.D50.4[R$`EF(1).T(2).P(3).`==2])
  max.perm<-max(R$grado.out.D50.4[R$`EF(1).T(2).P(3).`==3])
  for(i in 1:length(id.page.name)){
    R2<-R[which(R$PageName==id.page.name[i]),]
    MM<-matrix(10000,1,3)
    #log((R2[which(R2$`EF(1).T(2).P(3).`==1),centralidad_en]+1)/(R2[which(R2$`EF(1).T(2).P(3).`==2),centralidad_en]+1),2)->EF.T_grado.out 
    out.ef<-R2[which(R2$`EF(1).T(2).P(3).`==1),centralidad_en]; (out.ef/max.ef)->out.ef_st
    out.temp<-R2[which(R2$`EF(1).T(2).P(3).`==2),centralidad_en]; (out.temp/max.temp)->out.temp_st
    EF.T_grado.out<-log(((out.ef_st+1)/(out.temp_st+1)),2)
    #log((R2[which(R2$`EF(1).T(2).P(3).`==1),centralidad_en]+1)/(R2[which(R2$`EF(1).T(2).P(3).`==3),centralidad_en]+1),2)->EF.P_grado.out
    out.perm<-R2[which(R2$`EF(1).T(2).P(3).`==3),centralidad_en]; (out.perm/max.perm)->out.perm_st
    EF.P_grado.out<-log((out.ef_st+1)/(out.perm_st+1),2)
    #log((R2[which(R2$`EF(1).T(2).P(3).`==2),centralidad_en]+1)/(R2[which(R2$`EF(1).T(2).P(3).`==3),centralidad_en]+1),2)->T.P_grado.out
    T.P_grado.out<-log((out.temp_st+1)/(out.perm_st+1),2)
    
    if(length(EF.T_grado.out)) {MM[1,1]<-EF.T_grado.out} 
    if(length(EF.P_grado.out)){MM[1,2]<-EF.P_grado.out} 
    if(length(T.P_grado.out)) {MM[1,3]<-T.P_grado.out}
    MM2<-cbind(R2[1,2],R2[1,3],as.numeric(R2[1,9]),MM)
    colnames(MM2)<-c("CENTROID_X","CENTROID_Y","EcoR_ID","EF.T_grado.out","EF.P_grado.out","T.P_grado.out")
    rownames(MM2)<-c(id.page.name[i])
    out<-rbind(out,MM2)
  }
  out
}

#### chequeo
(1+R[which(R$PAGENAME=="DL530"),18][2]/max(R$grado.out.D50.4[R$`EF(1).T(2).P(3).`==2]))->a
(1+R[which(R$PAGENAME=="DL530"),18][3]/max(R$grado.out.D50.4[R$`EF(1).T(2).P(3).`==3]))->b
log(a/b,2)
head(R.out_st)

#####
###### Betweenness; misma centralidad es 0; si no hay alguno de los cuerpos de agua es 10000; sino un valor pos o neg
log_ratio_betweenness<-function(R,centralidad_en){
  out<-NULL
  id.page.name<-(unique(R$PageName))
  max.ef<-max(R$bc.D50.4[R$`EF(1).T(2).P(3).`==1])
  max.temp<-max(R$bc.D50.4[R$`EF(1).T(2).P(3).`==2])
  max.perm<-max(R$bc.D50.4[R$`EF(1).T(2).P(3).`==3])
  for(i in 1:length(id.page.name)){
    R2<-R[which(R$PageName==id.page.name[i]),]
    MM<-matrix(10000,1,3)
    bc.ef<-R2[which(R2$`EF(1).T(2).P(3).`==1),centralidad_en]; (bc.ef/max.ef)->bc.ef_st
    bc.temp<-R2[which(R2$`EF(1).T(2).P(3).`==2),centralidad_en]; (bc.temp/max.temp)->bc.temp_st
    EF.T_bc<-log(((bc.ef_st+1)/(bc.temp_st+1)),2)
    
    bc.perm<-R2[which(R2$`EF(1).T(2).P(3).`==3),centralidad_en]; (bc.perm/max.perm)->bc.perm_st
    EF.P_bc<-log((bc.ef_st+1)/(bc.perm_st+1),2)
    
    T.P_bc<-log((bc.temp_st+1)/(bc.perm_st+1),2)
    
    if(length(EF.T_bc)) {MM[1,1]<-EF.T_bc} 
    if(length(EF.P_bc)){MM[1,2]<-EF.P_bc} 
    if(length(T.P_bc)) {MM[1,3]<-T.P_bc}
    MM2<-cbind(R2[1,2],R2[1,3],as.numeric(R2[1,9]),MM)
    colnames(MM2)<-c("CENTROID_X","CENTROID_Y","EcoR_ID","EF.T_bc","EF.P_bc","T.P_bc")
    rownames(MM2)<-c(id.page.name[i])
    out<-rbind(out,MM2)
  }
  out
}

#### chequeo
#log_ratio_betweenness(R,19)->R.bc_st
(1+R[which(R$PAGENAME=="DL530"),19][1]/max(R$bc.D50.4[R$`EF(1).T(2).P(3).`==1]))->a
(1+R[which(R$PAGENAME=="DL530"),19][2]/max(R$bc.D50.4[R$`EF(1).T(2).P(3).`==2]))->b
log(a/b,2)
head(R.bc_st)

