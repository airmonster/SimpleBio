Calc_SimpleBIO=function(METCRO2D,GRIDCRO2D,GRIDDOT2D,EMIS3D,BIOREP,IFINLINE=F,UPDSMK=T){
  iswater_temp=16
  isice_temp=24
  UPDMETHOD="ADD" # "REP" / "ADD"
  MDSLU=T

  MET1=RNetCDF::open.nc(METCRO2D)
  MET2=RNetCDF::open.nc(GRIDCRO2D)
  MET3=RNetCDF::open.nc(GRIDDOT2D)
  SMK1=RNetCDF::open.nc(EMIS3D,write = T)

  GSW=RNetCDF::var.get.nc(MET1,"GSW")
  T2=RNetCDF::var.get.nc(MET1,"TEMP2")
  PA=RNetCDF::var.get.nc(MET1,"PRSFC")*0.01
  VG=RNetCDF::var.get.nc(MET1,"VEG")
  LATD=RNetCDF::var.get.nc(MET2,"LAT")
  LOND=RNetCDF::var.get.nc(MET2,"LON")
  COL=length(T2[,1,1])
  ROW=length(T2[1,,1])
  TSP=length(T2[1,1,])
  DX=RNetCDF::att.get.nc(MET1,"NC_GLOBAL","XCELL")

  print("CASEINFO:")
  print(paste0("ROW=",ROW,"; COL=",COL,"; DX=",DX,"; TS=",TSP))

  GRID_S=length(LATD[,1])*length(LATD[1,])*DX*DX/1000000 #KM2

  aefiso=array(0,dim=c(25))
  aefmter=array(0,dim=c(25))
  aefovoc=array(0,dim=c(25))
  aef_n=array(0,dim=c(25))
  ixxxlu=array(0,dim=c(25))
  modismap=array(0,dim=c(20))

  #USGS as default land use type.
  city_plant_rate=0.3
  aefiso[1] = 4400*city_plant_rate  #CITY ISOP=MAINTYPE*GREENFRAC
  aefiso[2] = 8.
  aefiso[3] = 8.
  aefiso[4] = 8.
  aefiso[5] = 4.
  aefiso[6] = 2204.
  aefiso[7] = 0.
  aefiso[8] = 0.
  aefiso[9] = 0.
  aefiso[10] = 0.
  aefiso[11] = 4400.
  aefiso[12] = 780.
  aefiso[13] = 4400.
  aefiso[14] = 780.
  aefiso[15] = 5775.
  aefiso[16] = 0.
  aefiso[17] = 0.
  aefiso[18] = 5775.
  aefiso[19] = 0.
  aefiso[20] = 70.
  aefiso[21] = 70.
  aefiso[22] = 70.
  aefiso[23] = 0.
  aefiso[24] = 0.
  aefiso[25] = 0.

  aefmter[1] = 385*city_plant_rate
  aefmter[2] = 20.
  aefmter[3] = 20.
  aefmter[4] = 20.
  aefmter[5] = 20.
  aefmter[6] = 202.5
  aefmter[7] = 20.
  aefmter[8] = 20.
  aefmter[9] = 20.
  aefmter[10] = 0
  aefmter[11] = 385.
  aefmter[12] = 1380.
  aefmter[13] = 385.
  aefmter[14] = 1380.
  aefmter[15] = 1001.
  aefmter[16] = 0.
  aefmter[17] = 0.
  aefmter[18] = 1001.
  aefmter[19] = 0.
  aefmter[20] = 0.
  aefmter[21] = 0.
  aefmter[22] = 0.
  aefmter[23] = 0.
  aefmter[24] = 0.
  aefmter[25] = 0.

  aefovoc[1] = 0.
  aefovoc[2] = 12.
  aefovoc[3] = 12.
  aefovoc[4] = 12.
  aefovoc[5] = 46.
  aefovoc[6] = 363.5
  aefovoc[7] = 80.
  aefovoc[8] = 80.
  aefovoc[9] = 80.
  aefovoc[10] = 0
  aefovoc[11] = 715.
  aefovoc[12] = 840.
  aefovoc[13] = 715.
  aefovoc[14] = 840.
  aefovoc[15] = 924.
  aefovoc[16] = 0.
  aefovoc[17] = 0.
  aefovoc[18] = 924.
  aefovoc[19] = 0.
  aefovoc[20] = 0.
  aefovoc[21] = 0.
  aefovoc[22] = 0.
  aefovoc[23] = 0.
  aefovoc[24] = 0.
  aefovoc[25] = 0.

  aef_n[1] = 0.07*city_plant_rate
  aef_n[2] = 30.0 #9. MODI:http://soils.issas.ac.cn/tr/ch/reader/create_pdf.aspx?file_no=tr201305129&flag=1&journal_id=tr&year_id=2013
  aef_n[3] = 30.0 #9.
  aef_n[4] = 50.0 #9.
  aef_n[5] = 25.0 #4.95
  aef_n[6] = 25.0 #4.535
  aef_n[7] = 0.9
  aef_n[8] = 0.07
  aef_n[9] = 0.07
  aef_n[10] = 0.
  aef_n[11] = 0.07
  aef_n[12] = 0.07
  aef_n[13] = 0.07
  aef_n[14] = 0.07
  aef_n[15] = 0.07
  aef_n[16] = 0.
  aef_n[17] = 0.
  aef_n[18] = 0.07
  aef_n[19] = 0.
  aef_n[20] = 0.
  aef_n[21] = 0.
  aef_n[22] = 0.
  aef_n[23] = 0.
  aef_n[24] = 0.
  aef_n[25] = 0.

  ixxxlu[1] = 4
  ixxxlu[2] = 2
  ixxxlu[3] = 2
  ixxxlu[4] = 2
  ixxxlu[5] = 2
  ixxxlu[6] = 4
  ixxxlu[7] = 3
  ixxxlu[8] = 6
  ixxxlu[9] = 3
  ixxxlu[10] = 6
  ixxxlu[11] = 4
  ixxxlu[12] = 5
  ixxxlu[13] = 4
  ixxxlu[14] = 5
  ixxxlu[15] = 5
  ixxxlu[16] = 0
  ixxxlu[17] = 6
  ixxxlu[18] = 4
  ixxxlu[19] = 1
  ixxxlu[20] = 6
  ixxxlu[21] = 4
  ixxxlu[22] = 6
  ixxxlu[23] = 1
  ixxxlu[24] = 0
  ixxxlu[25] = 1

  modismap[1]=14
  modismap[2]=13
  modismap[3]=12
  modismap[4]=11
  modismap[5]=15
  modismap[6]=8
  modismap[7]=9
  modismap[8]=8
  modismap[9]=10
  modismap[10]=7
  modismap[11]=17
  modismap[12]=2
  modismap[13]=1
  modismap[14]=5
  modismap[15]=24
  modismap[16]=19
  modismap[17]=16
  modismap[18]=21
  modismap[19]=22
  modismap[20]=23

  biogen=function(iland,ta,rad,pa,mminlu,vegflag,ludf,dx,vegfrc,bid){
    alpha=0.0027
    cl1=1.066
    r=8.314
    ct1=95000
    ct2=230000
    tm1=314
    ts1=303
    beta=0.09
    cl=1
    ct=1

    if(ixxxlu[iland]==4 || ixxxlu[iland]==5){
      #print(paste0("dtermlxxx:4,5"))
      par = 2.0*rad
      cl = alpha*cl1*par/sqrt(1+alpha*alpha*par*par)
      ct = exp(ct1*(ta-ts1)/(r*ts1*ta))/(1+exp(ct2*(ta-tm1)/(r*ts1*ta)))
      ecf_iso = cl*ct
      ecf_mter = exp(beta*(ta-ts1))
      ecf_ovoc = ecf_mter
      tsoil = 0.84*(ta-273.15) + 3.6
      ecf_n = exp(0.071*tsoil)
    }

    if(ixxxlu[iland]==2){
      #print(paste0("dtermlxxx:2"))
      ecf_iso = exp(0.1*(ta-30.-273.15))
      ecf_mter = ecf_iso
      ecf_ovoc = ecf_iso
      tsoil = 0.72*(ta-273.15) + 5.8
      ecf_n = exp(0.071*tsoil)
    }

    if(ixxxlu[iland]==3 || ixxxlu[iland]==6){
      #print(paste0("dtermlxxx:3,6"))
      ecf_iso = exp(0.1*(ta-30.-273.15))
      ecf_mter = ecf_iso
      ecf_ovoc = ecf_iso
      tsoil = 0.66*(ta-273.15) + 8.8
      ecf_n = exp(0.071*tsoil)
    }

    if(ixxxlu[iland]==1 || iland==iswater_temp || iland==isice_temp){
      #print(paste0("dtermlxxx:1"))
      ecf_iso=0
      ecf_mter = 0.
      ecf_ovoc = 0.
      ecf_n = 0.
    }

    tvgf=vegfrc

    for(j in 1:length(vegfrc[1,1,])){
      tvgf[,,j]=vegfrc[,,j]*ludf
    }

    rat=ta/pa

    coniso = 68.11/60/1E12  #rat*2.3095E-5
    eisoc = aefiso[iland]*ecf_iso
    eiso = coniso*eisoc*tvgf*dx*dx #/68.117/3600 #Mole/s NEW
    conter = 134.3564/120/1E12 #rat*1.1548E-5
    emterc = aefmter[iland]*ecf_mter
    emter = conter*emterc*tvgf*dx*dx #/134.3564/1000/3600
    conovoc = 142/96/1E12 #rat*1.4435E-5 #3-hexenyl-acetate,C8H14O2
    eovocc = aefovoc[iland]*ecf_ovoc
    eovoc = conovoc*eovocc
    vocsc = eisoc + emterc + eovocc
    conn = 30/14/1E15*3600 #rat*3.5633E-4
    e_nn = aef_n[iland]*ecf_n
    e_n = conn*e_nn*tvgf*dx*dx #/30/1000/3600

    if(bid==1){
      return(eiso)
    }

    if(bid==2){
      return(emter)
    }

    if(bid==3){
      return(e_n)
    }

  }



  vegflag=FALSE

  getlu=function(luid){
    if(luid<10){
      LUNAME=paste0("LUFRAC_0",luid)
    }else{
      LUNAME=paste0("LUFRAC_",luid)
    }
    LUDF=RNetCDF::var.get.nc(MET2,LUNAME)
    return(LUDF)
  }


  genbiovoc=function(bioid,mdslu){
    #1=ISOP,2=TERP,3=NO
    if(mdslu==FALSE){
      mxlu=24
      i=1
      ild=1
      outdf=biogen(ild,T2,GSW,PA,mminlu[ild],vegflag,getlu(i),DX,VG,bioid)

      for( i in 2:mxlu){
        ild=i
        outdf=outdf+biogen(ild,T2,GSW,PA,mminlu[ild],vegflag,getlu(i),DX,VG,bioid)
      }
    }else{
      mxlu=20
      i=1
      ild=modismap[1]
      outdf=biogen(ild,T2,GSW,PA,mminlu[ild],vegflag,getlu(i),DX,VG,bioid)
      for( i in 2:mxlu){
        ild=modismap[i]
        outdf=outdf+biogen(ild,T2,GSW,PA,mminlu[ild],vegflag,getlu(i),DX,VG,bioid)
      }
    }
    return(outdf)
  }

  CALGRIDSUM=function(GDDF){
    tsumdf=GDDF[,,1]
    for(tis in 2:length(GDDF[1,1,])){
      tsumdf=tsumdf+GDDF[,,tis]
    }
    return(tsumdf)
  }

  FormatGrid=function(ingrdf){
    if(IFINLINE==T){
      ougrdf=array(0,dim=c(COL,ROW,1,TSP))
      ougrdf[,,1,]=ingrdf
      return(ougrdf)
    }else{
      return(ingrdf)
    }
  }

  #genbiovoc单位为PPM*Mol/网格Min，逐小时,故*MW转换为g，再*1E6去掉PPM，/1E12转为t，*60转换为hr，得到的单位为t/hr`grid
  #in t
  ISOPB=genbiovoc(1,MDSLU) # 68.1100
  TERPB=genbiovoc(2,MDSLU) #134.3564
  NOB=genbiovoc(3,MDSLU)   # 30.0000

  EMR=sum(CALGRIDSUM(ISOPB))*12/GRID_S


  rep=data.frame(POL="ISOP",VAL=mean(ISOPB)*length(T2[1,1,])*length(T2[1,,1])*length(T2[,1,1]),UNIT="t")
  rep=rbind(rep,data.frame(POL="TERP",VAL=mean(TERPB)*length(T2[1,1,])*length(T2[1,,1])*length(T2[,1,1]),UNIT="t"))
  rep=rbind(rep,data.frame(POL="NO",VAL=mean(NOB)*length(T2[1,1,])*length(T2[1,,1])*length(T2[,1,1]),UNIT="t"))


  SMKISOP=RNetCDF::var.get.nc(SMK1,"ISOP")
  SMKTERP=RNetCDF::var.get.nc(SMK1,"TERP")
  SMKNO=RNetCDF::var.get.nc(SMK1,"NO")
  if (IFINLINE == T){
    print("NOTICE: Calculating BVOC for INLINE CMAQ.")
    if (UPDMETHOD=="ADD"){
      SMKISOP[,,]=SMKISOP[,,]+ISOPB*1E6/68.11/3600 #==> mols/s
      SMKTERP[,,]=SMKTERP[,,]+TERPB*1E6/134.3564/3600
      SMKNO[,,]=SMKNO[,,]+NOB*1E6/30/3600
    }else{
      SMKISOP[,,]=ISOPB*1E6/68.11/3600 #==> mols/s
      SMKTERP[,,]=TERPB*1E6/134.3564/3600
      SMKNO[,,]=SMKNO[,,]+NOB*1E6/30/3600
    }
  }else{
    if (UPDMETHOD=="ADD"){
      SMKISOP[,,1,]=SMKISOP[,,1,]+ISOPB*1E6/68.11/3600 #==> mols/s
      SMKTERP[,,1,]=SMKTERP[,,1,]+TERPB*1E6/134.3564/3600
      SMKNO[,,1,]=SMKNO[,,1,]+NOB*1E6/30/3600
    }else{
      SMKISOP[,,1,]=ISOPB*1E6/68.11/3600 #==> mols/s
      SMKTERP[,,1,]=TERPB*1E6/134.3564/3600
      SMKNO[,,1,]=SMKNO[,,1,]+NOB*1E6/30/3600
    }
  }

  if (UPDSMK==T){
    RNetCDF::var.put.nc(SMK1,variable = "ISOP",data = FormatGrid(SMKISOP))
    RNetCDF::var.put.nc(SMK1,variable = "TERP",data = FormatGrid(SMKTERP))
    RNetCDF::var.put.nc(SMK1,variable = "NO",data = FormatGrid(SMKNO))
  }

  TISOP=CALGRIDSUM(ISOPB)
  TTERP=CALGRIDSUM(TERPB)
  TNO=CALGRIDSUM(NOB)
  RESDF=data.frame(LAT=as.vector(LATD),LON=as.vector(LOND),ISOP=as.vector(TISOP),TERP=as.vector(TTERP),NO=as.vector(TNO))

  CreateNC=T
  if(CreateNC==T){
    ONC=RNetCDF::create.nc(BIOREP)
    RNetCDF::dim.def.nc(ONC,"TSTEP",unlim=T) #0
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","TSTEP",ONC,"NC_GLOBAL")
    RNetCDF::dim.def.nc(ONC,"DATE-TIME",2)   #1
    RNetCDF::dim.def.nc(ONC,"LAY",1)         #2
    RNetCDF::dim.def.nc(ONC,"VAR",3)         #3
    RNetCDF::dim.def.nc(ONC,"ROW",length(T2[1,,1]))  #4
    RNetCDF::dim.def.nc(ONC,"COL",length(T2[,1,1]))  #5
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","IOAPI_VERSION","NC_CHAR","$Id: @(#) ioapi library version 3.0 $                                           ")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","EXEC_ID","NC_CHAR","????????????????                                                                ")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","FTYPE","NC_CHAR","1")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","CDATE","NC_CHAR","2015304")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","CTIME","NC_CHAR","104513")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","WDATE","NC_CHAR","2015304")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","WTIME","NC_CHAR","104513")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","SDATE","NC_CHAR","2013001")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","STIME","NC_CHAR","0")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","TSTEP","NC_CHAR","10000")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","NTHIK","NC_CHAR","1")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","NCOLS","NC_DOUBLE",as.double(COL))
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","NROWS","NC_DOUBLE",as.double(ROW))
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","NLAYS","NC_CHAR","1")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","NVARS","NC_CHAR","3")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","GDTYP","NC_CHAR","2")
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","P_ALP",ONC,"NC_GLOBAL")
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","P_BET",ONC,"NC_GLOBAL")
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","P_GAM",ONC,"NC_GLOBAL")
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","XCENT",ONC,"NC_GLOBAL")
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","YCENT",ONC,"NC_GLOBAL")
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","XORIG",ONC,"NC_GLOBAL")
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","YORIG",ONC,"NC_GLOBAL")
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","XCELL",ONC,"NC_GLOBAL")
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","YCELL",ONC,"NC_GLOBAL")
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","VGTYP",ONC,"NC_GLOBAL")
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","VGTOP",ONC,"NC_GLOBAL")
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","GDNAM",ONC,"NC_GLOBAL")
    RNetCDF::att.copy.nc(MET1,"NC_GLOBAL","UPNAM",ONC,"NC_GLOBAL")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","VAR-LIST","NC_CHAR","ISOP TERP NO")
    RNetCDF::att.put.nc(ONC,"NC_GLOBAL","HISTORY","NC_CHAR","")
    RNetCDF::var.def.nc(ONC,"ISOP","NC_DOUBLE",c(5,4,2,0))
    RNetCDF::var.def.nc(ONC,"TERP","NC_DOUBLE",c(5,4,2,0))
    RNetCDF::var.def.nc(ONC,"NO","NC_DOUBLE",c(5,4,2,0))
    tmparrdf=array(0,dim=c(COL,ROW,1,TSP))
    tmparrdf[,,1,]=ISOPB*1E6/68.11/3600
    RNetCDF::var.put.nc(ONC,variable = "ISOP",data = tmparrdf)
    tmparrdf[,,1,]=TERPB*1E6/134.3564/3600
    RNetCDF::var.put.nc(ONC,variable = "TERP",data = tmparrdf)
    tmparrdf[,,1,]=NOB*1E6/30/3600
    RNetCDF::var.put.nc(ONC,variable = "NO",data = tmparrdf)
    RNetCDF::close.nc(ONC)
  }


  RNetCDF::close.nc(MET1)
  RNetCDF::close.nc(MET2)
  RNetCDF::close.nc(MET3)
  RNetCDF::close.nc(SMK1)

  print(rep)
}
