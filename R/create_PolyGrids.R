#' Create a Polygon Grid that is compatible with CCAMLR online GIS 
#'
#' Create a Polygon Grid that is compatible with CCAMLR online GIS 
#'
#' @param InputFile  currently, the name of the input data file in quotes e.g. "DataFile.csv", but I want to change this so you can choose between R dataframe or csv file 
#' @param OutputFormat is "ROBJECT" is the default and returns as SpatialPolygonDataFrame  to your R work enviornment and "SHAPEFILE" will write an ESRI Shapefile
#' @param OutputName  if "SHAPEFILE" format is specified then supply the name of the output shapefile in quotes e.g."MyShape", the default is NULL and assumes an "ROBJECT" format 
#' @param dlon width of the grid cells in decimal degrees of longitude e.g. dlon=1
#' @param dlat height of the grid cells in decimal degrees of latitude e.g.: dlat=0.5
#' @param Type set according to the type of data to be gridded. If it is numerical data (e.g. Krill length, tootfish catch; see Table 4) set Type="NUM". If it is categorical data (e.g. CCAMLR Species Code, Vessel name; see Table 5), set Type="CAT".
#' @import rgeos rgdal raster sp
#' @export


create_PolyGrids=function(InputFile,OutputFormat="ROBJECT",OutputName=" ",dlon,dlat,Type){
  if (class(InputFile)=="character"){
    data=read.csv(InputFile)}else{
    data=InputFile  
    }
  
  # suppressWarnings(SpatialPolygons())
  # 
  # suppressMessages("In `[<-`(`*tmp*`, i, value = <S4 object of class Polygons>):implicit list embedding of S4 objects is deprecated")
  
  lon=data[,1]
  lat=data[,2]
  Val=data[,3]
  # Define CRS projection
  CRSProj="+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # Prepare Group, the output file which will contain all polygons
  Group=list()
  GroupData=NULL
  # Prepare Grid
  Glon=(ceiling((lon+dlon)/dlon)*dlon-dlon)-dlon/2
  Glat=(ceiling((lat+dlat)/dlat)*dlat-dlat)-dlat/2
  
  # Set Categorical (Type="CAT") or numerical variable (Type="NUM")
  if (Type=="NUM"){
    GVal=Val
    GriddedData=as.data.frame(cbind(Glon,Glat,GVal))
    
    GriddedData_Mean=aggregate(GVal~Glon+Glat,data=GriddedData,mean)
    
    GLONS=GriddedData_Mean[,1]
    GLATS=GriddedData_Mean[,2]
    MEANS=GriddedData_Mean[,3]
    
    GriddedData_Sum=aggregate(GVal~Glon+Glat,data=GriddedData,sum)
    SUMS=GriddedData_Sum[,3]
    GriddedData_Median=aggregate(GVal~Glon+Glat,data=GriddedData, stats::median)
    MEDIANS=GriddedData_Median[,3]
    GriddedData_Max=aggregate(GVal~Glon+Glat,data=GriddedData,max)
    MAXS=GriddedData_Max[,3]
    GriddedData_Min=aggregate(GVal~Glon+Glat,data=GriddedData,min)
    MINS=GriddedData_Min[,3]
    GriddedData_StDev=aggregate(GVal~Glon+Glat,data=GriddedData,sd)
    STDEVS=GriddedData_StDev[,3]
    GriddedData_Count=aggregate(rep(1,length(Glon))~Glon+Glat,data=GriddedData,sum)
    COUNTS=GriddedData_Count[,3]
    
    for (i in (1:length(GLONS))){ #Loop over polygons
      
      PLon=c(GLONS[i]-dlon/2,
             GLONS[i]+dlon/2,
             GLONS[i]+dlon/2,
             GLONS[i]-dlon/2)
      PLat=c(GLATS[i]+dlat/2,
             GLATS[i]+dlat/2,
             GLATS[i]-dlat/2,
             GLATS[i]-dlat/2)
      Pmean=round(MEANS[i],2)
      Psum=round(SUMS[i],2)
      Pmed=round(MEDIANS[i],2)
      Pmax=round(MAXS[i],2)
      Pmin=round(MINS[i],2)
      Pstdev=round(STDEVS[i],2)
      Pcount=COUNTS[i]
      
      #Project Lat/Lon
      PRO=project(cbind(PLon,PLat),CRSProj)
      PLon=c(PRO[,1],PRO[1,1])
      PLat=c(PRO[,2],PRO[1,2])
      rm(PRO)
      
      #Create individual Polygons
      Pl=Polygon(cbind(PLon,PLat))
      Pls=Polygons(list(Pl), ID=i)
      SPls=SpatialPolygons(list(Pls))
      
      df=data.frame(name=names(data)[3],row.names=i,Pcount,Pmax,Pmin,Pmean,Pstdev,Pmed,Psum)
      colnames(df)[2:8]=c("Count","Max","Min","Mean","StdDev","Median","Sum")
      
      SPDF=SpatialPolygonsDataFrame(SPls, df)
      proj4string(SPDF)=CRS(CRSProj)
      
      #Add each polygon to the Group
      Group[i]=Pls
      GroupData=rbind(GroupData,df)
    }
    
    #Collate Group
    Group=SpatialPolygons(Group)
    Group=SpatialPolygonsDataFrame(Group,GroupData)
    proj4string(Group)=CRS(CRSProj)
    if(OutputFormat=="SHAPEFILE"){
    writeOGR(Group,".",OutputName,driver="ESRI Shapefile")
    }else{
      return(Group)
    }
  }#End of numerical variables
  if (Type=="CAT"){#Start of categorical variables
    
    GVal=as.character(Val)
    GVals=sort(unique(GVal))
    
    VAL=NULL
    for (icat in GVals){
      CountVal=rep(0,length(GVal))
      CountVal[GVal==icat]=1
      GriddedData=as.data.frame(cbind(Glon,Glat,CountVal))
      GriddedData_CountVal=aggregate(CountVal~Glon+Glat,data=GriddedData,sum)
      VAL=rbind(VAL,GriddedData_CountVal[,3])
    }
    
    
    GriddedData_CountALL=aggregate(rep(1,length(Glon))~Glon+Glat,data=GriddedData,sum)
    GLONS=GriddedData_CountALL[,1]
    GLATS=GriddedData_CountALL[,2]
    ALL=GriddedData_CountALL[,3]
    
    
    
    for (i in (1:length(GLONS))){ #Loop over polygons
      
      PLon=c(GLONS[i]-dlon/2,
             GLONS[i]+dlon/2,
             GLONS[i]+dlon/2,
             GLONS[i]-dlon/2)
      PLat=c(GLATS[i]+dlat/2,
             GLATS[i]+dlat/2,
             GLATS[i]-dlat/2,
             GLATS[i]-dlat/2)
      
      PCountALL=ALL[i]
      PCountVal=VAL[,i]
      Perc=round(100*PCountVal/PCountALL,2)
      #Project Lat/Lon
      PRO=project(cbind(PLon,PLat),CRSProj)
      PLon=c(PRO[,1],PRO[1,1])
      PLat=c(PRO[,2],PRO[1,2])
      rm(PRO)
      
      #Create individual Polygons
      Pl=Polygon(cbind(PLon,PLat))
      Pls=suppressWarnings(Polygons(list(Pl), ID=i))
      SPls=suppressWarnings(SpatialPolygons(list(Pls)))
      
      df=data.frame(name=names(data)[3],row.names=i,PCountALL,t(PCountVal),t(Perc))
      colnames(df)[2:(dim(df)[2])]=c("All",GVals,paste(GVals,"(%)",sep=""))
      
      SPDF=SpatialPolygonsDataFrame(SPls, df)
      proj4string(SPDF)=CRS(CRSProj)
      
      #Add each polygon to the Group
      Group[i]=Pls
      GroupData=rbind(GroupData,df)
    }
    
    #Collate Group
    Group=SpatialPolygons(Group)
    Group=SpatialPolygonsDataFrame(Group,GroupData)
    proj4string(Group)=CRS(CRSProj)
    
    if(OutputFormat=="SHAPEFILE"){
    writeOGR(Group,".",OutputName,driver="ESRI Shapefile")
    }else{
      return(Group)
    }
  }
  if (Type!="CAT" & Type!="NUM"){cat("Wrong 'Type'")}
}
