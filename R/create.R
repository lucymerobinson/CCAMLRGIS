#' Create Polygons that are compatible with CCAMLR online GIS 
#'
#' Create Polygons that are compatible with CCAMLR online GIS 
#'
#' @param InputFile  the name of the input data file in quotes e.g. "DataFile.csv" or an R dataframe 
#' @param OutputFormat is "ROBJECT" is the default and returns as SpatialPolygonDataFrame  to your R work enviornment and "SHAPEFILE" will write an ESRI Shapefile
#' @param OutputName  if "SHAPEFILE" format is specified then supply the name of the output shapefile in quotes e.g."MyShape", the default is NULL and assumes an "ROBJECT" format 
#' @param Buffer is the value in nautical miles to apply to the line. The default value is 0, assuming no Buffer
#' @param Separate currently, if the data file contains several items (e.g. several fishing lines) you may wish to produce one shapefile per item to display them separately on a map. In such a case, set Separate to 1. I want to remove this as I think you can manage this at the input end.
#' @param Densify is set to 1 as a default, which will add additional points between points of equal latitude when data are projected. If set to 0 then no additional points will be added 
#' @param Clip "Coast_Low" will clip a polygon that intersect with the coastline to remove the land and keep only the ocean area, "Coast_Medium" is a higher resolution coastline is also provided with the package, the default is set to 0 which assumes no clipping is required
#' @import rgeos rgdal raster sp
#' @export
#' @examples 
#' ## specify the name of the Research block
#' 
#' Name <-"5841_6"
#' 
#' ## specify the Longitude coordinates in decimal degrees
#' 
#' Lons <- c(130, 130, 134, 134)
#' 
#' ## specify the Latitude coordinates in decimal degrees
#' 
#' Lats <- c(-64.0, -65.5, -65.5,-64.0)
#' 
#' ## bind information together into a dataframe 
#' 
#' Coords <- data.frame(Name=rep(Name,length(Lons)),Lats=Lats,Lons=Lons)
#' 
#' ## create polygon of proposed Research area
#' 
#' New_RBs <-create_Polys(Coords)

create_Polys=function(InputFile,OutputFormat="ROBJECT",OutputName=NULL,Buffer=0,Separate=0,Densify=1,Clip=0){
  # Load data
  if (class(InputFile)=="character"){
  data=read.csv(InputFile)}else{
  data=InputFile  
  }
  IDs=as.character(data[,1])
  ListIDs=sort(unique(IDs))
  Lats=data[,2]
  Lons=data[,3]
  if (dim(data)[2]>3){XtraFields=data[,(4:(dim(data)[2]))]}
  # Prepare Group, the output file which will contain all polygons
  Group=list()
  GroupData=NULL
  # Define CRS projection
  CRSProj="+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # Set Buffer (Convert Nautical miles to meters)
  Buffer=Buffer*1852
  
  if (Densify==1){
    for (i in (1:length(ListIDs))){ #Loop over polygons
      
      PID=ListIDs[i]
      PLon=Lons[IDs==PID]
      PLat=Lats[IDs==PID]
      
      if ((dim(data)[2])==3){PVal=NULL}
      if ((dim(data)[2])==4){PVal=unique(XtraFields[IDs==PID])
      if(length(PVal)>1){cat(paste("ERROR: There is more than one value in metadata associated with polygon",PID),"\n");break}}
      if ((dim(data)[2])>4){PVal=unique(XtraFields[IDs==PID,])
      if(dim(PVal)[1]>1){cat(paste("ERROR: There is more than one value in metadata associated with polygon",PID),"\n");break}}
      
      #Close polygon before densifying
      PLon=c(PLon,PLon[1])
      PLat=c(PLat,PLat[1])
      #Densify
      Densified=DensifyData(Lon=PLon,Lat=PLat)
      PLon=Densified[,1]
      PLat=Densified[,2]
      rm(Densified)
      
      Coords<-data.frame(PLon=PLon,PLat=PLat)
      coordinates(Coords)<- ~ PLon + PLat
      proj4string(Coords)<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
      
      #Project Lat/Lon
      PRO=spTransform(Coords,CRS(CRSProj))
 
      
      #Add Buffer
      if (Buffer>0){
        # Pl_test=Polygon(cbind(PLon,PLat))
        Pl=Polygon(PRO)
        Pls=Polygons(list(Pl),ID="NA")
        SPls=SpatialPolygons(list(Pls),proj4string = CRS(proj4string(PRO)))
        Buffered=gBuffer(SPls,width=Buffer)
        PLon=Buffered@polygons[[1]]@Polygons[[1]]@coords[,1]
        PLat=Buffered@polygons[[1]]@Polygons[[1]]@coords[,2]
        rm(Pl,Pls,SPls,Buffered)}
      
      #Clip or not
      Pl=Polygon(PRO)
      if (Clip==0){
        Pls=Polygons(list(Pl), ID=PID)
      }else{
        cat(paste("Start clipping polygon",PID),"\n")
        Pls=Clip2Coast(Pl,Coastline=Clip,ID=PID)
        cat(paste("End clipping polygon",PID),"\n") 
      }
      
      SPls=SpatialPolygons(list(Pls))
      PArea=round(gArea(SPls, byid=F)/1000000)
      
      if ((dim(data)[2])==3){df=data.frame(name=PID,row.names=PID,AreaKm2=PArea)}
      if ((dim(data)[2])==4){df=data.frame(name=PID,row.names=PID,PVal,AreaKm2=PArea);colnames(df)[2]=names(data)[4]}
      if ((dim(data)[2])>4){df=data.frame(name=PID,row.names=PID,PVal,AreaKm2=PArea)}
      
      SPDF=SpatialPolygonsDataFrame(SPls, df)
      proj4string(SPDF)=CRS(CRSProj)
      
      if(OutputFormat=="SHAPEFILE"){
      if (Separate==1 & Buffer==0){writeOGR(SPDF,".",paste(OutputName,"_",as.character(PID),sep=""),driver="ESRI Shapefile")}
      if (Separate==1 & Buffer!=0){writeOGR(SPDF,".",paste(OutputName,"_Buffered_",as.character(PID),sep=""),driver="ESRI Shapefile")}
      }
      #Add each polygon to the Group
      Group[[i]] = Pls
      GroupData=rbind(GroupData,df)
    }
  } #end loop yes densify
  
  if (Densify==0){
    for (i in (1:length(ListIDs))){ #Loop over polygons
      
      PID=ListIDs[i]
      PLon=Lons[IDs==PID]
      PLat=Lats[IDs==PID]
      
      if ((dim(data)[2])==3){PVal=NULL}
      if ((dim(data)[2])==4){PVal=unique(XtraFields[IDs==PID])
      if(length(PVal)>1){cat(paste("ERROR: There is more than one value in metadata associated with polygon",PID),"\n");break}}
      if ((dim(data)[2])>4){PVal=unique(XtraFields[IDs==PID,])
      if(dim(PVal)[1]>1){cat(paste("ERROR: There is more than one value in metadata associated with polygon",PID),"\n");break}}
      #Close polygon
      PLon=c(PLon,PLon[1])
      PLat=c(PLat,PLat[1])
      
      Coords<-data.frame(PLon=PLon,PLat=PLat)
      coordinates(Coords)<-c("PLon","Plat")
      proj4string(Coords)<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
      
      #Project Lat/Lon
      PRO=spTransform(Coords,CRS(CRSProj))

      
      #Add Buffer
      if (Buffer>0){
        # Pl=Polygon(cbind(PLon,PLat))
        Pl=Polygon(PRO)
        Pls=Polygons(list(Pl),ID="NA")
        SPls=SpatialPolygons(list(Pls))
        Buffered=gBuffer(SPls,width=Buffer,CRS(proj4string(PRO)))
        PLon=Buffered@polygons[[1]]@Polygons[[1]]@coords[,1]
        PLat=Buffered@polygons[[1]]@Polygons[[1]]@coords[,2]
        rm(Pl,Pls,SPls,Buffered)}
      
      #Clip or not
      Pl=Polygon(PRO)
      if (Clip==0){
        Pls=Polygons(list(Pl), ID=PID)
      }else{
        cat(paste("Start clipping polygon",PID),"\n")
        Pls=Clip2Coast(Pl,Coastline=Clip,ID=PID)
        cat(paste("End clipping polygon",PID),"\n") 
      }
      
      SPls=SpatialPolygons(list(Pls))
      PArea=round(gArea(SPls, byid=F)/1000000)
      
      if ((dim(data)[2])==3){df=data.frame(name=PID,row.names=PID,AreaKm2=PArea)}
      if ((dim(data)[2])==4){df=data.frame(name=PID,row.names=PID,PVal,AreaKm2=PArea);colnames(df)[2]=names(data)[4]}
      if ((dim(data)[2])>4){df=data.frame(name=PID,row.names=PID,PVal,AreaKm2=PArea)}
      

      SPDF=SpatialPolygonsDataFrame(SPls, df)
      proj4string(SPDF)=CRS(CRSProj)
      if(OutputFormat=="SHAPEFILE"){
      if (Separate==1 & Buffer==0){writeOGR(SPDF,".",paste(OutputName,"_",as.character(PID),sep=""),driver="ESRI Shapefile")}
      if (Separate==1 & Buffer!=0){writeOGR(SPDF,".",paste(OutputName,"_Buffered_",as.character(PID),sep=""),driver="ESRI Shapefile")}
      }
      
      #Add each polygon to the Group
      Group[[i]] = Pls
      GroupData=rbind(GroupData,df)
    }
  } #end loop no densify
  
  Group=SpatialPolygons(Group)
  Group=SpatialPolygonsDataFrame(Group,GroupData)
  proj4string(Group)=CRS(CRSProj)
  if(OutputFormat=="SHAPEFILE"){
  if (Separate==1 & Buffer==0){writeOGR(Group,".",paste(OutputName,"_Group",sep=""),driver="ESRI Shapefile")}
  if (Separate==1 & Buffer!=0){writeOGR(Group,".",paste(OutputName,"_Buffered_Group",sep=""),driver="ESRI Shapefile")}
  if (Separate==0 & Buffer==0){writeOGR(Group,".",OutputName,driver="ESRI Shapefile")}
  if (Separate==0 & Buffer!=0){writeOGR(Group,".",paste(OutputName,"_Buffered",sep=""),driver="ESRI Shapefile")}
  }else{
    return(Group)
  }  
}

#' Create a Polygon Grid that is compatible with CCAMLR online GIS 
#'
#' Create a Polygon Grid that is compatible with CCAMLR online GIS 
#'
#' @param InputFile  the name of the input data with format = Longitude, Latitude and Value
#' @param OutputFormat is "ROBJECT" is the default and returns as SpatialPolygonDataFrame  to your R work enviornment and "SHAPEFILE" will write an ESRI Shapefile
#' @param OutputName  if "SHAPEFILE" format is specified then supply the name of the output shapefile in quotes e.g."MyShape", the default is NULL and assumes an "ROBJECT" format 
#' @param dlon width of the grid cells in decimal degrees of longitude e.g. dlon=1
#' @param dlat height of the grid cells in decimal degrees of latitude e.g.: dlat=0.5
#' @param Type set according to the type of data to be gridded. If it is numerical data (e.g. Krill length, tootfish catch; see Table 4) set Type="NUM". If it is categorical data (e.g. CCAMLR Species Code, Vessel name; see Table 5), set Type="CAT".
#' @import rgeos rgdal raster sp
#' @importFrom stats median sd
#' @importFrom utils read.csv
#' @export

create_PolyGrids=function(InputFile,OutputFormat="ROBJECT",OutputName=NULL,dlon,dlat,Type){
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
    GriddedData_Median=aggregate(GVal~Glon+Glat,data=GriddedData,median)
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
      Group[[i]]=Pls
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
      Group[[i]]=Pls
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
