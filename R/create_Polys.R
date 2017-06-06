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
      # PLon=coordinates(PRO)[,1]
      # PLat=coordinates(PRO)[,2]
      # rm(PRO)
      
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
      Group[i]=Pls
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
      # PLon=coordinates(PRO)[,1]
      # PLat=coordinates(PRO)[,2]
      # rm(PRO)
      
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
      Group[i]=Pls
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

