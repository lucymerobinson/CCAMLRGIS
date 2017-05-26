#' Create Points that are compatible with CCAMLR online GIS 
#'
#' Create Points that are compatible with CCAMLR online GIS 
#'
#' @param InputFile  currently, the name of the input data file in quotes e.g. "DataFile.csv", but I want to change this so you can choose between R dataframe or csv file 
#' @param OutputName  currently, the name of the output shapefile in quotes e.g."MyShape" but I want to change this to "ROBJECT" is stored in work environment, "RDATA" will write an .rda file or "SHAPEFILE" with write an ESRI Shapefile
#' @param Buffer is the value in nautical miles to apply to the line. The default value is 0, assuming no Buffer
#' @param Separate currently, If the data file contains several items (e.g. several fishing lines) you may wish to produce one shapefile per item to display them separately on a map. In such a case, set Separate to 1. I want to remove this as I think you can manage this at the input end.
#' @param Clip "Coast_Low" will clip a line of polygon that intersect with the coastline to remove the land and keep only the ocean area, "Coast_Medium" is a higher resolution coastline is also provided with the package, the default is set to 0 which assumes no clipping is required
#' @import rgeos rgdal raster sp
#' @export


create_Points=function(InputFile,OutputName,Buffer=0,Separate=0,Clip=0){
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
  
  if (Buffer==0){
    Group=NULL
    for (i in (1:length(ListIDs))){ #Loop over Lines
      
      PID=ListIDs[i]
      PLon=Lons[IDs==PID]
      PLat=Lats[IDs==PID]
      LLon=PLon
      LLat=PLat
      
      if ((dim(data)[2])==3){PVal=NULL}
      if ((dim(data)[2])==4){PVal=unique(XtraFields[IDs==PID])
      if(length(PVal)>1){cat(paste("ERROR: There is more than one value in metadata associated with polygon",PID),"\n");break}}
      if ((dim(data)[2])>4){PVal=unique(XtraFields[IDs==PID,])
      if(dim(PVal)[1]>1){cat(paste("ERROR: There is more than one value in metadata associated with polygon",PID),"\n");break}}
      
      #Project Lat/Lon
      PRO=project(cbind(PLon,PLat),CRSProj)
      PLon=PRO[,1]
      PLat=PRO[,2]
      rm(PRO)
      
      #Create individual Points
      SPls=SpatialPoints(cbind(PLon,PLat))
      
      if ((dim(data)[2])==3){df=data.frame(name=PID,row.names=PID,Lat=LLat,Lon=LLon)}
      if ((dim(data)[2])==4){df=data.frame(name=PID,row.names=PID,PVal,Lat=LLat,Lon=LLon);colnames(df)[2]=names(data)[4]}
      if ((dim(data)[2])>4){df=data.frame(name=PID,row.names=PID,PVal,Lat=LLat,Lon=LLon)}
      SPDF=SpatialPointsDataFrame(SPls, df)
      proj4string(SPDF)=CRS(CRSProj)
      if (Separate==1){writeOGR(SPDF,".",paste(OutputName,"_",as.character(PID),sep=""),driver="ESRI Shapefile")}
      #Add each Line to the Group
      Group=rbind(Group,c(PLon,PLat))
      GroupData=rbind(GroupData,df)
      
    }
    Group=SpatialPoints(Group)
    Group=SpatialPointsDataFrame(Group,GroupData)
    proj4string(Group)=CRS(CRSProj)
    if (Separate==1){writeOGR(Group,".",paste(OutputName,"_Group",sep=""),driver="ESRI Shapefile")}
    else{writeOGR(Group,".",OutputName,driver="ESRI Shapefile")}
  }
  else #Add BUFFER
  {for (i in (1:length(ListIDs))){ #Loop over Lines
    
    PID=ListIDs[i]
    PLon=Lons[IDs==PID]
    PLat=Lats[IDs==PID]
    LLon=PLon
    LLat=PLat
    
    if ((dim(data)[2])==3){PVal=NULL}
    if ((dim(data)[2])==4){PVal=unique(XtraFields[IDs==PID])
    if(length(PVal)>1){cat(paste("ERROR: There is more than one value in metadata associated with polygon",PID),"\n");break}}
    if ((dim(data)[2])>4){PVal=unique(XtraFields[IDs==PID,])
    if(dim(PVal)[1]>1){cat(paste("ERROR: There is more than one value in metadata associated with polygon",PID),"\n");break}}
    
    #Project Lat/Lon
    PRO=project(cbind(PLon,PLat),CRSProj)
    PLon=PRO[,1]
    PLat=PRO[,2]
    rm(PRO)
    
    #Create individual Points
    SPls=SpatialPoints(cbind(PLon,PLat))
    
    Buffered=gBuffer(SPls,width=Buffer)
    PLon=Buffered@polygons[[1]]@Polygons[[1]]@coords[,1]
    PLat=Buffered@polygons[[1]]@Polygons[[1]]@coords[,2]
    rm(Buffered)
    
    #Clip or not
    Pl=Polygon(cbind(PLon,PLat))
    if (Clip==0){
      Pls=Polygons(list(Pl), ID=PID)
    }else{
      cat(paste("Start clipping polygon",PID),"\n")
      # need to fix when Coastline data on the online GIS has been clarified 
      Pls=Clip2Coast(Pl,Coastline_data,ID=PID)
      cat(paste("End clipping polygon",PID),"\n") 
    }
    
    SPls=SpatialPolygons(list(Pls))
    PArea=round(gArea(SPls, byid=F)/1000000)
    if ((dim(data)[2])==3){df=data.frame(name=PID,row.names=PID,AreaKm2=PArea,Lat=LLat,Lon=LLon)}
    if ((dim(data)[2])==4){df=data.frame(name=PID,row.names=PID,PVal,AreaKm2=PArea,Lat=LLat,Lon=LLon);colnames(df)[2]=names(data)[4]}
    if ((dim(data)[2])>4){df=data.frame(name=PID,row.names=PID,PVal,AreaKm2=PArea,Lat=LLat,Lon=LLon)}
    SPDF=SpatialPolygonsDataFrame(SPls, df)
    proj4string(SPDF)=CRS(CRSProj)
    if (Separate==1){writeOGR(SPDF,".",paste(OutputName,"_Buffered_",as.character(PID),sep=""),driver="ESRI Shapefile")}
    #Add each polygon to the Group
    Group[i]=Pls
    GroupData=rbind(GroupData,df)
    
  }
    Group=SpatialPolygons(Group)
    Group=SpatialPolygonsDataFrame(Group,GroupData)
    proj4string(Group)=CRS(CRSProj)
    if (Separate==1){writeOGR(Group,".",paste(OutputName,"_Buffered_Group",sep=""),driver="ESRI Shapefile")}
    else{writeOGR(Group,".",paste(OutputName,"_Buffered",sep=""),driver="ESRI Shapefile")}
  }
  
}