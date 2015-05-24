Este repositorio contiene:
  1. Un Script llamado "run_analysis.R" el cual descarga un archivo zip de internet,
     lo descomprime y carga las bases necesarias para el proyecto de curso y finalmete genera una base ordenada.
  2. Un libro de codigos en el cual se explican las variables utilizadas.
  
#### MY PROJECT GETTING AND CLEANING DATA ####

## ESTABLECE EL DIRECTORIO DE TRABAJO
setwd("C:/Users/ALEXIS/Documents/ALEXIS/CURSEA/NIVEL 3Getting and Cleaning Data/SEMANA 3")

##RUTA Y DESCARGA
fileurl="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl,"DatosProyect.zip")

####FUNCION PARA CARGAR LOS ARCHIVOS ZIP####

ReadZip=function(Conjunto,FileRuta){
  if(!is.null(Conjunto)){
  ZipConect=unzip("DatosProyect.zip",paste("UCI HAR Dataset",Conjunto,FileRuta,sep="/"),junkpaths = TRUE)
   
  conectar=ZipConect
  archivo=read.table(conectar)
  archivo
  }else { 
    ZipConect=unzip("DatosProyect.zip",paste("UCI HAR Dataset",FileRuta,sep="/"),junkpaths = TRUE)
    conectar=ZipConect
    archivo=read.table(conectar)
    archivo
  }
}

##Carga archivos ID sujetos
TRSubj=ReadZip("train","subject_train.txt");TSSubj=ReadZip("test","subject_test.txt")
##Carga los sets
SetTest_X=ReadZip("test","X_test.txt");SetTest_Y=ReadZip("test","y_test.txt")
SetTrain_X=ReadZip("train","X_train.txt");SetTrain_Y=ReadZip("train","y_train.txt")
##Carga Feactures
feat=ReadZip(Conjunto = NULL,"features.txt")
##Carga Actividades
Act=ReadZip(Conjunto = NULL,"activity_labels.txt")


### 1. Merges the training and the test sets to create one data set.
Data_Set_X=rbind(SetTest_X,SetTrain_X)                ## Union de las bases con observaciones
Data_Set_Y=rbind(SetTest_Y,SetTrain_Y)                ## Union de bases con etiquetas correspondientes a actividades
Data_Set_Subj=rbind(TSSubj,TRSubj)                    ## Union de bases con el ID de los sujetos


### 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_std_features=grep("-(mean|std)\\(\\)", feat[, 2]) ##Identificar las columnas que son promedio o desviacion 
Data_Set_X=Data_Set_X[, mean_std_features]             ## Filtras las columnas identficadas anteriormente
names(Data_Set_X) <- feat[mean_std_features, 2]        ##Especificar los nombres de las columnas identificadas


### 3.Uses descriptive activity names to name the activities in the data set
Data_Set_Y[, 1] = Act[Data_Set_Y[, 1], 2]             ## Obtiene la variable que contiene el nombre de las actividades       


### 4. Appropriately labels the data set with descriptive variable names.
names(Data_Set_Y)=c("Activities")
names(Data_Set_Subj)=c("ID_Subject")


### 5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Data_Final=cbind(Data_Set_Subj,Data_Set_Y,Data_Set_X)
Data_Promedio = aggregate(.~ID_Subject+Activities,data=Data_Final,mean)                 ##Agrupa la data por el promedio
Data_Promedio=Data_Promedio[order(Data_Promedio$ID_Subject,Data_Promedio$Activities),]  ##Ordena la data por sujeto y actividad

write.table(Data_Promedio,"Data_Promedio.txt",row.names = FALSE)     ## Imprime la Data Ordenada
  
  
     
