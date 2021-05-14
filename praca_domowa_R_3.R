library(DBI)
library(RSQLite)
library(RPostgres)
library(rstudioapi)
library(tidyverse)
library(dplyr)

#określanie zmiennych
dbp<-"bazaKonta.sqlite"
con<-dbConnect(SQLite(),dbp)
readToBase("konta.csv",con,"konta",size=1000,)
con<-dbConnect(SQLite(),dbp)
tablename<-"konta"
liczbaWierszyBaza<-dbGetQuery(con, paste0("SELECT COUNT(*) FROM ",tablename,";" ) )
liczbaWierszyPlik<-lengthOfFile("konta.csv",FALSE)

#(---------------------------FUNKCJE--------------------------------)

#_______________funkcją łącząca się z bazą
connectMe<-function(typ=Postgres(),dbname="novmkhcn",host="dumbo.db.elephantsql.com",user="novmkhcn"){
  con<- dbConnect(typ,
                  dbname=dbname,
                  host=host,
                  user=user,
                  password=askForPassword("database password")
  )
}


#_______________Funkcja wczytująca .csv do postkresu online
readToBase<- function(filepath,dbConn,tablename,header=TRUE,size,sep=",",deleteTable=TRUE){
  ap= !deleteTable
  ov= deleteTable
  fileConnection<- file(description = filepath,open = "r")
  data<-read.table(fileConnection,nrows=size,header = header,fill=TRUE,sep=sep)
  columnsNames<-names(data)
  dbWriteTable(conn = dbConn,name=tablename,data,append=ap,overwrite=ov)
  
  repeat{
    if ( nrow(data)==0){
      close(fileConnection)
      dbDisconnect(dbConn)
      break
    }
    
    data<-read.table(fileConnection,nrows=size,col.names=columnsNames,fill=TRUE,sep=sep)
    dbWriteTable(conn = dbConn,name=tablename,data,append=TRUE,overwrite=FALSE)
  }
}


#(---------------------------MAIN-----------------------------------)

# połączenie z bazą i stworzenie tabeli
con<-connectMe()
readToBase("konta.csv",con,"konda",size=1000)

#połączenie z bazą i wyquerowanie wartości
con<-connectMe()
dbGetQuery(con,query)

print(query)


#(---------------------------ZADANIE 1-------------------------------------)

accounts <- read.csv(file = 'konta.csv')
table="konda"
colName<-"LISTONOSZ"
groupName<-"occupation"
valueSort<-"saldo"
num<-7
dataframe=accounts


rankAccount <- function(dataFrame,colName,groupName,valueSort,num){
  x = filter(accounts,eval(parse(text = groupName))==colName)
  z = x[
    with(x, order(-eval(parse(text = valueSort)))),
  ]
  y = head(z,num)
  return(y)
}

rankAccount(dataFrame,colName,groupName,valueSort,num)

#(---------------------------ZADANIE 2-------------------------------------)

accounts <- read.csv(file = 'konta.csv')
table="konda"
colName<-"NAUCZYCIEL"
groupName<-"occupation"
valueSort<-"saldo"
num<-7
accounts <- read.table("konta.csv", header=TRUE, sep =',')
dataFrame<-accounts

rankAccountChunk <- function(filepath, chunkSize,
                             colName, groupName, valueSort, num=10,
                             sep =',' ){
  
  if (num>chunkSize){
    print('num greater than chunkSize, adjusting chunkSize')
    chunkSize <= num*2
  }
  fileConnection <- file(description = filepath, open="r")
  data <- read.table(fileConnection, nrows = chunkSize, 
                     header=TRUE, fill = TRUE, sep=sep)
  columnsNames <- names(data)
  
  retVal <- dataFrame %>% filter(.data[[colName]]==groupName) %>% arrange (desc(.data[[valueSort]])) %>% slice_head(n = num)
  
  data <- retVal
  
  repeat{
    
    dataStep <- read.table(fileConnection, nrows = chunkSize, 
                           col.names = columnsNames, fill = TRUE, sep=sep)
    if (nrow(dataStep)==0){
      close(fileConnection)
      break
    }
    
    data <- bind_rows(data,dataStep)
    retVal <- dataFrame %>% filter(.data[[colName]]==groupName) %>% arrange (desc(.data[[valueSort]])) %>% slice_head(n = num)
    
    data <- retVal
  }
  
  return(data)
}

# test
rankAccountChunk("konta.csv",100,groupName, colName, valueSort, num)


#(---------------------------ZADANIE 3-------------------------------------)

table3="konda"
colName3<-"LISTONOSZ"
groupName3<-"occupation"
valueSort3<-"saldo"
num3<-"6"




rankAccount3 <- function(table3, con,colName3,groupName3,valueSort3,num3){
  
  #SELECT saldo, occupation FROM "public"."konda" WHERE occupation='NAUCZYCIEL' ORDER BY saldo DESC LIMIT 4
  my_query<-paste("SELECT ",valueSort3,", ",groupName3," FROM ",table3, " WHERE ",groupName3,"='",colName3,"' ORDER BY ",valueSort3, " DESC LIMIT ",num3,sep="")
  dbGetQuery(con,my_query)
  
}


con<-connectMe()
rankAccount3(table3, con,colName3,groupName3,valueSort3,num3)



#(------------------------------------------------------------------)
#miscellaneous
srednia<- function(filepath,columnname,header=TRUE,size,sep=","){
  fileConnection<-file(description = filepath,open="r")
  suma<-0
  counter<-0
  data<-read.table(fileConnection,nrows=size,header = header,fill=TRUE,sep=sep)
  columnsNames<-names(data)
  repeat{
    if(nrow(data) == 0){
      break
    }
    data<-na.omit(data)
    suma<- suma + sum(data[[columnname]])
    counter <- counter + nrow(data)
    data<-read.table(fileConnection,nrows=size,col.names = columnsNames,fill=TRUE,sep=sep)
  }
  suma / counter
}

lengthOfFile<- function(filepath,systemLinuxUnix=FALSE){
  #if(.Platform$OS.type == "unix" )systemLinuxUnix=TRUE
  if ( systemLinuxUnix){
    l <- try(system(paste("wc -l",filepath),intern=TRUE))
    l<-strsplit(l,split=" ")
    l<-as.numeric(l[[1]])
    l
  }
  else{
    l<-length(count.fields(filepath))
    l
  }
}


  
  
  


