# Zadanie 1
#1. Napisz funkcję sprawdzająca czy 1 liczba jest podzielna przez druga użyj - %%

dividing <- function(x,y) {
  if(x%%y!=0){
    print("niepodzielna")
  }
  else{
    print("podzielna")
  }
}
dividing(100,11)


#Zadanie 2
#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.
#Drugą połowę przejechał ze średnią prędkością 90 km/h.
#Jaka była średnia prędkość pociągu.

predkosc_1 <- 120
predkosc_2 <- 90

czesc_1 <- 0.5
czesc_2 <- 0.5

sredniaPredkosc <- function(i,j,k,l){
  srednia_prekosc = (predkosc_1*czesc_1+predkosc_2*czesc_2)/(czesc_1+czesc_2)
  print(srednia_prekosc)
}

sredniaPredkosc(predkosc_1,predkosc_2,czesc_1,czesc_2)


#Zadanie 3
#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
#Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.

data <- read.csv("C:\\Users\\Tomek\\Desktop\\Uczelnia\\PJWSTK\\R\\cwiczeniar11c\\prace domowe\\dane.csv",sep=";", header=TRUE)
head(data)

cor(data$waga, data$wzrost)



#Zadanie 4
#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika 
#stworzDataFrame <- function(ile=1)
#W pierwszym wierszu użytkownik podaje nazwy kolumn. w kolejnych wierszach zawartość wierszy ramki danych 
#( tyle wierszy ile podaliśmy w argumencie ile. ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości 
#jako parametr, domyślna wartością będzie 1)


create_df <- function(ile = 1, cols, rows = list()){
  
  if (class(rows) != "list")
    stop("Zmienna 'rows' musi być lista", call. = TRUE)
  
  
  if (length(cols) != unique(sapply(rows, length)) | ile != length(rows) )
    stop("Zmienne sa roznej dlugosci!", call. = TRUE)
  
  
  my_dt <- data.table()
  my_dt <- rbind(my_dt,do.call(rbind,rows))
  my_dt <- as.data.frame(my_dt)
  colnames(my_dt) <- cols
  
  
  return(my_dt)
}

create_df(ile =3, cols = c('imie', 'wiek','xyz'),rows = list(c('Ja', 30, "x"), c('ty', 15, "y"), c('on', 15,"z")))



#5 Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy: 
#mean, median,min,max w zależności od podanej nazwy funkcji w argumencie, z katologu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny.

install.packages("data.table")
install.packages("tidyverse")

library("data.table")

library("tidyverse")

options(datatable.fread.input.cmd.message=FALSE)
agregateEnum <- function() {
  list(MAX = 1, MIN = 2, MEDIAN = 3, MEAN = 4)
}
z_5 <- function (sciezkaKatalogu, nazwaKolumny, dlaIluPlikow, jakaFunkcja){
  fileList = list.files(sciezkaKatalogu)
  N <- min(length(fileList),dlaIluPlikow)
  for (i in 1:N){
    fname <-paste(sciezkaKatalogu, fileList[[i]], sep = '\\')
    if (i==1){
      f<- fread(fname, select=nazwaKolumny )
    }else{
      f <- bind_rows(f, fread(fname, select=nazwaKolumny ))
    }
    
  }
  na.strings <- c("","NA")
  f <- na.omit(f)
  retval <- switch(jakaFunkcja,lapply(f,mean,na.rm=TRUE),lapply(f,median,na.rm=TRUE),lapply(f,min,na.rm=TRUE),lapply(f,max,na.rm=TRUE))
  
  return (retval)
}

z_5("C:/Users/Tomek/Desktop/Uczelnia/PJWSTK/R/cwiczeniar11c/smogKrakow/", "3_pm10", 12,1)
