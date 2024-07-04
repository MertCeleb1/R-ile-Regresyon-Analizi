
veridosyasi= "https://archive.ics.uci.edu/ml/machine-learning-databases/00560/SeoulBikeData.csv"
bisikletkiralama <- as.data.frame(read.csv(veridosyasi, header= F, sep=",", dec="."))
bisikletkiralama<-bisikletkiralama[-1,]
colnames(bisikletkiralama)<-c("Tarih",
                              "KiralikBisikletSayisi",
                              "Saat",
                              "Sicaklik(0C)",
                              "Nem(%)",
                              "Ruzgarhizi(m/s)",
                              "Gorus",
                              "Giynoktasisicakligi(0C)",
                              "GunesRadyasyonu(MJ/m2)",
                              "yagis(mm)",
                              "Karyagisi",
                              "Mevsimler",
                              "Tatil",
                              "??slevselGun")
attach(bisikletkiralama)
#string olan verileri veri setimden cikardim 
bisikletkiralama<-bisikletkiralama[,-14]
bisikletkiralama<-bisikletkiralama[,-13]
bisikletkiralama<-bisikletkiralama[,-12]
bisikletkiralama<-bisikletkiralama[,-1]


#veri setimin tamami carakter oldugu icin verileri integera geviridim 
KiralikBisikletSayisi<-as.integer(KiralikBisikletSayisi)
Saat<-as.integer(Saat)
Sicaklik<-as.integer(`Sicaklik(0C)`)
Nem<-as.integer(`Nem(%)`)
Ruzgar<-as.integer(`Ruzgarhizi(m/s)`)
Gorus<-as.integer(Gorus)
ciynoktasi<-as.integer(`Giynoktasisicakligi(0C)`)
GunesRadiasyonu<-as.integer(`GunesRadyasyonu(MJ/m2)`)
Yagis<-as.integer(`yagis(mm)`)
Karyagisi<-as.integer(Karyagisi)
#cevirdigim verileri yeni bir veri setine aktardim
kiralama<-data.frame(KiralikBisikletSayisi,
              Saat,
              Sicaklik,
              Nem,
              Ruzgar,
              Gorus,
              ciynoktasi,
              GunesRadiasyonu,
              Yagis,
              Karyagisi)
attach(kiralama)
#R dilin de noramlik testi en fazla 5000 veri ile yap}ld}g} icin ilk 5000 verimi ald}m.

shapiro.test(KiralikBisikletSayisi[0:5000])
shapiro.test(Saat[0:5000])
shapiro.test(Sicaklik[0:5000])
shapiro.test(Nem[0:5000])
shapiro.test(Ruzgar[0:5000])
shapiro.test(Gorus[0:5000])
shapiro.test(ciynoktasi[0:5000])
shapiro.test(GunesRadiasyonu[0:5000])
shapiro.test(Yagis[0:5000])
shapiro.test(Karyagisi[0:5000])

#Kolmogrov-Smiirnov Testin de S}n}rlama yoktur ama verim cok b|y|k oldugu icin ilk 5000 ad}m}
#almakla yaklas}k olarak ayn} sonucu verecektir.
ks.test(KiralikBisikletSayisi[0:5000],Sicaklik[0:5000])
ks.test(KiralikBisikletSayisi,Sicaklik)
ks.test(KiralikBisikletSayisi[0:5000],Saat[0:5000])
ks.test(KiralikBisikletSayisi,Nem)
ks.test(KiralikBisikletSayisi,Gorus)
ks.test(KiralikBisikletSayisi,Ruzgar)
ks.test(Saat,ciynoktasi)

#Grafiksel olarak yorumlalamk igin 
install.packages("ggplot2") #Paketi y|klemek igin.
library(ggplot2)    #Paketi gap}rmak igin
hist.kiralma=ggplot(kiralama,aes(KiralikBisikletSayisi))+geom_histogram(aes(y=..density..),
                                                                       colour="black",fill="white")+
  labs(x="Kiral}k Bisiklet Say}s}",y="Yopunluk")
hist.kiralma
hist.kiralma+stat_function(fun=dnorm, args= list(mean=mean(KiralikBisikletSayisi),
                                                sd=sd(KiralikBisikletSayisi)), colour="blue",size=0)

p=ggplot(kiralama,aes(sample=KiralikBisikletSayisi))+stat_qq()+stat_qq_line()
p

#Veriler aras}ndaki iliskiyi goruntuluyelim
plot(kiralama, main="KiralikBisikletSayisi")

#Regresyon modelimizi 0lusturup Analizimizi yapalim.
model <-lm(KiralikBisikletSayisi~Gorus+Saat+Sicaklik+Nem+Ruzgar+GunesRadiasyonu+ciynoktasi+Yagis+Karyagisi,data=kiralama)
print(model)
summary(model)

#Modelimizdeki anlams}z verileri g}kart}p tekrardan yeni bir model olusturalim. 
model2 <-lm(KiralikBisikletSayisi~Saat+Sicaklik+Nem+GunesRadiasyonu+Yagis,data=kiralama)
print(model2)
summary(model2)

#Adjusted R-Squared :0.4705 oldugundan bag}ml} degisken bag}ms}z degiskenleri yeterince ac}klamaz. 
#Farkl} modeller olustrdugumda daha dustugu icin modelimle daha fazla oynamad}m. 

#Modlin art}k standart hatas}n} bulmak iginkuland}m.Dusuk art}k hatalar modin veriye iyi uydugunu gvsterir
469.3/mean(KiralikBisikletSayisi) 

#Modelimizin grafigini olusturup yorumlayal}m.
plot(model2)

##ONGORU
ongoru <- predict(model2)
print(ongoru)

#gercek veri ile ongoru verilerini ve artiklari birlikte gorelim
data.frame(KiralikBisikletSayisi=kiralama[,1],ongoru=ongoru, artik=kiralama[,1]-ongoru)

# modele dayali grafik olarak gorelim
plot(kiralama[,2],kiralama[,1],main="Gercek veri ile ongorulerin dagilimi")
lines(KiralamaSayisi=kiralama[,2],ongoru)

#buradan baska grafiklerde elde ettim ama yorumlayamad}m.
plot(KiralikBisikletSayisi,ciynoktasi)
plot(KiralikBisikletSayisi,Gorus)
plot(KiralikBisikletSayisi,GunesRadiasyonu)
plot(KiralikBisikletSayisi,Nem)
plot(KiralikBisikletSayisi,Ruzgar)
plot(KiralikBisikletSayisi,Saat)
plot(KiralikBisikletSayisi,Sicaklik)


