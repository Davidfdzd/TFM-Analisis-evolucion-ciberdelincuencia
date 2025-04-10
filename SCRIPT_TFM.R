# AQUÍ MUESTRO EL SCRIPT CON EL CÓDIGO USADO EN EL PROYECTO. ES EL MISMO QUE EL EMPLEADO EN EL RMARKDOWN.
# CABE MENCIONAR QUE HAY ALGUNOS RESULTADOS QUE AQUÍ NO SE MUESTRAN IGUAL QUE EN EL PROYECTO, COMO LOS BIPLOTS DEL FINAL
# DEBIDO A QUE AL SACARLOS EN CÓDIGO NO SE APRECIAN BIEN YA QUE SE SALEN DE LOS MARGENES Y HAN SIDO EDITADOS
# EN POWERPOINT

# Creación tabla de porcentaje que representan los ciberdelitos con respecto al total de delitos en España.*** Datos tomados del SEC (elaboración propia)

library(knitr)
Tabla1 = data.frame(
  Año = "%",
  `2015` = "4,1%",
  `2016` = "4,60%",
  `2017` = "5,70%",
  `2018` = "7,50%",
  `2019` = "9,90%",
  `2020` = "16,30%",
  `2021` = "16,60%",
  `2022` = "16,16%"
)
colnames(Tabla1) <- c("**Año**", "**2015**", "**2016**", "**2017**", "**2018**", "**2019**", "**2020**", "**2021**", "**2022**")
kable(Tabla1, caption = "***Porcentaje que representan los ciberdelitos con respecto al total de delitos en España.*** Datos tomados del SEC (elaboración propia)", align = c("c", "c", "c", "c", "c", "c", "c", "c", "c"))

# Creación tabla de grupos penales

Tabla2 = data.frame(
  Variable = c("$X_1$", "$X_2$", "$X_3$", "$X_4$", "$X_5$", "$X_6$", "$X_7$", "$X_8$"),
  Grupo_Penal = c("Acceso e Interpretación Ilícita", "Amenazas y Coacciones", "Contra el Honor", "Contra la Propiedad Industrial/Intelectual", "Delitos Sexuales", "Falsificación INformática", "Fraude Informático", "Interferencias en los Datos y en el Sistema"),
  Código = c("AII", "AYC", "HON", "CPI/I", "SEX", "FALIN", "FRAIN", "IDS")
)
kable(Tabla2, caption = "***Grupos Penales***", align = c("c", "c", "c"))

# Creación tabla de tipologías penales

Tabla3 = data.frame(
  Variable = c("$X_1$", "$X_2$", "$X_3$", "$X_4$", "$X_5$", "$X_6$", "$X_7$", "$X_8$", "$X_9$", "$X_10$", "$X_11$", "$X_12$", "$X_13$", "$X_14$", "$X_15$", "$X_16$", "$X_17$", "$X_18$", "$X_19$", "$X_20$", "$X_21$", "$X_22$", "$X_23$", "$X_24$", "$X_25$"),
  Grupo_Penal = c("Abuso Sexual", "Acceso Ilegal Informático", "Acoso Sexual", "Amenazas", "Amenazas a Grupos Étnico Cultutal o Religioso", "Ataques Informáticos", "Calumnias", "Coacciones", "Contra la Propiedad Industrial", "Contra la Propiedad Intelectual", "Corrupción de Menores/con Discapacidad/Diversidad Funcional", "Daños", "Delito de Contacto MEdiante Tecnologías con Menor de 16 años con Fines Sexuales", "Descubrimiento/Revelación de Secretos", "Estafa Bancaria", "Estafas con Tarjetas de Crédito, Débito y Cheques de Viaje", "Estafas Informáticas", "Exhibicionismo", "Fabricación de Moneda, Sellos y Efectos Timbrados", "Injurias", "Otras Estafas", "Otros Relativos al MErcado/Consumidores", "Pornografía de Menores", "Provocación Sexual", "Usurpación de Estado Civil"),
  Código = c("ABSEX", "AIINF", "ACSEX", "AME", "AGECTR", "ATQINF", "CALUM", "COAC", "PINDUS", "PINTEL", "CMD/D", "DAÑOS", "TEC16SEX", "DES/REVSEC", "ESTBAN", "ESTTARYCHEQ", "ESTINF", "EXHI", "FDSET", "INJURIAS", "OTRASEST", "ORM/C", "PORME", "PROSEX", "USUESTCIV")
)
kable(Tabla3, caption = "***Tipologías Penales***", align = c("c", "c", "c"))

# Tratamiento de los datos para realización de descriptiva

library(readxl)
datos=read_excel('Grupos penales.xls')
AIIo <- read_excel("Grupos penales.xls", 
                   sheet = "AII")

AYCo <- read_excel("Grupos penales.xls", 
                   sheet = "AYC")

SEXo <- read_excel("Grupos penales.xls", 
                   sheet = "SEX")

HONo <- read_excel("Grupos penales.xls", 
                   sheet = "HON")

CPIo <- read_excel("Grupos penales.xls", 
                   sheet = "CPI")

FALINo <- read_excel("Grupos penales.xls", 
                     sheet = "FALIN")

FRAINo <- read_excel("Grupos penales.xls", 
                     sheet = "FRAIN")

IDSo <- read_excel("Grupos penales.xls", 
                   sheet = "IDS")
library(ggplot2)
library(gridExtra)
AII=AIIo[,2:9]
AII_reversed <- AII[, rev(seq_len(ncol(AII)))]
AII_reversed = as.data.frame(AII_reversed)
rownames(AII_reversed)=AIIo$CCAA

AYC=AYCo[,2:9]
AYC_reversed <- AYC[, rev(seq_len(ncol(AYC)))]
AYC_reversed = as.data.frame(AYC_reversed)
rownames(AYC_reversed)=AYCo$CCAA

SEX=SEXo[,2:9]
SEX_reversed <- SEX[, rev(seq_len(ncol(SEX)))]
SEX_reversed = as.data.frame(SEX_reversed)
rownames(SEX_reversed)=SEXo$CCAA

HON=HONo[,2:9]
HON_reversed <- HON[, rev(seq_len(ncol(HON)))]
HON_reversed = as.data.frame(HON_reversed)
rownames(HON_reversed)=HONo$CCAA

CPI=CPIo[,2:9]
CPI_reversed <- CPI[, rev(seq_len(ncol(CPI)))]
CPI_reversed = as.data.frame(CPI_reversed)
rownames(CPI_reversed)=CPIo$CCAA

FALIN=FALINo[,2:9]
FALIN_reversed <- FALIN[, rev(seq_len(ncol(FALIN)))]
FALIN_reversed = as.data.frame((FALIN_reversed))
rownames(FALIN_reversed)=FALINo$CCAA

FRAIN=FRAINo[,2:9]
FRAIN_reversed <- FRAIN[, rev(seq_len(ncol(FRAIN)))]
FRAIN_reversed = as.data.frame(FRAIN_reversed)
rownames(FRAIN_reversed)=FRAINo$CCAA

IDS=IDSo[,2:9]
IDS_reversed <- IDS[, rev(seq_len(ncol(IDS)))]
IDS_reversed = as.data.frame(IDS_reversed)
rownames(IDS_reversed)=IDSo$CCAA

summary(datos[,3:10])

# Barplots por gruppos penales

barplot(as.matrix(t(AII_reversed)), beside = TRUE,
        main = "Acceso e interpretación ilícita (AII)",
        xlab = "CCAA", ylab = "Nº de hechos conocidos /100.000 hbts.",
        col = rainbow(ncol(AII_reversed)))

barplot(as.matrix(t(AYC_reversed)), beside = TRUE,
        main = "Amenazas y Coacciones (AYC)",
        xlab = "CCAA", ylab = "Nº de hechos conocidos /100.000 hbts.",
        col = rainbow(ncol(AYC_reversed)))

barplot(as.matrix(t(HON_reversed)), beside = TRUE,
        main = "Contra el Honor (HON)",
        xlab = "CCAA", ylab = "Nº de hechos conocidos /100.000 hbts.",
        col = rainbow(ncol(HON_reversed)))

barplot(as.matrix(t(SEX_reversed)), beside = TRUE,
        main = "Sexuales (SEX)",
        xlab = "CCAA", ylab = "Nº de hechos conocidos /100.000 hbts.",
        col = rainbow(ncol(SEX_reversed)))

barplot(as.matrix(t(CPI_reversed)), beside = TRUE,
        main = "Contra la Propiedad Industrial/Intelectual (CPI/I)",
        xlab = "CCAA", ylab = "Nº de hechos conocidos /100.000 hbts.",
        col = rainbow(ncol(CPI_reversed)))

barplot(as.matrix(t(FALIN_reversed)), beside = TRUE,
        main = "Falsificación Informática (FALIN)",
        xlab = "CCAA", ylab = "Nº de hechos conocidos /100.000 hbts.",
        col = rainbow(ncol(FALIN_reversed)))

barplot(as.matrix(t(FRAIN_reversed)), beside = TRUE,
        main = "Fraude Informático (FRAIN)",
        xlab = "CCAA", ylab = "Nº de hechos conocidos /100.000 hbts.",
        col = rainbow(ncol(FRAIN_reversed)))

barplot(as.matrix(t(IDS_reversed)), beside = TRUE,
        main = "Interferencia en los Datos y en el Sistema (IDS)",
        xlab = "CCAA", ylab = "Nº de hechos conocidos /100.000 hbts.",
        col = rainbow(ncol(IDS_reversed)))

# PCA 1

library(ade4)
library(adegraphics)
library(ade4TkGUI)
datos$Año=as.factor(datos$Año)
wit <- withinpca(datos[,3:10], datos$Año, scannf = FALSE, scaling = "partial")
kta <- ktab.within(wit, colnames = rep(c('AND', 'ARA', 'AST', "BAL", 'CANA','CANT', 'CYL','CLM','CAT','VAL','EXTR','GAL','MAD','MUR','NAV','PV','RIO','CEU','MEL'), 8))
kta1 = t(kta)
pta1 = pta(kta1, scann = FALSE)
plot(pta1, plabels.boxes.draw=FALSE)

# Tabla de matriz RV

RV = pta1$RV
rownames(RV) = c("**2015**", "**2016**", "**2017**", "**2018**", "**2019**", "**2020**", "**2021**", "**2022**")
kable(RV, caption = "***Matriz (RV) de coeficientes de correlación vectorial entre los años***")

# Correlaciones

library(corrplot)
corrplot(RV, title = "***Correlaciones vectoriales entre años***", tl.col = "black")

# Barplots PTA 1

barplot(pta1$RV.eig, sub = "Valores Propios Interestructura")

barplot(pta1$tabw,names.arg=pta1$tab.names, sub="pesos")

barplot(pta1$cos2,names.arg=pta1$tab.names, sub="cos2")

barplot(pta1$eig, sub = "Valores Propios Compromiso")

# Compomiso PTA 1 superpuesto

g0=s.arrow( pta1$co,plabels.boxes.draw=FALSE,sub="COMPROMISO",plabels.cex=1,plabels.col = "red")
g1=s.label(pta1$l1,plabels.boxes.draw=FALSE, sub="COMPROMISO",plabels.cex=1,plabels.col = "blue")
g01=superpose(g0,g1,plot=TRUE)

# Trayectorias por años

la1=s.label(pta1$Tli,facets=pta1$TL[, 1], psub.cex=0, labels=pta1$TL[,2], plabels.cex=1.2, plabels.col= "blue", plot= TRUE, plabels.boxes.draw=FALSE)
ar1=s.arrow(pta1$Tco*3, facets=pta1$TC[, 1], psub.cex=1.5, labels=pta1$TC[,2], plot=TRUE, plabels.boxes.draw=FALSE, parrows.length=0.05)
s1=superpose(la1,ar1)
plot(s1)

# Trayectoria por comunidades

la1.1=s.label(pta1$Tli,facets=pta1$TL[, 2], psub.cex=1.5, labels=pta1$TL[,1], plabels.cex=1.2, plabels.col= "blue", plot= TRUE, plabels.boxes.draw=FALSE)
tr1=s.traject(pta1$Tli, facets=pta1$TL[, 2], plabels.cex = 0, psub.cex=0,col="red", plot=TRUE)
s2=superpose(tr1,la1.1)
plot(s2)

# PTA 2

datos_IE=read_excel('conocidos, esclarecidos e IE.xlsx')
wit_IE = withinpca(datos_IE[,3:10], datos$Año, scannf = FALSE, scaling = "partial")
kta_IE <- ktab.within(wit_IE, colnames = rep(c('AND', 'ARA', 'AST', "BAL", 'CANA','CANT', 'CYL','CLM','CAT','VAL','EXTR','GAL','MAD','MUR','NAV','PV','RIO','CEU','MEL'), 8))
kta2 = t(kta_IE)
pta2 = pta(kta2, scann = FALSE)
plot(pta2, plabels.boxes.draw=FALSE)

# Barplots PTA 2

barplot(pta2$RV.eig, sub = "Valores Propios Interestructura del índice de esclarecimiento")

barplot(pta2$tabw,names.arg=pta2$tab.names, sub="Gráfico de los pesos de cada matriz para la formación del compromiso del índice de esclarecimiento")

barplot(pta2$cos2,names.arg=pta2$tab.names, sub="Gráfico de las calidades de representación del índice de esclarecimiento")

barplot(pta2$eig, sub = "Valores Propios Compromiso")

# Compromiso PTA 2 superpuesto 

g2=s.arrow(pta2$co,plabels.boxes.draw=FALSE,sub="COMPROMISO",plabels.cex=1,plabels.col = "red")
g3=s.label(pta2$l1,plabels.boxes.draw=FALSE, sub="COMPROMISO",plabels.cex=1,plabels.col = "blue")
g23=superpose(g2,g3,plot=TRUE)

# Trayectorias por año PTA 2

ar2=s.arrow(pta2$Tco*3, facets=pta2$TC[, 1], psub.cex=1.5, labels=pta2$TC[,2], plot=TRUE, plabels.boxes.draw=FALSE, parrows.length=0.05)
la2=s.label(pta2$Tli,facets=pta2$TL[, 1], psub.cex=0, labels=pta2$TL[,2], plabels.cex=1.2, plabels.col= "blue", plot= TRUE, plabels.boxes.draw=FALSE) 
s3=superpose(la2,ar2)
plot(s3)

# Trayectorias por comunidades PTA 2

la2.2=s.label(pta2$Tli,facets=pta2$TL[, 2], psub.cex=1.5, labels=pta2$TL[,1], plabels.cex=1.2, plabels.col= "blue", plot= TRUE, plabels.boxes.draw=FALSE)
tr2=s.traject(pta2$Tli, facets=pta2$TL[, 2], plabels.cex = 0, psub.cex=0,col="red", plot=TRUE)
s4=superpose(tr2,la2.2)
plot(s4)

# Centrandonos en CLM, gráfico de comisión de las tipologías en las provincias

library(readxl)
datosclm = read_excel("datosclm.xlsx")
datos1=datosclm[2:18]
table.value(scale(datos1), symbol="circle", col=c("grey", "red"))

# Eigenvalues

pca1=dudi.pca(datos1,scale=TRUE,scannf=FALSE, nf = 5)
screeplot(pca1, main= "Valores propios del JK-Biplot de las tasas de hechos conocidos en CLM en 2022", ylab="valor propio", xlim=c(0,7))
pca1$eig

# Calidades de representación por tipologías

library(FactoMineR)
library(factoextra)
library(Factoshiny)
res.PCA = PCA(datos1,graph=FALSE)
fviz_cos2(res.PCA,
          choice = c("var"),
          axes = 1,
          fill = "steelblue",
          color = "steelblue",
          sort.val = c("desc"),
          top = Inf,
          xtickslab.rt = 45)

fviz_cos2(res.PCA,
          choice = c("var"),
          axes = 2,
          fill = "steelblue",
          color = "steelblue",
          sort.val = c("desc"),
          top = Inf,
          xtickslab.rt = 45)

fviz_cos2(res.PCA,
          choice = c("var"),
          axes = 3,
          fill = "steelblue",
          color = "steelblue",
          sort.val = c("desc"),
          top = Inf,
          xtickslab.rt = 45)

# Calidades de representación por provincias

fviz_cos2(res.PCA,
          choice = c("ind"),
          axes = 1,
          fill = "steelblue",
          color = "steelblue",
          sort.val = c("desc"),
          top = Inf,
          xtickslab.rt = 45, xlim=c(0,7))

fviz_cos2(res.PCA,
          choice = c("ind"),
          axes = 2,
          fill = "steelblue",
          color = "steelblue",
          sort.val = c("desc"),
          top = Inf,
          xtickslab.rt = 45, xlim=c(0,7))

fviz_cos2(res.PCA,
          choice = c("ind"),
          axes = 3,
          fill = "steelblue",
          color = "steelblue",
          sort.val = c("desc"),
          top = Inf,
          xtickslab.rt = 45, xlim=c(0,7))

# JK Biplots

biplot(pca1, plot= TRUE,xax=1, yax=2, xlabel= "eje2", rows.labels=datosclm$provincias, posieig="none", plabel.boxes.draw=FALSE, plabel.optim=TRUE)

biplot(pca1, plot= TRUE,xax=1, yax=3, xlabel= "eje2", rows.labels=datosclm$provincias, posieig="none", plabel.boxes.draw=FALSE, plabel.optim=TRUE)

biplot(pca1, plot= TRUE,xax=2, yax=3, xlabel= "eje2", rows.labels=datosclm$provincias, posieig="none", plabel.boxes.draw=FALSE, plabel.optim=TRUE)

        