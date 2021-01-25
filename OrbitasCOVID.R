
#### Codigo para analizar la cantidad y tendencia de una serie
#### los datos de entrada son fallecimientos de COVID
#### los datos de fallecimientos por millón se pueden descargar de
#### https://ourworldindata.org/coronavirus/country/mexico
require(zoo); require(viridis)
DB <- read.csv("owid-covid-data.csv")
d <- dim(DB)[1] # dias en la base

#### la serie DB contiene por columna los fallecimientos
#### para cada día (filas) para cada país (columna) por millón


#### Figura con los datos de MX
#### Mx aparece en la columna 113
SP <- smooth.spline(DB[, 113], df = 21)
png("MexicoSeries.PNG", width = 1500, height = 600)
par(mar = c(0,0,0,0))
plot(SP, col = NA, ylim = c(0, 8), 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
polygon(c(SP$x, rev(SP$x)),
        c(pmax(0,SP$y), 0*SP$y),
        col = cols[80])
points(X[, 113], type = "l", col = cols[20], lwd = 8)
dev.off()


#### Diagrama XY solo MX
png("DiagramaMexico.PNG", width = 1000, height = 700)
par(mar = c(0,0,0,0))
plot(0, col = NA, xlim = c(2.5, 10.5), 
     ylim = c(-0.03, 0.04))
for(tp in -6:16){points(c(0, 20), tp*0.01*c(1,1), type = "l", col = "gray")}
for(tp in 0:20){points(tp*c(1, 1), c(-.06,.16), type = "l", col = "gray")}
points(c(0,12), c(0,0), type = "l", lwd = 2)
for(k in c(113)){
  colt <- colorRampPalette(rev(inferno(100)))
  SP <- smooth.spline(DB[, k], df = 21)
  speed = (SP$y[2:d]-SP$y[1:(d-1)])/SP$y[1:(d-1)]
  delta = dim(DB)[1] - 120 ## dias para graficar
  for(s in 1:(delta-1)){
    points(SP$y[(120+s):(120+s+1)], 
           speed[(120+s-1):(120+s)], 
           type = "l",
           lwd = s/6+3.5,
           col =colt(delta)[s])
  }
}
dev.off()

#### diagrama para otros países por continente

png("DiagramaPaisesNorteAmerica.PNG", width = 1000, height = 500)
{
  par(mar = c(0,0,0,0))
  plot(0, col = NA, xlim = c(0, 13), 
       ylim = c(-0.03, 0.085))
  for(tp in -6:16){points(c(0, 20), tp*0.01*c(1,1), type = "l", col = "gray")}
  for(tp in 0:20){points(tp*c(1, 1), c(-.06,.16), type = "l", col = "gray")}
  points(c(0,13), c(0,0), type = "l", lwd = 2)
  
  ## USA
  k = 184
  colt <- colorRampPalette(c("blue", "navyblue"))
  {
    SP <- smooth.spline(DB[, k], df = 21)
    speed = (SP$y[2:333]-SP$y[1:332])/SP$y[1:332]
    delta = dim(DB)[1] - 120 ## dias para graficar
    for(s in 1:(delta-1)){
      points(SP$y[(120+s):(120+s+1)], 
             speed[(120+s-1):(120+s)], 
             type = "l",
             lwd = s/6+3.5,
             col =colt(delta)[s])
    }}
  points(SP$y[281], speed[280], pch = 20, col = 1, cex = 4)
  points(SP$y[312], speed[311], pch = 20, col = "white", cex = 6)
  
  ## CANADA
  k = 32
  colt <- colorRampPalette(c("gray43", "gray20"))
  {
    SP <- smooth.spline(DB[, k], df = 21)
    speed = (SP$y[2:333]-SP$y[1:332])/SP$y[1:332]
    delta = dim(DB)[1] - 120 ## dias para graficar
    for(s in 1:(delta-1)){
      points(SP$y[(120+s):(120+s+1)], 
             speed[(120+s-1):(120+s)], 
             type = "l",
             lwd = s/6+3.5,
             col =colt(delta)[s])
    }}
  points(SP$y[281], speed[280], pch = 20, col = 1, cex = 4)
  points(SP$y[312], speed[311], pch = 20, col = "white", cex = 6)
  
  ### MEXICO
  k = 113
  colt <- colorRampPalette(c("red", "red4"))
  {
    SP <- smooth.spline(DB[, k], df = 21)
    speed = (SP$y[2:333]-SP$y[1:332])/SP$y[1:332]
    delta = dim(DB)[1] - 120 ## dias para graficar
    for(s in 1:(delta-1)){
      points(SP$y[(120+s):(120+s+1)], 
             speed[(120+s-1):(120+s)], 
             type = "l",
             lwd = s/6+3.5,
             col =colt(delta)[s])
    }}
  points(SP$y[281], speed[280], pch = 20, col = 1, cex = 4)
  points(SP$y[312], speed[311], pch = 20, col = "white", cex = 6)
}
dev.off()

png("DiagramaPaisesEuropa.PNG", width = 1000, height = 500)
{
  par(mar = c(0,0,0,0))
  plot(0, col = NA, xlim = c(0, 13), 
       ylim = c(-0.03, 0.085))
  for(tp in -6:16){points(c(0, 20), tp*0.01*c(1,1), type = "l", col = "gray")}
  for(tp in 0:20){points(tp*c(1, 1), c(-.06,.16), type = "l", col = "gray")}
  points(c(0,13), c(0,0), type = "l", lwd = 2)
  ### UK
  k = 183
  colt <- colorRampPalette(c("gray", "black"))
  {
    SP <- smooth.spline(DB[, k], df = 21)
    speed = (SP$y[2:333]-SP$y[1:332])/SP$y[1:332]
    delta = dim(DB)[1] - 120 ## dias para graficar
    for(s in 1:(delta-1)){
      points(SP$y[(120+s):(120+s+1)], 
             speed[(120+s-1):(120+s)], 
             type = "l",
             lwd = s/6+3.5,
             col =colt(delta)[s])
    }}
  points(SP$y[281], speed[280], pch = 20, col = 1, cex = 4)
  points(SP$y[312], speed[311], pch = 20, col = "white", cex = 6)
  
  ### FRANCE
  k = 62
  colt <- colorRampPalette(c("mediumblue", "navyblue"))
  {
    SP <- smooth.spline(DB[, k], df = 21)
    speed = (SP$y[2:333]-SP$y[1:332])/SP$y[1:332]
    delta = dim(DB)[1] - 120 ## dias para graficar
    for(s in 1:(delta-1)){
      points(SP$y[(120+s):(120+s+1)], 
             speed[(120+s-1):(120+s)], 
             type = "l",
             lwd = s/6+3.5,
             col =colt(delta)[s])
    }}
  points(SP$y[281], speed[280], pch = 20, col = 1, cex = 4)
  points(SP$y[312], speed[311], pch = 20, col = "white", cex = 6)
  
  ### GERMANY
  k = 66
  colt <- colorRampPalette(c("white", "springgreen", "springgreen4"))
  {
    SP <- smooth.spline(DB[, k], df = 21)
    speed = (SP$y[2:333]-SP$y[1:332])/SP$y[1:332]
    delta = dim(DB)[1] - 120 ## dias para graficar
    for(s in 1:(delta-1)){
      points(SP$y[(120+s):(120+s+1)], 
             speed[(120+s-1):(120+s)], 
             type = "l",
             lwd = s/6+3.5,
             col =colt(delta)[s])
    }}
  points(SP$y[281], speed[280], pch = 20, col = 1, cex = 4)
  points(SP$y[312], speed[311], pch = 20, col = "white", cex = 6)
  
  ### SPAIN
  k = 164
  colt <- colorRampPalette(c("white", "red", "darkred"))
  {
    SP <- smooth.spline(DB[, k], df = 21)
    speed = (SP$y[2:333]-SP$y[1:332])/SP$y[1:332]
    delta = dim(DB)[1] - 120 ## dias para graficar
    for(s in 1:(delta-1)){
      points(SP$y[(120+s):(120+s+1)], 
             speed[(120+s-1):(120+s)], 
             type = "l",
             lwd = s/6+3.5,
             col =colt(delta)[s])
    }}
  points(SP$y[281], speed[280], pch = 20, col = 1, cex = 4)
  points(SP$y[312], speed[311], pch = 20, col = "white", cex = 6)
  
  ### ITALY
  k = 86
  colt <- colorRampPalette(c("orange", "darkorange4"))
  {
    SP <- smooth.spline(DB[, k], df = 21)
    speed = (SP$y[2:333]-SP$y[1:332])/SP$y[1:332]
    delta = dim(DB)[1] - 120 ## dias para graficar
    for(s in 1:(delta-1)){
      points(SP$y[(120+s):(120+s+1)], 
             speed[(120+s-1):(120+s)], 
             type = "l",
             lwd = s/6+3.5,
             col =colt(delta)[s])
    }}
  points(SP$y[281], speed[280], pch = 20, col = 1, cex = 4)
  points(SP$y[312], speed[311], pch = 20, col = "white", cex = 6)
  
  
}
dev.off()


png("DiagramaPaisesSurAmerica.PNG", width = 1000, height = 500)
{
  par(mar = c(0,0,0,0))
  plot(0, col = NA, xlim = c(0, 13), 
       ylim = c(-0.03, 0.085))
  for(tp in -6:16){points(c(0, 20), tp*0.01*c(1,1), type = "l", col = "gray")}
  for(tp in 0:20){points(tp*c(1, 1), c(-.06,.16), type = "l", col = "gray")}
  points(c(0,13), c(0,0), type = "l", lwd = 2)
  
  ### PERU
  k = 137
  colt <- colorRampPalette(c("khaki", "khaki4"))
  {
    SP <- smooth.spline(DB[, k], df = 21)
    speed = (SP$y[2:333]-SP$y[1:332])/SP$y[1:332]
    delta = dim(DB)[1] - 120 ## dias para graficar
    for(s in 1:(delta-1)){
      points(SP$y[(120+s):(120+s+1)], 
             speed[(120+s-1):(120+s)], 
             type = "l",
             lwd = s/6+3.5,
             col =colt(delta)[s])
    }}
  points(SP$y[281], speed[280], pch = 20, col = 1, cex = 4)
  points(SP$y[312], speed[311], pch = 20, col = "white", cex = 6)
  
  ### BRASIL
  k = 25
  colt <- colorRampPalette(c("green2", "green4"))
  {
    SP <- smooth.spline(DB[, k], df = 21)
    speed = (SP$y[2:333]-SP$y[1:332])/SP$y[1:332]
    delta = dim(DB)[1] - 120 ## dias para graficar
    for(s in 1:(delta-1)){
      points(SP$y[(120+s):(120+s+1)], 
             speed[(120+s-1):(120+s)], 
             type = "l",
             lwd = s/6+3.5,
             col =colt(delta)[s])
    }}
  points(SP$y[281], speed[280], pch = 20, col = 1, cex = 4)
  points(SP$y[312], speed[311], pch = 20, col = "white", cex = 6)
  
  ## ARG
  k = 8
  colt <- colorRampPalette(c("skyblue", "skyblue4"))
  {
    SP <- smooth.spline(DB[, k], df = 21)
    speed = (SP$y[2:333]-SP$y[1:332])/SP$y[1:332]
    delta = dim(DB)[1] - 120 ## dias para graficar
    for(s in 1:(delta-1)){
      points(SP$y[(120+s):(120+s+1)], 
             speed[(120+s-1):(120+s)], 
             type = "l",
             lwd = s/6+3.5,
             col =colt(delta)[s])
    }}
  points(SP$y[281], speed[280], pch = 20, col = 1, cex = 4)
  points(SP$y[312], speed[311], pch = 20, col = "white", cex = 6)
  
  ### COLOMBIA
  k = 38
  colt <- colorRampPalette(c("gold", "gold3"))
  {
    SP <- smooth.spline(DB[, k], df = 21)
    speed = (SP$y[2:333]-SP$y[1:332])/SP$y[1:332]
    delta = dim(DB)[1] - 120 ## dias para graficar
    for(s in 1:(delta-1)){
      points(SP$y[(120+s):(120+s+1)], 
             speed[(120+s-1):(120+s)], 
             type = "l",
             lwd = s/6+3.5,
             col =colt(delta)[s])
    }}
  points(SP$y[281], speed[280], pch = 20, col = 1, cex = 4)
  points(SP$y[312], speed[311], pch = 20, col = "white", cex = 6)
}
dev.off()
