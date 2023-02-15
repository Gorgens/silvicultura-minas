require(raster)
require(terra)
require(dplyr)
install.packages('corrplot')
require(corrplot)
library("RColorBrewer")  #http://sthda.com/english/wiki/colors-in-r

## -------------------------- Cria PUC para MG ----------------------------------------
puc1 = raster('pucClass-0000000000-0000000000.tif')
m <- cbind(from = c(-Inf, 1.8, 2.6, 3.4, 4.2), 
           to = c(1.8, 2.6, 3.4, 4.2, Inf), 
           becomes = c(1, 2, 3, 4, 5))
puc1class <- reclassify(puc1, m)
rm(puc1)

puc2 = raster('pucClass-0000000000-0000023296.tif')
m <- cbind(from = c(-Inf, 1.8, 2.6, 3.4, 4.2), 
           to = c(1.8, 2.6, 3.4, 4.2, Inf), 
           becomes = c(1, 2, 3, 4, 5))
puc2class <- reclassify(puc2, m)
rm(puc2)

puc3 = raster('pucClass-0000023296-0000000000.tif')
m <- cbind(from = c(-Inf, 1.8, 2.6, 3.4, 4.2), 
           to = c(1.8, 2.6, 3.4, 4.2, Inf), 
           becomes = c(1, 2, 3, 4, 5))
puc3class <- reclassify(puc3, m)
rm(puc3)

puc4 = raster('pucClass-0000023296-0000023296.tif')
m = cbind(from = c(-Inf, 1.8, 2.6, 3.4, 4.2), 
          to = c(1.8, 2.6, 3.4, 4.2, Inf), 
          becomes = c(1, 2, 3, 4, 5))
puc4class = reclassify(puc4, m)
rm(puc4)

pucClass = merge(puc1class, puc2class)
pucClass = merge(pucClass, puc3class)
pucClass = merge(pucClass, puc4class)
rm(puc1class, puc2class, puc3class, puc4class)

#pucClass = raster('pucClass.tif')
plot(pucClass, col = brewer.pal(n = 5, name = "RdBu"))

## -------------------------- Importa mapbiomas e redimensiona para criar Stack --------------------------
mapbiomas21 = raster('mapbiomas-brazil-collection-70-minasgerais-2021.tif')
mapbiomas85 = raster('mapbiomas-brazil-collection-70-minasgerais-1985.tif')

e1 = c(extent(mapbiomas21)[1], extent(mapbiomas85)[1], extent(pucClass)[1])
e2 = c(extent(mapbiomas21)[2], extent(mapbiomas85)[2], extent(pucClass)[2])
e3 = c(extent(mapbiomas21)[3], extent(mapbiomas85)[3], extent(pucClass)[3])
e4 = c(extent(mapbiomas21)[4], extent(mapbiomas85)[4], extent(pucClass)[4])

mapbiomas21 = crop(mapbiomas21, 
                   extent(max(e1), min(e2), max(e3), min(e4)))
proj4string(mapbiomas21) <- CRS("+init=epsg:4326")
writeRaster(mapbiomas21, 'mapbiomas21.tif')

mapbiomas85 = crop(mapbiomas85, 
                   extent(max(e1), min(e2), max(e3), min(e4)))
proj4string(mapbiomas85) <- CRS("+init=epsg:4326")
mapbiomas85 = raster(vals=values(mapbiomas85),
                  ext=extent(mapbiomas21),
                  crs=crs(mapbiomas21),
                  nrows=dim(mapbiomas21)[1],
                  ncols=dim(mapbiomas21)[2])
writeRaster(mapbiomas85, 'mapbiomas85.tif')

pucClass = crop(pucClass, 
                extent(max(e1), min(e2), max(e3), min(e4)))
pucClass = raster(vals=values(pucClass),
                  ext=extent(mapbiomas21),
                  crs=crs(mapbiomas21),
                  nrows=dim(mapbiomas21)[1],
                  ncols=dim(mapbiomas21)[2])
proj4string(pucClass) <- CRS("+init=epsg:4326")
writeRaster(pucClass, 'pucClass.tif')

## -------------------------- Calcula expansão e perda de silvicultura ---------------
expSilvi = overlay(mapbiomas21, mapbiomas85, fun=function(x,y){return(ifelse(x == 9 & y != 9, 1, 0))})
proj4string(expSilvi) <- CRS("+init=epsg:4326")
plot(expSilvi)
writeRaster(expSilvi, 'expSilvi.tif')

perdaSilvi = overlay(mapbiomas21, mapbiomas85, fun=function(x,y){return(ifelse(x != 9 & y == 9, 1, 0))})
proj4string(perdaSilvi) <- CRS("+init=epsg:4326")
plot(perdaSilvi)
writeRaster(perdaSilvi, 'perdaSilvi.tif')

## -------------------------- Abre as camadas já criadas ---------------
mapbiomas85 = raster('mapbiomas85.tif')
mapbiomas21 = raster('mapbiomas21.tif')
pucClass = raster('pucClass.tif')
expSilvi = raster('expSilvi.tif')
perdaSilvi = raster('perdaSilvi.tif')

## -------------------------- Cria Stack ------------------------------------
mystack = stack(mapbiomas85, mapbiomas21, pucClass, expSilvi, perdaSilvi)
names(mystack) = c("mapbiomas85", "mapbiomas21", "pucClass", "expSilvi", "perdaSilvi")

## -------------------------- Calcula transição 85/21 ------------------
transicao = crosstab(mystack$mapbiomas21, mystack$mapbiomas85)
uso85_21 = data.frame(floresta = rowSums(transicao[ , c("3", "4", "11", "12", "13", "29")]), 
                             pastagem = rowSums(transicao[ , c("15", "21")]), 
                             agricultura = rowSums(transicao[ , c("20", "41", "46", "39", "47", "48")]),
                             silvicultura = transicao[ , c("9")], 
                             agua = transicao[ , c("33")], 
                             outros = rowSums(transicao[ , c("23", "24", "25", "30")]))

uso85_21 = rbind(uso85_21[c("3") , ] + uso85_21[c("4") , ] + uso85_21[c("11") , ] + uso85_21[c("12") , ] + uso85_21[c("13") , ] + uso85_21[c("29") , ],
                         uso85_21[c("15") , ] + uso85_21[c("21") , ],
                         uso85_21[c("20") , ] + uso85_21[c("46") , ] + uso85_21[c("41") , ] + uso85_21[c("39") , ] + uso85_21[c("47") , ] + uso85_21[c("48") , ],
                         uso85_21[c("9") , ],
                         uso85_21[c("33") , ],
                         uso85_21[c("24") , ] + uso85_21[c("25") , ] + uso85_21[c("30") , ] + uso85_21[c("23") , ])
rownames(uso85_21) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")
write.csv(uso85_21, 'transicao85_21.csv')

chisq_transicao85_21 = chisq.test(uso85_21)
corrplot(chisq_transicao85_21$residuals, is.cor = FALSE)

## -------------------------- Analisa cada Mesorregião do estado de MG ------------------
meso = shapefile('ide_1102_mg_mesorregioes_ibge_pol.shp')
proj4string(meso) <- CRS("+init=epsg:4326")
unique(meso$nm_meso)
# 'CAMPO DAS VERTENTES''CENTRAL MINEIRA''JEQUITINHONHA''METROPOLITANA DE BELO HORIZONTE''NOROESTE DE MINAS''NORTE DE MINAS''OESTE DE MINAS''SUL/SUDOESTE DE MINAS''TRI\xc2NGULO MINEIRO/ALTO PARANA\xcdBA''VALE DO MUCURI''VALE DO RIO DOCE''ZONA DA MATA'

central = subset(meso, nm_meso == 'CENTRAL MINEIRA')
stack.meso = crop(mystack, central)   
stack.meso = mask(stack.meso, central)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
centro_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(centro_expPuc, 'centro_expPuc.csv')

temp = crosstab(stack.meso$perdaSilvi, stack.meso$pucClass)
centro_perdaPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(centro_perdaPuc, 'centro_perdaPuc.csv')

centro_transicao = crosstab(stack.meso$mapbiomas21, stack.meso$mapbiomas85)
temp = data.frame(floresta = rowSums(centro_transicao[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(centro_transicao[ , c("15", "21")]), 
                             agricultura = rowSums(centro_transicao[ , c("20", "39", "46", "47", "48")]),
                             silvicultura = centro_transicao[ , c("9")], 
                             agua = centro_transicao[ , c("33")], 
                             outros = rowSums(centro_transicao[ , c("24", "25", "30")]))

temp = rbind(temp[c("3") , ] + temp[c("4") , ] + temp[c("11") , ] + temp[c("12") , ] + temp[c("29") , ],
                         temp[c("15") , ] + temp[c("21") , ],
                         temp[c("20") , ] + temp[c("41") , ] + temp[c("46") , ] + temp[c("39") , ] + temp[c("47") , ] + temp[c("48") , ],
                         temp[c("9") , ],
                         temp[c("33") , ],
                         temp[c("24") , ] + temp[c("25") , ] + temp[c("30") , ])
rownames(temp) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")
centro_transicao = temp
write.csv(centro_transicao, 'centro_transicao.csv')

vertentes = subset(meso, nm_meso == 'CAMPO DAS VERTENTES')
stack.meso = crop(mystack, vertentes)   
stack.meso = mask(stack.meso, vertentes)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
vertentes_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(vertentes_expPuc, 'vertentes_expPuc.csv')

temp = crosstab(stack.meso$perdaSilvi, stack.meso$pucClass)
vertentes_perdaPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(vertentes_perdaPuc, 'vertentes_perdaPuc.csv')

# Calcula matriz de transição para vertentes
vertentes_transicao = crosstab(stack.meso$mapbiomas21, stack.meso$mapbiomas85)
# Agrupa as colunas pelos tipos de uso
temp = data.frame(floresta = rowSums(vertentes_transicao[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(vertentes_transicao[ , c("15", "21")]), 
                             agricultura = rowSums(vertentes_transicao[ , c("20", "41", "46", "47", "48")]),
                             silvicultura = vertentes_transicao[ , c("9")], 
                             agua = vertentes_transicao[ , c("33")], 
                             outros = rowSums(vertentes_transicao[ , c("24", "25", "30")]))
# Agrupa as linhas pelos tipos de uso
temp = rbind(temp[c("3") , ] + temp[c("4") , ] + temp[c("11") , ] + temp[c("12") , ] + temp[c("29") , ],
                         temp[c("15") , ] + temp[c("21") , ],
                         temp[c("41") , ] + temp[c("46") , ] + temp[c("47") , ] + temp[c("48") , ] + temp[c("39") , ],
                         temp[c("9") , ],
                         temp[c("33") , ],
                         temp[c("24") , ] + temp[c("25") , ] + temp[c("30") , ])
rownames(temp) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")
vertentes_transicao = temp
write.csv(vertentes_transicao, 'vertentes_transicao.csv')

jequitinhonha = subset(meso, nm_meso == 'JEQUITINHONHA')
stack.meso = crop(mystack, jequitinhonha)   
stack.meso = mask(stack.meso, jequitinhonha)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
jequitinhonha_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(jequitinhonha_expPuc, 'jequitinhonha_expPuc.csv')

temp = crosstab(stack.meso$perdaSilvi, stack.meso$pucClass)
jequitinhonha_perdaPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(jequitinhonha_perdaPuc, 'jequitinhonha_perdaPuc.csv')

jequitinhonha_transicao = crosstab(stack.meso$mapbiomas21, stack.meso$mapbiomas85)
temp = data.frame(floresta = rowSums(jequitinhonha_transicao[ , c("3", "4", "11", "12", "13", "29")]), 
                             pastagem = rowSums(jequitinhonha_transicao[ , c("15", "21")]), 
                             agricultura = rowSums(jequitinhonha_transicao[ , c("41", "46", "47", "48")]),
                             silvicultura = jequitinhonha_transicao[ , c("9")], 
                             agua = jequitinhonha_transicao[ , c("33")], 
                             outros = rowSums(jequitinhonha_transicao[ , c("24", "25", "30")]))

temp = rbind(temp[c("3") , ] + temp[c("4") , ] + temp[c("11") , ] + temp[c("12") , ] + temp[c("29") , ],
                         temp[c("15") , ] + temp[c("21") , ],
                         temp[c("41") , ] + temp[c("46") , ] + temp[c("47") , ] + temp[c("48") , ] + temp[c("39") , ],
                         temp[c("9") , ],
                         temp[c("33") , ],
                         temp[c("24") , ] + temp[c("25") , ] + temp[c("30") , ])
rownames(temp) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")
jequitinhonha_transicao = temp
write.csv(jequitinhonha_transicao, 'jequitinhonha_transicao.csv')

noroeste = subset(meso, nm_meso == 'NOROESTE DE MINAS')
stack.meso = crop(mystack, noroeste)   
stack.meso = mask(stack.meso, noroeste)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
noroeste_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(noroeste_expPuc, 'noroeste_expPuc.csv')

temp = crosstab(stack.meso$perdaSilvi, stack.meso$pucClass)
noroeste_perdaPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(noroeste_perdaPuc, 'noroeste_perdaPuc.csv')

noroeste_transicao = crosstab(stack.meso$mapbiomas21, stack.meso$mapbiomas85)
temp = data.frame(floresta = rowSums(noroeste_transicao[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(noroeste_transicao[ , c("15", "21")]), 
                             agricultura = rowSums(noroeste_transicao[ , c("39", "41", "46", "47", "48")]),
                             silvicultura = noroeste_transicao[ , c("9")], 
                             agua = noroeste_transicao[ , c("33")], 
                             outros = rowSums(noroeste_transicao[ , c("24", "25", "30")]))

temp = rbind(temp[c("3") , ] + temp[c("4") , ] + temp[c("11") , ] + temp[c("12") , ] + temp[c("29") , ],
                         temp[c("15") , ] + temp[c("21") , ],
                         temp[c("20") , ] + temp[c("39") , ] + temp[c("41") , ] + temp[c("46") , ] + temp[c("47") , ] + temp[c("48") , ],
                         temp[c("9") , ],
                         temp[c("33") , ],
                         temp[c("24") , ] + temp[c("25") , ] + temp[c("30") , ])
rownames(temp) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")
noroeste_transicao = temp
write.csv(noroeste_transicao, 'noroeste_transicao.csv')

norte = subset(meso, nm_meso == 'NORTE DE MINAS')
stack.meso = crop(mystack, norte)   
stack.meso = mask(stack.meso, norte)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
norte_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(norte_expPuc, 'norte_expPuc.csv')

temp = crosstab(stack.meso$perdaSilvi, stack.meso$pucClass)
norte_perdaPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(norte_perdaPuc, 'norte_perdaPuc.csv')

norte_transicao = crosstab(stack.meso$mapbiomas21, stack.meso$mapbiomas85)
temp = data.frame(floresta = rowSums(norte_transicao[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(norte_transicao[ , c("15", "21")]), 
                             agricultura = rowSums(norte_transicao[ , c("20", "39", "41", "46", "47", "48")]),
                             silvicultura = norte_transicao[ , c("9")], 
                             agua = norte_transicao[ , c("33")], 
                             outros = rowSums(norte_transicao[ , c("24", "25", "30")]))

temp = rbind(temp[c("3") , ] + temp[c("4") , ] + temp[c("11") , ] + temp[c("12") , ] + temp[c("29") , ],
                         temp[c("15") , ] + temp[c("21") , ],
                         temp[c("20") , ] + temp[c("39") , ] + temp[c("41") , ] + temp[c("46") , ] + temp[c("47") , ] + temp[c("48") , ],
                         temp[c("9") , ],
                         temp[c("33") , ],
                         temp[c("24") , ] + temp[c("25") , ] + temp[c("30") , ])
rownames(temp) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")
norte_transicao = temp
write.csv(norte_transicao, 'norte_transicao.csv')

oeste = subset(meso, nm_meso == 'OESTE DE MINAS')
stack.meso = crop(mystack, oeste)   
stack.meso = mask(stack.meso, oeste)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
oeste_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(oeste_expPuc, 'oeste_expPuc.csv')

temp = crosstab(stack.meso$perdaSilvi, stack.meso$pucClass)
oeste_perdaPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(oeste_perdaPuc, 'oeste_perdaPuc.csv')

oeste_transicao = crosstab(stack.meso$mapbiomas21, stack.meso$mapbiomas85)
temp = data.frame(floresta = rowSums(oeste_transicao[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(oeste_transicao[ , c("15", "21")]), 
                             agricultura = rowSums(oeste_transicao[ , c("39", "41", "46", "47", "48")]),
                             silvicultura = oeste_transicao[ , c("9")], 
                             agua = oeste_transicao[ , c("33")], 
                             outros = rowSums(oeste_transicao[ , c("24", "25", "30")]))

temp = rbind(temp[c("3") , ] + temp[c("4") , ] + temp[c("11") , ] + temp[c("12") , ] + temp[c("29") , ],
                         temp[c("15") , ] + temp[c("21") , ],
                         temp[c("20") , ] + temp[c("39") , ] + temp[c("41") , ] + temp[c("46") , ] + temp[c("47") , ] + temp[c("48") , ],
                         temp[c("9") , ],
                         temp[c("33") , ],
                         temp[c("24") , ] + temp[c("25") , ] + temp[c("30") , ])
rownames(temp) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")
oeste_transicao = temp
write.csv(oeste_transicao, 'oeste_transicao.csv')

sul = subset(meso, nm_meso == 'SUL/SUDOESTE DE MINAS')
stack.meso = crop(mystack, sul)   
stack.meso = mask(stack.meso, sul)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
sul_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(sul_expPuc, 'sul_expPuc.csv')

temp = crosstab(stack.meso$perdaSilvi, stack.meso$pucClass)
sul_perdaPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(sul_perdaPuc, 'sul_perdaPuc.csv')

sul_transicao = crosstab(stack.meso$mapbiomas21, stack.meso$mapbiomas85)
temp = data.frame(floresta = rowSums(sul_transicao[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(sul_transicao[ , c("15", "21")]), 
                             agricultura = rowSums(sul_transicao[ , c("20", "39", "41", "46", "47", "48")]),
                             silvicultura = sul_transicao[ , c("9")], 
                             agua = sul_transicao[ , c("33")], 
                             outros = rowSums(sul_transicao[ , c("24", "25", "30")]))

temp = rbind(temp[c("3") , ] + temp[c("4") , ] + temp[c("11") , ] + temp[c("12") , ] + temp[c("29") , ],
                         temp[c("15") , ] + temp[c("21") , ],
                         temp[c("20") , ] + temp[c("39") , ] + temp[c("41") , ] + temp[c("46") , ] + temp[c("47") , ] + temp[c("48") , ],
                         temp[c("9") , ],
                         temp[c("33") , ],
                         temp[c("24") , ] + temp[c("25") , ] + temp[c("30") , ])
rownames(temp) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")
sul_transicao = temp
write.csv(sul_transicao, 'sul_transicao.csv')

triangulo = subset(meso, nm_meso == 'TRI\xc2NGULO MINEIRO/ALTO PARANA\xcdBA')
stack.meso = crop(mystack, triangulo)   
stack.meso = mask(stack.meso, triangulo)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
triangulo_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(triangulo_expPuc, 'triangulo_expPuc.csv')

temp = crosstab(stack.meso$perdaSilvi, stack.meso$pucClass)
triangulo_perdaPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(triangulo_perdaPuc, 'triangulo_perdaPuc.csv')

triangulo_transicao = crosstab(stack.meso$mapbiomas21, stack.meso$mapbiomas85)
temp = data.frame(floresta = rowSums(triangulo_transicao[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(triangulo_transicao[ , c("15", "21")]), 
                             agricultura = rowSums(triangulo_transicao[ , c("20", "39", "41", "46", "47", "48")]),
                             silvicultura = triangulo_transicao[ , c("9")], 
                             agua = triangulo_transicao[ , c("33")], 
                             outros = rowSums(triangulo_transicao[ , c("24", "25", "30")]))

temp = rbind(temp[c("3") , ] + temp[c("4") , ] + temp[c("11") , ] + temp[c("12") , ] + temp[c("29") , ],
                         temp[c("15") , ] + temp[c("21") , ],
                         temp[c("20") , ] + temp[c("39") , ] + temp[c("41") , ] + temp[c("46") , ] + temp[c("47") , ] + temp[c("48") , ],
                         temp[c("9") , ],
                         temp[c("33") , ],
                         temp[c("24") , ] + temp[c("25") , ] + temp[c("30") , ])
rownames(temp) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")
triangulo_transicao = temp
write.csv(triangulo_transicao, 'triangulo_transicao.csv')


mucuri = subset(meso, nm_meso == 'VALE DO MUCURI')
stack.meso = crop(mystack, mucuri)   
stack.meso = mask(stack.meso, mucuri)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
mucuri_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(mucuri_expPuc, 'mucuri_expPuc.csv')

temp = crosstab(stack.meso$perdaSilvi, stack.meso$pucClass)
mucuri_perdaPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(mucuri_perdaPuc, 'mucuri_perdaPuc.csv')

mucuri_transicao = crosstab(stack.meso$mapbiomas21, stack.meso$mapbiomas85)
temp = data.frame(floresta = rowSums(mucuri_transicao[ , c("3", "4", "11", "12", "13", "29")]), 
                             pastagem = rowSums(mucuri_transicao[ , c("15", "21")]), 
                             agricultura = rowSums(mucuri_transicao[ , c("41", "46", "48")]),
                             silvicultura = mucuri_transicao[ , c("9")], 
                             agua = mucuri_transicao[ , c("33")], 
                             outros = rowSums(mucuri_transicao[ , c("24", "25")]))

temp = rbind(temp[c("3") , ] + temp[c("4") , ] + temp[c("11") , ] + temp[c("12") , ] + temp[c("13") , ] + temp[c("29") , ],
                         temp[c("15") , ] + temp[c("21") , ],
                         temp[c("20") , ] + temp[c("39") , ] + temp[c("41") , ] + temp[c("46") , ] + temp[c("48") , ],
                         temp[c("9") , ],
                         temp[c("33") , ],
                         temp[c("24") , ] + temp[c("25") , ] + temp[c("30") , ])
rownames(temp) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")
mucuri_transicao = temp
write.csv(mucuri_transicao, 'mucuri_transicao.csv')

doce = subset(meso, nm_meso == 'VALE DO RIO DOCE')
stack.meso = crop(mystack, doce)   
stack.meso = mask(stack.meso, doce)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
doce_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(doce_expPuc, 'doce_expPuc.csv')

temp = crosstab(stack.meso$perdaSilvi, stack.meso$pucClass)
doce_perdaPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(doce_perdaPuc, 'doce_perdaPuc.csv')

doce_transicao = crosstab(stack.meso$mapbiomas21, stack.meso$mapbiomas85)
temp = data.frame(floresta = rowSums(doce_transicao[ , c("3", "4", "11", "12", "13", "29")]), 
                             pastagem = rowSums(doce_transicao[ , c("15", "21")]), 
                             agricultura = rowSums(doce_transicao[ , c("41", "46", "48")]),
                             silvicultura = doce_transicao[ , c("9")], 
                             agua = doce_transicao[ , c("33")], 
                             outros = rowSums(doce_transicao[ , c("24", "25", "30")]))

temp = rbind(temp[c("3") , ] + temp[c("4") , ] + temp[c("11") , ] + temp[c("12") , ] + temp[c("29") , ],
                         temp[c("15") , ] + temp[c("21") , ],
                         temp[c("39") , ] + temp[c("41") , ] + temp[c("46") , ] + temp[c("48") , ],
                         temp[c("9") , ],
                         temp[c("33") , ],
                         temp[c("24") , ] + temp[c("25") , ] + temp[c("30") , ])
rownames(temp) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")
doce_transicao = temp
write.csv(doce_transicao, 'doce_transicao.csv')

mata = subset(meso, nm_meso == 'ZONA DA MATA')
stack.meso = crop(mystack, mata)   
stack.meso = mask(stack.meso, mata)
plot(stack.meso$pucClass)

temp = crosstab(stack.meso$expSilvi, stack.meso$pucClass)
mata_expPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(mata_expPuc, 'mata_expPuc.csv')

temp = crosstab(stack.meso$perdaSilvi, stack.meso$pucClass)
mata_perdaPuc = data.frame(muitoBaixo = temp[ , c("1")],
                             baixo = temp[ , c("2")],
                             medio = temp[ , c("3")],
                             alto = temp[ , c("4")], 
                             muitoAlto = temp[ , c("5")])
write.csv(mata_perdaPuc, 'mata_perdaPuc.csv')

mata_transicao = crosstab(stack.meso$mapbiomas21, stack.meso$mapbiomas85)
temp = data.frame(floresta = rowSums(mata_transicao[ , c("3", "4", "11", "12", "29")]), 
                             pastagem = rowSums(mata_transicao[ , c("15", "21")]), 
                             agricultura = rowSums(mata_transicao[ , c("41", "46", "47", "48")]),
                             silvicultura = mata_transicao[ , c("9")], 
                             agua = mata_transicao[ , c("33")], 
                             outros = rowSums(mata_transicao[ , c("23", "24", "25", "30")]))

temp = rbind(temp[c("3") , ] + temp[c("4") , ] + temp[c("11") , ] + temp[c("12") , ] + temp[c("29") , ],
                         temp[c("15") , ] + temp[c("21") , ],
                         temp[c("39") , ] + temp[c("41") , ] + temp[c("46") , ] + temp[c("48") , ],
                         temp[c("9") , ],
                         temp[c("33") , ],
                         temp[c("23") , ] + temp[c("24") , ] + temp[c("25") , ] + temp[c("30") , ])
rownames(temp) = c("floresta", "pastagem", "agricultura", "silvicultura", "agua", "outros")
mata_transicao = temp
write.csv(mata_transicao, 'mata_transicao.csv')


## -------------------------- Constrói tabelas para de resultados ------------------

# compila resultados da expansão do eucalipto versus potencial de uso
centro_expPuc = read.csv('centro_expPuc.csv')
centro_expPuc = centro_expPuc[2,]
rownames(centro_expPuc) = "centro"

vertentes_expPuc = read.csv('vertentes_expPuc.csv')
vertentes_expPuc = vertentes_expPuc[2,]
rownames(vertentes_expPuc) = "vertentes"

jequitinhonha_expPuc = read.csv('jequitinhonha_expPuc.csv')
jequitinhonha_expPuc = jequitinhonha_expPuc[2,]
rownames(jequitinhonha_expPuc) = "jequitinhonha"

noroeste_expPuc = read.csv('noroeste_expPuc.csv')
noroeste_expPuc = noroeste_expPuc[2,]
rownames(noroeste_expPuc) = "noroeste"

norte_expPuc = read.csv('norte_expPuc.csv')
norte_expPuc = norte_expPuc[2,]
rownames(norte_expPuc) = "norte"

oeste_expPuc = read.csv('oeste_expPuc.csv')
oeste_expPuc = oeste_expPuc[2,]
rownames(oeste_expPuc) = "oeste"

sul_expPuc = read.csv('sul_expPuc.csv')
sul_expPuc = sul_expPuc[2,]
rownames(sul_expPuc) = "sul"

triangulo_expPuc = read.csv('triangulo_expPuc.csv')
triangulo_expPuc = triangulo_expPuc[2,]
rownames(triangulo_expPuc) = "triangulo"

mucuri_expPuc = read.csv('mucuri_expPuc.csv')
mucuri_expPuc = mucuri_expPuc[2,]
rownames(mucuri_expPuc) = "mucuri"

doce_expPuc = read.csv('doce_expPuc.csv')
doce_expPuc = doce_expPuc[2,]
rownames(doce_expPuc) = "doce"

mata_expPuc = read.csv('mata_expPuc.csv')
mata_expPuc = mata_expPuc[2,]
rownames(mata_expPuc) = "mata"

pucExpansao = rbind(mata_expPuc, doce_expPuc, mucuri_expPuc, triangulo_expPuc, sul_expPuc, oeste_expPuc,
     norte_expPuc, noroeste_expPuc, jequitinhonha_expPuc, vertentes_expPuc, centro_expPuc)
pucExpansao = pucExpansao %>% select(-X)
write.csv(pucExpansao, 'pucExpansao.csv')

# compila resultados da perda de eucalipto versus potencial de uso
centro_perdaPuc = read.csv('centro_perdaPuc.csv')
centro_perdaPuc = centro_perdaPuc[2,]
rownames(centro_perdaPuc) = "centro"

vertentes_perdaPuc = read.csv('vertentes_perdaPuc.csv')
vertentes_perdaPuc = vertentes_perdaPuc[2,]
rownames(vertentes_perdaPuc) = "vertentes"

jequitinhonha_perdaPuc = read.csv('jequitinhonha_perdaPuc.csv')
jequitinhonha_perdaPuc = jequitinhonha_perdaPuc[2,]
rownames(jequitinhonha_perdaPuc) = "jequitinhonha"

noroeste_perdaPuc = read.csv('noroeste_perdaPuc.csv')
noroeste_perdaPuc = noroeste_perdaPuc[2,]
rownames(noroeste_perdaPuc) = "noroeste"

norte_perdaPuc = read.csv('norte_perdaPuc.csv')
norte_perdaPuc = norte_perdaPuc[2,]
rownames(norte_perdaPuc) = "norte"

oeste_perdaPuc = read.csv('oeste_perdaPuc.csv')
oeste_perdaPuc = oeste_perdaPuc[2,]
rownames(oeste_perdaPuc) = "oeste"

sul_perdaPuc = read.csv('sul_perdaPuc.csv')
sul_perdaPuc = sul_perdaPuc[2,]
rownames(sul_perdaPuc) = "sul"

triangulo_perdaPuc = read.csv('triangulo_perdaPuc.csv')
triangulo_perdaPuc = triangulo_perdaPuc[2,]
rownames(triangulo_perdaPuc) = "triangulo"

mucuri_perdaPuc = read.csv('mucuri_perdaPuc.csv')
mucuri_perdaPuc = mucuri_perdaPuc[2,]
rownames(mucuri_perdaPuc) = "mucuri"

doce_perdaPuc = read.csv('doce_perdaPuc.csv')
doce_perdaPuc = doce_perdaPuc[2,]
rownames(doce_perdaPuc) = "doce"

mata_perdaPuc = read.csv('mata_perdaPuc.csv')
mata_perdaPuc = mata_perdaPuc[2,]
rownames(mata_perdaPuc) = "mata"

pucPerda = rbind(mata_perdaPuc, doce_perdaPuc, mucuri_perdaPuc, triangulo_perdaPuc, sul_perdaPuc, oeste_perdaPuc,
     norte_perdaPuc, noroeste_perdaPuc, jequitinhonha_perdaPuc, vertentes_perdaPuc, centro_perdaPuc)
pucPerda = pucPerda %>% select(-X)
write.csv(pucPerda, 'pucPerda.csv')

# Expensão sobre o uso de terra
centro_transicao = read.csv('centro_transicao.csv')
centro_caracterizacao = centro_transicao[4,]
rownames(centro_caracterizacao) = "centro"

mata_transicao = read.csv('mata_transicao.csv')
mata_caracterizacao = mata_transicao[4,]
rownames(mata_caracterizacao) = "mata"

doce_transicao = read.csv('doce_transicao.csv')
doce_caracterizacao = doce_transicao[4,]
rownames(doce_caracterizacao) = "doce"

mucuri_transicao = read.csv('mucuri_transicao.csv')
mucuri_caracterizacao = mucuri_transicao[4,]
rownames(mucuri_caracterizacao) = "mucuri"

triangulo_transicao = read.csv('triangulo_transicao.csv')
triangulo_caracterizacao = triangulo_transicao[4,]
rownames(triangulo_caracterizacao) = "triangulo"

sul_transicao = read.csv('sul_transicao.csv')
sul_caracterizacao = sul_transicao[4,]
rownames(sul_caracterizacao) = "sul"

oeste_transicao = read.csv('oeste_transicao.csv')
oeste_caracterizacao = oeste_transicao[4,]
rownames(oeste_caracterizacao) = "oeste"

norte_transicao = read.csv('norte_transicao.csv')
norte_caracterizacao = norte_transicao[4,]
rownames(norte_caracterizacao) = "norte"

noroeste_transicao = read.csv('noroeste_transicao.csv')
noroeste_caracterizacao = noroeste_transicao[4,]
rownames(noroeste_caracterizacao) = "noroeste"

jequitinhonha_transicao = read.csv('jequitinhonha_transicao.csv')
jequitinhonha_caracterizacao = jequitinhonha_transicao[4,]
rownames(jequitinhonha_caracterizacao) = "jequitinhonha"

vertentes_transicao = read.csv('vertentes_transicao.csv')
vertentes_caracterizacao = vertentes_transicao[4,]
rownames(vertentes_caracterizacao) = "vertentes"

eucaliptoCaracterizaoRegiao = rbind(mata_caracterizacao, doce_caracterizacao, mucuri_caracterizacao, 
                    triangulo_caracterizacao, sul_caracterizacao, oeste_caracterizacao,
                    norte_caracterizacao, noroeste_caracterizacao, jequitinhonha_caracterizacao, 
                    vertentes_caracterizacao, centro_caracterizacao)
eucaliptoCaracterizaoRegiao = eucaliptoCaracterizaoRegiao %>% select(-X)

write.csv(eucaliptoCaracterizaoRegiao, 'eucaliptoCaracterizaoRegiao.csv')


## -------------------------- Teste estatísticos ------------------
# http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
# https://stats.stackexchange.com/questions/147925/chi2-of-multidimensional-data/148174#148174

usoExpansao = read.csv('usoExpansao.csv', row.names = 1) %>% t()
pucExpansao = read.csv('pucExpansao.csv', row.names = 1) %>% t()

layout(matrix(c(1,2), 1, 2, byrow = TRUE))
chisq_usoExpansao = chisq.test(usoExpansao)
corrplot(chisq_usoExpansao$residuals, is.cor = FALSE)
chisq_pucExpansao = chisq.test(pucExpansao)
corrplot(chisq_pucExpansao$residuals, is.cor = FALSE)


