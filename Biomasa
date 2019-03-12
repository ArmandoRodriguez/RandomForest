# Calculo de Biomasa
######################################################################################
# Armando Rodriguez Montellano
# armando.rodriguez.montellano@gmail.com
######################################################################################
#Cargar librerias
require(maptools)
require(sp)
require(randomForest)
require(raster)
require(rgdal)
#
#############################   SET VARIABLES HERE  ##################################
pointData <- 'F:\\Biomasa\\biomasa_calibracion.csv'
inImage <-'F:\\Biomasa\\CompoVAR3.tif'
outImage <- 'F:\\Biomasa\\BiomasaTest9.tif'
nd <- -1
######################################################################################
#
print("Definicion de variables")
startTime <- Sys.time()
cat("Hora de inicio de corrida del modelo", format(startTime),"\n")

pointTable <- read.csv(pointData, header=TRUE)
xy <- SpatialPoints(pointTable[,3:4])
response <- as.numeric(pointTable[,10])

satImage <- stack(inImage)
for (b in 1:nlayers(satImage)) { NAvalue(satImage@layers[[b]]) <- nd }

print("Obteniendo los valores de píxel para cada punto")
trainvals <- cbind(response, extract(satImage, xy)) 
trainvals_no_na <- na.omit(trainvals)

print("Iniciando la construccion de los Arboles")
randfor <- randomForest(response ~. , data=trainvals_no_na)
randfor2 <- randomForest(response ~ ., data=trainvals_no_na, ntree=1000, keep.forest=FALSE, importance=TRUE, type=1, scale=FALSE)
print("Calculando el mapa raster de Biomasa")
pre <-predict(satImage, randfor, filename=outImage, progress='text', format='GTiff', datatype='FLT4S', type='response', overwrite=TRUE)
#
timeDiff <- Sys.time() - startTime
cat("Tiempo de corrida", format(timeDiff), "\n")
varImpPlot(randfor)

unc <- clusterR(trainvals, randfor, args=list(model=model,what=sd))


#MDSplot(randfor, pointTable$response)

hist(treesize(randfor))
