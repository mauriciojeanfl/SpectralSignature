# Instalando as bibliotecas
#install.packages('sf')
#install.packages('raster')
#install.packages('dplyr')
# Carregando as bibliotecas
library(raster)
library(tidyverse)
library(sf)

# Caminho da pasta
pathraster = setwd('C:\\Users\\mauri\\Documents\\GitHub\\AssinaturaEspectral\\SpectralSignature\\Bandas')
pathamostras = 'C:\\Users\\mauri\\Documents\\GitHub\\AssinaturaEspectral\\SpectralSignature\\Amostras'

# Lista com os arquivos
rastlist <- list.files(path = pathraster, pattern="^B", all.files=TRUE, full.names=FALSE)

# Criando stack das bandas
bandas = stack(rastlist)

#----#
# Importando o Shapefile
pontos = st_read(dsn = pathamostras, layer = 'Amostras')

# Plotando a imagem RGB e as amostras
plotRGB(bandas,r=5,g=4,b=3, stretch = "lin")
plot(pontos[,'classe'], add = T)

# Multiplicar por 0.0001 pois para obtenção dos valores de reflectância normalizado (0-1)
bandas = bandas/10000

# Extração dos valores de reflectância
valores = raster::extract(bandas,pontos, df = T)

# Merge com as classes
ValoresReflect = cbind(valores,'Classe' = pontos$classe)

# Obtendo a média por classe de uso do solo
ValoresReflect_mean = ValoresReflect %>% 
  group_by(Classe)  %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>% 
  dplyr::select(5:11,B11, B12, everything())

# Tabela de assinatura espectral
Tabassinatura = pivot_longer(ValoresReflect_mean,cols = names(ValoresReflect_mean)[1:9], names_to = 'Bandas',values_to = 'Reflectancia')

# Tranformando bandas para fator
Tabassinatura$Bandas = factor(Tabassinatura$Bandas, levels = unique(Tabassinatura$Bandas))

# Criando gráfico
ggplot(Tabassinatura,aes(x=Bandas,y=Reflectancia,col=Classe, group = Classe)) + geom_point(aes(shape = Classe), size =2.5)+ geom_line(size = 1.1) + xlab('Bandas') + ylab('Reflectância (%)') + theme_classic() +   scale_colour_manual(name = "Classe",values = c("blue", "green", "brown"))

