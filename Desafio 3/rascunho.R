library(CARBayesdata)
library(sp)
library(dplyr)
library(sf)
library(ggplot2)
library(spdep)
library(spatialreg)
library(rgdal)
library(leaflet)

base <-read.csv2("pneumonia_rj.csv", header = TRUE, encoding="ISO-8859-1", sep = ";", dec = ",")
base$Cod_muni<- as.character(base$Cod_muni)
base = base |> 
  rename(NM_MUN = Munic?pio )
# Carregando o pacote
library(sf)

# Importando o shapefile
RJ = st_read(dsn = "shape/RJ_Municipios_2020.shp")

## Para plotar somente o contorno do objeto recife
ggplot(RJ) +
  geom_sf(fill = "White")

#unindo as bases
RJ = left_join(x = RJ, y = base, by = c("NM_MUN" = "Munic?pio"))
base_rj = merge(x = RJ, 
                y = base, by = "NM_MUN", all.x=FALSE)
# Histogram do pre?o dos imoveis
g1 = ggplot(base)+
  geom_histogram(aes(x =Taxa,
                     y = ..density..),
                 fill="blue",
                 color = "black") +
  labs(x = "Taxa de Mortalidade por Pneumonia",
       y = "Densidade")


# fazendo o mapa do pre?o
RJ <- st_transform(RJ,CRS("+proj=longlat +datum=WGS84 +no_defs"))

colours1 <- colorNumeric(palette = "YlOrRd", domain = RJ$Taxa)
map1 <- leaflet(data=RJ) %>%
  addTiles() %>%
  addPolygons(fillColor = ~colours1(Taxa), color="", weight=1,
              fillOpacity = 0.7) %>%
  addLegend(pal = colours, values = RJ$Taxa, opacity = 2,
            title="Taxa") %>%
  addScaleBar(position="bottomleft") 
map1 


colours <- colorNumeric(palette = "Greens", domain =RJ$Equipamentos_Existentes)
map2 <- leaflet(data=RJ) %>%
  addTiles() %>%
  addPolygons(fillColor = ~colours(Equipamentos_Existentes), color="", weight=1,
              fillOpacity = 0.7) %>%
  addLegend(pal = colours, values = RJ$Equipamentos_Existentes, opacity = 3,
            title="N? de Respiradores",bins = 10) %>%
  addScaleBar(position="bottomleft")
map2


g2 = ggplot(RJ,aes(x = Equipamentos_Existentes , y = Taxa)) + 
  geom_point() +
  labs(x='N?mero de respiradores',y='Taxa de Mortalidade por Pneumonia') +
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5))

ggplot(RJ)+
  geom_sf(aes(fill = Taxa))+
  scale_fill_viridis_c() + 
  labs(fill = "Taxa de\nMortalidade")

ggplot(RJ)+
  geom_sf(aes(fill = Equipamentos_Existentes))+
  scale_fill_viridis_c()+
  labs(fill = "N? de Respiradores")

## Regressao Nao Espacial


modelo <- lm(Taxa~Equipamentos_Existentes, data=RJ)
summary(modelo)
AIC(modelo)

# Matriz de vizinhan?a
W.nb <- poly2nb(RJ, row.names = rownames(RJ))
W.list <- nb2listw(W.nb, style="B")


# Indice de Moran Global
globalMoran <- moran.test(residuals(modelo), W.list)
globalMoran

# Indice de Moral Local (LISA)
localMoran <- localmoran(residuals(modelo), W.list)
localMoran

RJ <- cbind(RJ,LISA = localMoran[,4])
RJ$LISA

ggplot(RJ)+
  geom_sf(aes(fill = LISA))+
  scale_fill_viridis_c()+
  labs(fill = "N? de Respiradores")

## Regressao Espacial

## MODELO CAR


nc_car <- spautolm(Taxa~Equipamentos_Existentes,
                   data = RJ,
                   listw =  W.list,
                   family = "CAR")

summary(nc_car)

RJ['r_car'] <- nc_car$fit$residuals
RJ['r_ind'] <- modelo$residuals

# residuos CAR vs IND
ggplot(RJ,aes(x = r_car , y = r_ind)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col=2, lwd=1.1) +
  labs(x='res?duos CAR',y='res?duos IND', title= "CAR vs IND - res?duos") +
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5))


