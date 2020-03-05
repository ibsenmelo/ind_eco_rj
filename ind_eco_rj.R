
library(xts)
library(BETS)
library(dplyr)
library(tidyverse)
library(zoo)
require(ggthemes)
library(DT)
library(kableExtra)
library(dygraphs)
library(plotly)
library(scales)
#setwd("~/Documents/rdir")
setwd("~/Dropbox/rdir")
# load("/Users/ibsen/Documents/rdir/data.RData")
#load("data.RData")


Brasil <- BETSget(24364) #economic activity index (IBC-Br) dessazonalizado
Rio_de_Janeiro <- BETSget(25397) #economic activity index rj dessazonalizado
sao_paulo <- BETSget(25394) #economic activity index sp dessazonalizado
minas_gerais<- BETSget(25380) #economic activity index mg dessazonalizado
espirito_santo<- BETSget(25399) #economic activity index es dessazonalizado
norte<- BETSget(25407) #economic activity index es dessazonalizado
nordeste<- BETSget(25389) #economic activity index es dessazonalizado
centroeste<- BETSget(25382) #economic activity index es dessazonalizado
sul<- BETSget(25403) #economic activity index es dessazonalizado

SC<- BETSget(25405) #economic activity index es dessazonalizado
parana<- BETSget(25413) #economic activity index es dessazonalizado
RS<- BETSget(25404) #economic activity index es dessazonalizado

GO<- BETSget(25384) #economic activity index es dessazonalizado
DF<- BETSget(25384)
BA<-  BETSget(25416)
CE <- BETSget(25391)
PE <- BETSget(25418)

sudeste <- BETSget(25395)  # regional economic activity index - sudeste

ibc2 <- ts.union(Brasil,Rio_de_Janeiro) # para usar na tabela do resumo

tab_ibc <- cbind(Brasil,Rio_de_Janeiro,sudeste) %>% 
  tail(2)

var_ibc_brasil <- percent((tab_ibc[2,"Brasil"]/tab_ibc[1,"Brasil"]) - 1, accuracy = .01)
var_ibc_sudeste<- percent((tab_ibc[2,"sudeste"]/tab_ibc[1,"sudeste"]) - 1, accuracy = .01)
var_ibc_rj<- percent((tab_ibc[2,"Rio_de_Janeiro"]/tab_ibc[1,"Rio_de_Janeiro"]) - 1, accuracy = .01)


grafico_ibc<-ts.union(Brasil,sudeste,norte,nordeste,centroeste,sul,SC,parana,RS,GO,DF,BA,CE,PE,
         sao_paulo,minas_gerais,espirito_santo, Rio_de_Janeiro) %>% 
  autoplot() %>% 
  ggplotly()
 















# arrecadação de icms do rj - fonte MF-cotepe
primario <- BETSget(7641)# icms primary sector
secundario <- BETSget(7652)# icms secondary sector
terciario <- BETSget(7663)# icms tertiary sector
energia <- BETSget(7674)# icms energy sector
petroleo <- BETSget(7685)# icms petroleum sector
outros <- BETSget(7696)# icms other services





setor_icms <- ts.union(primario,secundario,terciario,energia,petroleo,outros) %>% 
  as.xts() %>% 
  dygraph() %>% 
  dyRangeSelector()

# grafico pizza da arrecadação por setor

setor_icms1 <- ts.union(primario,secundario,terciario,energia,petroleo,outros) %>% 
  tail(1) %>% 
  as.data.frame() %>% 
  gather(setor, Receita, primario:outros) %>% 
  plot_ly(labels = ~setor, values = ~Receita, type = 'pie')
  

 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  






Brasil <- BETSget(1455)  # sales volume index - brasil
rio_de_janeiro <- BETSget(1474)  # sales volume index - RJ


varejo <- ts.union(Brasil,rio_de_janeiro)

grafico_varejo<- varejo %>% 
  as.xts() %>% 
  dygraph() %>% 
  dyRangeSelector()









total <- BETSget(1474) #Sales volume index in the retail sector (2003=100)
combustivel <- BETSget(1489) #Sales volume index in the retail sector (2003=100)
mercado <- BETSget(1502)  # Sales volume index in the retail sector (2003=100) - Hypermarkets, 
                          #supermarkets, food, beverages and tobacco - Rio de Janeiro
textil <- BETSget(1515) #Sales volume index in the retail sector (2003=100)
moveis <- BETSget(1528) #Sales volume index in the retail sector - Furniture and white goods - Rio de Janeiro
veiculo <- BETSget(1554) #Sales volume index in the retail sector - Vehicles and motorcycles, 
                         #spare parts - Rio de Janeiro


varejo_setor<-ts.union(total,combustivel,mercado,textil,moveis,veiculo) %>% 
  as.xts() %>% 
  dygraph() %>% 
  dyRangeSelector()







#individual <- BETSget(15879) #Default rate of National Financial System credit 
                             #operations - Rio de Janeiro - Individual persons
#corporativo <- BETSget(15911) #Default rate of National Financial System credit 
                              #operations - Rio de Janeiro - Corporations
#total <- BETSget(15943) #Default rate of National Financial System credit operations - Rio de Janeiro - Total


data <-"2010-01-01" # data inicial


acre <- BETSget(4326, from = data)
alagoas <- BETSget(4333, from = data)
amapa <- BETSget(4327, from = data)
amazonas <- BETSget(4328, from = data)
bahia <- BETSget(4334, from = data)
ceara <- BETSget(4335, from = data)
df <- BETSget(4349, from = data)
es <- BETSget(4342, from = data)
goias <- BETSget(4350, from = data)
maranhao <- BETSget(4336, from = data)
mt <- BETSget(	4351, from = data)
ms <- BETSget(4352, from = data)
mg <- BETSget(4343, from = data)
para <- BETSget(4329, from = data)
pb <- BETSget(4337, from = data)
parana <- BETSget(4346, from = data)
pe <- BETSget(4338, from = data)
piaui <- BETSget(4339, from = data)
rj <- BETSget(4344, from = data)
rn <- BETSget(4340, from = data)
rs <- BETSget(4347, from = data)
rondonia <- BETSget(4330, from = data)
rr <- BETSget(4331, from = data)
sc <- BETSget(4348, from = data)
sp <- BETSget(4345, from = data)
sergipe <- BETSget(4341, from = data)
tocantins <- BETSget(4332, from = data)



icms_uf <- ts.union(acre, alagoas, amapa, amazonas, bahia, ceara, df ,
                    es, goias, maranhao, mt, ms, mg, para, pb, parana, 
                    pe, piaui, rj, rn, rs, rondonia, rr, sc, sp, sergipe, tocantins) %>% 
  autoplot() %>% 
  ggplotly()










### construcao da tabela de indicadores
ibc2 <- ts.union(Brasil,Rio_de_Janeiro)
#ibc2 <- cbind(as.Date(dd1),as.data.frame(dd1))
#colnames(ibc2)[1] <- "data"

ipca <- BETSget(433, from = as.character(Sys.Date()- 400)) %>% as.xts() #Broad National Consumer Price Index (IPCA)

ipca_rj <- BETSget(13381, from = as.character(Sys.Date()- 400)) %>%  as.xts() #IPCA - Broad National Consumer Price Index - monthly variation - Rio de Janeiro

emprego_rj <- BETSget(13395, from = as.character(Sys.Date()- 420)) %>%  #Formal employment created - Rio de Janeiro
              as.xts()








df = data.frame("fonte" = c("IBGE","Bacen","IBGE","MTE"),
                "tipo" = c("(%)","(%)","(%)","unidades"),
                "ultimo_mes" = c(as.yearmon(time(varejo)[nrow(varejo)]),
                                 as.yearmon(time(tab_ibc)[nrow(tab_ibc)]),
                                 as.yearmon(time(ipca)[nrow(ipca)]),
                                 as.yearmon(time(emprego_rj)[nrow(emprego_rj)])),
                "indicador" = c("varejo","IBC-Br","ipca","empregos criados"),
                "Brasil_M" = c((as.numeric(vare[nrow(vare)]) / as.numeric(first(last(vare,2),1)))-1 ,
                               as.numeric((tail(ibc2[,"Brasil"],1) / first(tail(ibc2[,"Brasil"],2)))) - 1,
                               as.numeric(tail(ipca,1))/100,NA),
                "RJ_M" = c((varejo[nrow(varejo),"rio_de_janeiro"]/varejo[nrow(varejo) - 1,"rio_de_janeiro"])-1,
                           (ibc2[nrow(ibc2),"Rio_de_Janeiro"]/ibc2[nrow(ibc2) - 1,"Rio_de_Janeiro"])-1,
                           as.numeric(tail(ipca_rj,1))/100,as.numeric(tail(emprego_rj,1))),
                "Brasil_12m" = c(((varejo[nrow(varejo),"Brasil"]/varejo[nrow(varejo) - 12,"Brasil"])-1),
                                 ((ibc2[nrow(ibc2),"Brasil"]/ibc2[nrow(ibc2) - 12,"Brasil"])-1),
                                 (prod((as.numeric(tail(ipca,12)) + 100)/100)-1),NA),
                "RJ_12m" = c(((varejo[nrow(varejo),"rio_de_janeiro"]/varejo[nrow(varejo) - 12,"rio_de_janeiro"])-1),
                             ((ibc2[nrow(ibc2),"Rio_de_Janeiro"]/ibc2[nrow(ibc2) - 12,"Rio_de_Janeiro"])-1),
                             (prod((as.numeric(tail(ipca_rj,12)) + 100)/100)-1),
                             sum(as.numeric(tail(emprego_rj,12)))))




    
  
   

d1<- cbind(df[1:3,1:4],sapply(df[1:3,5:8], percent,accuracy = .01))

df[4,5:8]<- as.factor( df[4,5:8])

d1<-rbind(d1,df[4,])
knitr::kable(d1)
 # kable_styling(c("striped", "bordered")) %>%
#  add_header_above(c(" " = 4, "no mês" = 2, "nos ultimos 12 meses" = 2))




# grafico de barra de emprego




autoplot(tail(emprego_rj,12))+
  geom_bar(stat = "identity")+
  coord_flip()+
  geom_text(aes(label = as.numeric(tail(emprego_rj,12))), color = "red", hjust =- .05,position = position_stack(vjust = 0.05))

















resultado <- BETSget(13385) # trade balance
export <- BETSget(13383) # export of goods
import <- BETSget(13384)  # imports of goods


ts.union(import,export,resultado)%>% window(start=2018) %>% autoplot() %>% ggplotly()


dd <- cbind(import,export,resultado)

# transformando em dataframe para usar no ggplot

gg <- cbind(as.Date(dd),as.data.frame(dd))
colnames(gg)[1] <- "data"
t2 <- gg %>% gather(rubrica, valor, import:resultado)







save(list = c("t1", "t2", "t3", "t4", "t5","t6","t7", "gg1", "d1", "d3", "d5",
              "last_setor", "empr"), file = "data.Rdata")

load("/Users/ibsen/data.RData")



rm(list = ls())








