
### Import data and save as MySQL database

GDP = read.csv("GDP.csv",header = T)
PopulationGrowth = read.csv("PopulationGrowth.csv",header = T)
PopulationTotal = read.csv("PopulationTotal.csv",header = T)
CO2_emission = read.csv("CO2_emission.csv",header = T)
EnergyConsumption = read.csv("EnergyConsumption.csv",header = T)

library(RMySQL) 
con <- dbConnect(RMySQL::MySQL(),host="127.0.0.1",port=3306,
                 dbname="DBSproject",user="root",
                 password="****")
dbSendQuery(con,"set character_set_results=gbk") 
dbSendQuery(con,"set character_set_client=gbk") 
dbSendQuery(con,"set character_set_connection=gbk") 
dbSendQuery(con,"set character_set_database=gbk") 
dbSendQuery(con, "SET GLOBAL local_infile = true;") 

dbWriteTable(con, "GDP", GDP,append=FALSE,row.names=FALSE)
dbWriteTable(con, "PopulationGrowth", PopulationGrowth,append=FALSE,row.names=FALSE)
dbWriteTable(con, "PopulationTotal", PopulationTotal,append=FALSE,row.names=FALSE)
dbWriteTable(con, "CO2_emission", CO2_emission,append=FALSE,row.names=FALSE)
dbWriteTable(con, "EnergyConsumption", EnergyConsumption,append=FALSE,row.names=FALSE)

#dbDisconnect(con)



### Load data from MySQL database

con <- dbConnect(RMySQL::MySQL(),host="127.0.0.1",port=3306,
                 dbname="DBSproject",user="root",
                 password="****")
dbSendQuery(con,"set character_set_results=gbk") 
dbSendQuery(con,"set character_set_client=gbk") 
dbSendQuery(con,"set character_set_connection=gbk") 
dbSendQuery(con,"set character_set_database=gbk") 
dbSendQuery(con, "SET GLOBAL local_infile = true;")

res <- dbSendQuery(con, "SELECT * FROM GDP")
GDP <- dbFetch(res, n=-1) 
res <- dbSendQuery(con, "SELECT * FROM PopulationGrowth")
PopulationGrowth <- dbFetch(res, n=-1) 
res <- dbSendQuery(con, "SELECT * FROM PopulationTotal")
PopulationTotal <- dbFetch(res, n=-1) 
res <- dbSendQuery(con, "SELECT * FROM CO2_emission")
CO2_emission <- dbFetch(res, n=-1) 
res <- dbSendQuery(con, "SELECT * FROM EnergyConsumption")
EnergyConsumption <- dbFetch(res, n=-1) 

### Preprocess GDP

GDP0 = GDP
names(GDP0) = rep("gdp",length(colnames(GDP)))
year_GDP = NULL
for (Country in GDP$Country.Name){
  country_year_col = t(GDP[GDP$Country.Name==Country,-1])
  country_year_block = cbind(rep(Country,length(country_year_col)),country_year_col)
  year_GDP = rbind(year_GDP, country_year_block)
}
year_GDP = cbind(year_GDP, 1960:2020) 
year_GDP = as.data.frame(year_GDP[,c(1,3,2)])
colnames(year_GDP) = c("Country.Name","Year","gdp")


### Preprocess PopulationGrowth 

PopulationGrowth0 = PopulationGrowth
names(PopulationGrowth0) = rep("PopulationGrowth",length(colnames(PopulationGrowth)))
year_PopulationGrowth = NULL
for (Country in PopulationGrowth$Country.Name){
  country_year_col = t(PopulationGrowth[PopulationGrowth$Country.Name==Country,-1])
  country_year_block = cbind(rep(Country,length(country_year_col)),country_year_col)
  year_PopulationGrowth = rbind(year_PopulationGrowth, country_year_block)
}
year_PopulationGrowth = cbind(year_PopulationGrowth, 1960:2020) 
year_PopulationGrowth = as.data.frame(year_PopulationGrowth[,c(1,3,2)])
colnames(year_PopulationGrowth) = c("Country.Name","Year","PopulationGrowth")


### Cbind all

Cdata = merge(PopulationTotal,year_GDP, by = c("Country.Name","Year"), all = TRUE)
Cdata = merge(Cdata, year_PopulationGrowth, by = c("Country.Name","Year"), all = TRUE)
Cdata = merge(Cdata, CO2_emission, by = c("Country.Name","Year"), all = TRUE)
Cdata = merge(Cdata, EnergyConsumption, by = c("Country.Name","Year"), all = TRUE)

colnames(Cdata)[6] = "CO2_Emission"
Cdata = na.omit(Cdata)

### select years: 1997-2017 and fill NA

Cdata_97_17 = Cdata[Cdata$Year>1996 & Cdata$Year<2018,]

fill_na <- function(col){
  expand_value = c()
  i = 0
  for (x in col) {
    expand_value = c(expand_value, x)
    expand_value = na.omit(expand_value)
    i = i+1
    l = length(expand_value)
    if (l>2){
      if (is.na(x)){
        col[i]=expand_value[l]
      }
    }
  }
  return(col)
}

Cdata_97_17e <- Cdata_97_17
for (i in 3:19) {
  Cdata_97_17e[,i] <- fill_na(Cdata_97_17[,i]) 
}

dbWriteTable(con, "Cdata_97_17e", Cdata_97_17e,append=FALSE,row.names=FALSE)

### GDP clustering

# Use log GDP per capita to clustering
GDP_omit_na = na.omit(GDP[,c(1,7:(length(GDP[0,])-3))])
GDP_capita = GDP_omit_na
for (country in countries) {
  for ( year in years) {
    GDP_capita[GDP_capita$Country.Name==country, year] = NA
  }
}

GDP_ln_capita = GDP_capita
countries = GDP_omit_na$Country.Name
years = colnames(GDP_omit_na)[-1]

for (country in countries) {
  for ( year in years) {
    capita = GDP_omit_na[GDP_omit_na$Country.Name==country, year] / PopulationTotal_Cross[PopulationTotal_Cross$Country.Name==country, year]
    if (length(capita)!=0){
      GDP_capita[GDP_capita$Country.Name==country, year] = capita
      GDP_ln_capita[GDP_ln_capita$Country.Name==country, year] = log(capita)
    }
  }
}
GDP_capita = na.omit(GDP_capita)
GDP_ln_capita = na.omit(GDP_ln_capita)

set.seed(1)
GDP_km <- kmeans(GDP_ln_capita[34:54], 3)
GDP_km$centers[,1]
CountryCluster = as.data.frame(cbind(as.character(GDP_capita$Country.Name), GDP_km$cluster))

colnames(CountryCluster)<- c("Country.Name","GDP_Cluster")
country_num<-length(CountryCluster$Country.Name)
for (i in 1:country_num) {
  if (CountryCluster[i,2]==1){CountryCluster[i,2]="High"}
  if (CountryCluster[i,2]==2){CountryCluster[i,2]="Middle"}
  if (CountryCluster[i,2]==3){CountryCluster[i,2]="Low"}
}


Cdata_97_17 <- Cdata[Cdata$Year<=2017 & Cdata$Year>=1997,]
Cdata_97_17_e <- Cdata_97_17
for (i in 3:19) {
  Cdata_97_17_e[,i] <- fill_na(Cdata_97_17[,i]) 
}
CountryCluster_97_17<-merge(CountryCluster,Cdata_97_17_e, by = c("Country.Name"), all = TRUE)
CountryCluster_97_17<-CountryCluster_97_17[!is.na(CountryCluster_97_17$GDP_Cluster),]
CC_97_17<-na.omit(CountryCluster_97_17)

### calculate data per capita
GDP_pc <- log(as.numeric(CC_97_17$gdp) / as.vector(CC_97_17$Population))
CO2_Emission_pc <- as.vector(CC_97_17$CO2_Emission) / as.vector(CC_97_17$Population)
primary_energy_consumption_pc <- CC_97_17$primary_energy_consumption / CC_97_17$Population
renewables_consumption_pc <- CC_97_17$renewables_consumption / CC_97_17$Population
biofuel_consumption_pc <- CC_97_17$biofuel_consumption / CC_97_17$Population
coal_consumption_pc <- CC_97_17$coal_consumption / CC_97_17$Population
fossil_fuel_consumption_pc <- CC_97_17$fossil_fuel_consumption / CC_97_17$Population
gas_consumption_pc <- CC_97_17$gas_consumption / CC_97_17$Population
hydro_consumption_pc <- CC_97_17$hydro_consumption / CC_97_17$Population
low_carbon_consumption_pc <- CC_97_17$low_carbon_consumption / CC_97_17$Population
nuclear_consumption_pc <- CC_97_17$nuclear_consumption / CC_97_17$Population
oil_consumption_pc <- CC_97_17$oil_consumption / CC_97_17$Population
solar_consumption_pc <- CC_97_17$solar_consumption / CC_97_17$Population
wind_consumption_pc <- CC_97_17$wind_consumption / CC_97_17$Population
other_renewable_consumption_pc <- CC_97_17$other_renewable_consumption / CC_97_17$Population

CC_97_17 <- cbind(CC_97_17,GDP_pc,CO2_Emission_pc,primary_energy_consumption_pc,renewables_consumption_pc,
                  biofuel_consumption_pc,coal_consumption_pc,fossil_fuel_consumption_pc,gas_consumption_pc,
                  hydro_consumption_pc,low_carbon_consumption_pc,nuclear_consumption_pc,oil_consumption_pc,
                  solar_consumption_pc,wind_consumption_pc,other_renewable_consumption_pc)
# Save in MySQL
dbWriteTable(con, "CC_97_17", CC_97_17,append=FALSE,row.names=FALSE)


































