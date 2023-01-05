## section dédiée à l'import du dataset
vehicle_dataset= read.csv(file = "/home/razafini/5ISS_principal/TD_big_data/Rapport_BOUJDAA_RAZAFINIARY/Electric_Vehicle_Population_Data.csv", sep=";")
head(vehicle_dataset)

##librairies utilisées
install.packages("tidyverse")
library(tidyverse) 
library(graphics)
library(ggplot2)

vehicle_dataset$County= factor(vehicle_dataset$County)
vehicle_dataset$VIN..1.10.= factor(vehicle_dataset$VIN..1.10.)
vehicle_dataset$Model.Year= factor(vehicle_dataset$Model.Year)

# 1. Eléments descriptifs de la dataset 

##*********** 1.1 répartition par type des véhicules de la dataset ****************
EV_type <- vehicle_dataset$Electric.Vehicle.Type
Vehicle_ID <- vehicle_dataset$VIN..1.10.
All_EV_sample <- nrow(vehicle_dataset)
All_EV_sample
EV_type_Rate <- group_by(vehicle_dataset,Electric.Vehicle.Type) %>% summarise(Rate = NROW(VIN..1.10.)/All_EV_sample*100)
EV_type_Rate
#affichage en pie chart 
ggplot(EV_type_Rate , aes(x = "", y = Rate, fill = Electric.Vehicle.Type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Electric Vehicles distribution by type", x = "", y = "")


##***************1.2 Model year & CAFV eligibility *********************
# Number of electric vehicles by model year and their CAFV eligibility #
##group_by(vehicle_dataset, Model.Year) %>% summarise(TotalElectricVehicles= NROW(VIN..1.10.)) %>% ggplot(aes(Model.Year,TotalElectricVehicles))+geom_col()
vehicle_dataset %>% group_by(Model.Year,Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)%>% summarise(TotalElectricVehicles= NROW(VIN..1.10.)) %>% ggplot(aes(Model.Year,TotalElectricVehicles,fill=Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility))+geom_col()

# 2. Analyse de la dataset suivant des axes d'analyse(questions que nous nous sommes posées)
##***************2.1 la ville qui concentre le plus grand nombre de véhicules électriques *********************

#nombre d’eletric vehicles/city : top 15 des villes les plus peuplés en véhicules électrique
group_by(vehicle_dataset,City) %>% summarise(TotalElectricVehicles= NROW(VIN..1.10.)) %>% arrange(-TotalElectricVehicles) %>% head(20)%>% ggplot(aes(TotalElectricVehicles,City))+geom_col()
#la ville avec le plus grand nombre de vehicules électrique 
EV_per_City <- vehicle_dataset %>% group_by(City) %>% summarise(TotalElectricVehicles= NROW(VIN..1.10.)) %>% arrange(-TotalElectricVehicles)
head(EV_per_City)

winner_city <- subset(EV_per_City,TotalElectricVehicles== max(TotalElectricVehicles))
winner_city
#it is Seattle  with 20305 vehicles
#dans la partie 2.2 on trouvera que Tacoma a le plus grand nbr de bornes de rechargement 
Tacoma_EV_nb <- subset(EV_per_City,City=="Tacoma")
Tacoma_EV_nb


#Everett se trouve dans le top 5 des villes les plus peuplés mais ne se trouve pas dans le top 15
everett_EVdensity <- subset(EV_per_City,City=="Everett")
everett_EVdensity 


##***************2.2 la ville qui concentre le plus grand nombre de bornes de rechargement *********************
vehicle_dataset %>% group_by(City) %>% summarise(TotalElectricUtility=length(unique(Electric.Utility)))%>% arrange(-TotalElectricUtility) %>% head(10)%>% ggplot(aes(TotalElectricUtility,City))+geom_col()
EU_per_City <- vehicle_dataset %>% group_by(City) %>% summarise(TotalElectricUtility=length(unique(Electric.Utility)))
seatle_EUnumber <- subset(EU_per_City,City=="Seattle")
seatle_EUnumber
#graphe qui affiche en nuage de points le nbr de véhicules/ville et le nbr de bornes de rechargement/ville
#dataset ordonné dans l'ordre décroissant de villes selon leur nbr de VE (véhicules élec)
ordered_dataset <- vehicle_dataset %>% group_by(City) %>% summarise(TotalElectricVehicles= NROW(VIN..1.10.),TotalElectricUtility=length(unique(Electric.Utility)))%>%arrange(-TotalElectricVehicles)%>%subset(TotalElectricVehicles>0)
head(ordered_dataset)
city_with8EU <- subset(EU_per_City,TotalElectricUtility==8)
city_with8EU 

# affichage en nuage de point sur le même graphe 
ggplot(ordered_dataset, aes(x=City, y=TotalElectricVehicles)) + geom_col()+ theme(axis.text.x = element_blank())
ggplot(ordered_dataset, aes(x=City, y=TotalElectricUtility)) + geom_col(color="red")+ theme(axis.text.x = element_blank())
ggplot(ordered_dataset, aes(x=TotalElectricUtility, y=TotalElectricVehicles)) + geom_col(color="red") 
# répartition % du nombre de villes associé à chq valeur de TotalElectricity Utility 
EV_EU_Rate <- group_by(ordered_dataset,TotalElectricUtility) %>% summarise(Rate=length(unique(City))/641*100)
EV_EU_Size <- group_by(ordered_dataset,TotalElectricUtility) %>% summarise(Rate=length(unique(City)))
EV_EU_Size
vehicle_dataset %>% group_by(City) %>% summarise(TotalElectricUtility=length(unique(Electric.Utility)))%>% arrange(-TotalElectricUtility) %>% head(10)%>% ggplot(aes(TotalElectricUtility,City))+geom_col()
#affichage en pie chart 
ggplot(EV_EU_Rate , aes(x = "", y = Rate, fill = TotalElectricUtility)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Cities distribution by accessible Electricity Utility number", x = "", y = "")

# 3. Analyse du marché 
##*********** 3.1 Tesla marque favorite à Washington ? ****************
vehicle_dataset %>% group_by(Make,Electric.Vehicle.Type)%>% summarise(TotalElectricVehicles= NROW(VIN..1.10.)) %>% ggplot(aes(Make,TotalElectricVehicles,fill=Electric.Vehicle.Type))+geom_col()+theme(axis.text.x = element_text(angle = 90))
#pourcentage de véhicules occupé par tesla 
make_EV_dataset <-  group_by(vehicle_dataset,Make,Electric.Vehicle.Type) %>% summarise(TotalElectricVehicles= NROW(VIN..1.10.)/112634*100, EVNumber =NROW(VIN..1.10.)) 
#112634 = all_EV_sample 
head(make_EV_dataset)
Tesla_part <- subset(make_EV_dataset,Make=="TESLA")
Tesla_part
#nombre de marques qui partagent le marché des PHEV 
make_EV_dataset %>% group_by(Electric.Vehicle.Type) %>% summarise(TotalBrands=length(unique(Make)))
#palmarès des marques les plus représentées 
make_EV_dataset %>% arrange(-TotalElectricVehicles) %>% ggplot(aes(TotalElectricVehicles,Make))+geom_col()

