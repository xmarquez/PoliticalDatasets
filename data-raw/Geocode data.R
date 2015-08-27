library(ggmap)

data <- PoliticalDatasets::data

data <- data %>% select(-lat,-lon)

countries <- paste0("country:",data$country_name)

countries[ grepl("Cote",countries) ] <- "country: Ivory Coast"
from_country <- paste0("country:",c("Great Colombia","Germany (Prussia)","German Federal Republic","German Democratic Republic","Württemberg","Hesse-Kassel (Electoral)","Hesse-Darmstadt (Ducal)","Mecklenburg-Schwerin","Austria-Hungary","Czechoslovakia","Papal States","Macedonia (Former Yugoslav Republic of)","Russia (Soviet Union)","Belarus (Byelorussia)","South Ossetia","São Tomé and Principe","Burkina Faso (Upper Volta)","Congo, Democratic Republic of (Zaire)","Tanzania/Tanganyika","Transvaal","Madagascar (Malagasy)","Turkey (Ottoman Empire)","Yemen (Arab Republic of Yemen)","Yemen, People's Republic of","Cambodia (Kampuchea)", "Vietnam (Annam/Cochin China/Tonkin)","Vietnam, Democratic Republic of","United Provinces of Central America","Korea, People's Republic of","Abkhazia","Vietnam, Republic of","Korea, Republic of"))

to_country <- c("country:Colombia","country:Germany","locality:Bonn","locality:Berlin","locality:Stuttgart","locality:Kassel","locality:Kassel","locality:Schwerin","locality:Vienna","locality:Prague","country:Vatican City","country:Macedonia","country:Russia","country:Belarus","locality:Tskhinvali","country:Sao Tome and Principe","country:Burkina Faso","country:Democratic Republic of the Congo","country:Tanzania","location:Pretoria","country:Madagascar","country:Turkey","country:Yemen","locality:Aden","country:Cambodia", "country:Vietnam","locality:Hanoi","country:Guatemala","country:North Korea","country:Georgia&location:Sokhumi","locality:Saigon","South Korea")

locations <- geocode(plyr::mapvalues(countries,from = from_country, to = to_country))

data <- cbind(data,locations)

devtools::use_data(data, internal=TRUE, overwrite = TRUE)

# Test
library(rworldmap)
library(ggplot2)

world <- fortify(getMap())
ggplot() + geom_map(map=world,map_id=id) + geom_path(data=world,aes(x=long,y=lat,group=group)) + geom_text(data = data %>% filter(country_name %in% c("Great Colombia","Germany (Prussia)","German Federal Republic","German Democratic Republic","Württemberg","Hesse-Kassel (Electoral)","Hesse-Darmstadt (Ducal)","Mecklenburg-Schwerin","Austria-Hungary","Czechoslovakia","Papal States","Macedonia (Former Yugoslav Republic of)","Russia (Soviet Union)","Belarus (Byelorussia)","South Ossetia","São Tomé and Principe","Cote D'Ivoire","Burkina Faso (Upper Volta)","Congo, Democratic Republic of (Zaire)","Tanzania/Tanganyika","Transvaal","Madagascar (Malagasy)","Turkey (Ottoman Empire)","Yemen (Arab Republic of Yemen)","Yemen, People's Republic of","Cambodia (Kampuchea)", "Vietnam (Annam/Cochin China/Tonkin)","Vietnam, Democratic Republic of","United Provinces of Central America","Korea, People's Republic of","Abkhazia","Vietnam, Republic of","Korea, Republic of","Georgia")), aes(x=lon,y=lat,label=country_name)) 
