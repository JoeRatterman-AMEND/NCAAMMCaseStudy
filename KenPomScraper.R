# Code from https://lakedatainsights.com/2019/04/08/scraping-web-page-tables-with-r/
library(XML)
library(RCurl)
library(rvest)
library(stringr)
library(data.table)


getKenPomYearData <- function(year){
  theUrl <- paste0("https://kenpom.com/index.php?y=", as.character(year))
  page <- read_html(theUrl)
  tables <- page %>% html_nodes("table") %>% html_table()
  data <- as.data.frame(tables[1])
  
  colnames(data) <- c("Rank", "Team", "Conf", "Record", "AdjEM", "AdjO", 
                      "AdjO_R", "AdjD", "AdjD_R", "AdjT", "AdjT_R",
                      "Luck", "Luck_R", "SoS_AdjEM", "SoS_AdjEM_R", 
                      "OppO", "OppO_R", "OppD", "OppD_R", "NC_AdjEM", "NC_AdjEM_R")
  
  data <- data %>% filter(!str_detect(Rank, "Rk")) # Remove label row
  
  data <- data %>% filter(nchar(as.character(Rank)) > 0) %>% as_tibble() # Remove empty rank rows.
  data$Year = year
  
  return(data)
}



# Clean Names from KenPom to match NCAA Dataset
#https://www.kaggle.com/paulorzp/kenpom-scraper-2020

# Sample of pulling 2020 KenPom data
kenpom <- getKenPomYearData(2020)

# Convert to data table for formatting
setDT(kenpom)


kenpom[,Team:=gsub("\\.","",team)]
kenpom[,Team:=gsub("Cal St","CS",Team)]
kenpom[,Team:=gsub("Albany","SUNY Albany",Team)]
kenpom[,Team:=gsub("Abilene Christian","Abilene Chr",Team)]
kenpom[,Team:=gsub("American","American Univ",Team)]
kenpom[,Team:=gsub("Arkansas Little Rock","Ark Little Rock",Team)]
kenpom[,Team:=gsub("Arkansas Pine Bluff","Ark Pine Bluff",Team)]
kenpom[,Team:=gsub("Boston University","Boston Univ",Team)]
kenpom[,Team:=gsub("Central Michigan","C Michigan",Team)]
kenpom[,Team:=gsub("Central Connecticut","Central Conn",Team)]
kenpom[,Team:=gsub("Coastal Carolina","Coastal Car",Team)]
kenpom[,Team:=gsub("East Carolina","E Kentucky",Team)]
kenpom[,Team:=gsub("Eastern Washington","E Washington",Team)]
kenpom[,Team:=gsub("East Tennessee St","ETSU",Team)]
kenpom[,Team:=gsub("Fairleigh Dickinson","F Dickinson",Team)]
kenpom[,Team:=gsub("Florida Atlantic","FL Atlantic",Team)]
kenpom[,Team:=gsub("Florida Gulf Coast","FL Gulf Coast",Team)]
kenpom[,Team:=gsub("George Washington","G Washington",Team)]
kenpom[,Team:=gsub("Illinois Chicago","IL Chicago",Team)]
kenpom[,Team:=gsub("Kent St","Kent",Team)]
kenpom[,Team:=gsub("Monmouth","Monmouth NJ",Team)]
kenpom[,Team:=gsub("Mississippi Valley St","MS Valley St",Team)]
kenpom[,Team:=gsub("Mount St Mary's","Mt St Mary's",Team)]
kenpom[,Team:=gsub("Montana St","MTSU",Team)]
kenpom[,Team:=gsub("Northern Colorado","N Colorado",Team)]
kenpom[,Team:=gsub("North Dakota St","N Dakota St",Team)]
kenpom[,Team:=gsub("Northern Kentucky","N Kentucky",Team)]
kenpom[,Team:=gsub("North Carolina A&T","NC A&T",Team)]
kenpom[,Team:=gsub("North Carolina Central","NC Central",Team)]
kenpom[,Team:=gsub("North Carolina St","NC State",Team)]
kenpom[,Team:=gsub("Northwestern St","Northwestern LA",Team)]
kenpom[,Team:=gsub("Prairie View A&M","Prairie View",Team)]
kenpom[,Team:=gsub("South Carolina St","S Carolina St",Team)]
kenpom[,Team:=gsub("South Dakota St","S Dakota St",Team)]
kenpom[,Team:=gsub("Southern Illinois","S Illinois",Team)]
kenpom[,Team:=gsub("Southeastern Louisiana","SE Louisiana",Team)]
kenpom[,Team:=gsub("Stephen F Austin","SF Austin",Team)]
kenpom[,Team:=gsub("Southern","Southern Univ",Team)]
kenpom[,Team:=gsub("Southern Univ Miss","Southern Miss",Team)]
kenpom[,Team:=gsub("Saint Joseph's","St Joseph's PA",Team)]
kenpom[,Team:=gsub("Saint Louis","St Louis",Team)]
kenpom[,Team:=gsub("Saint Mary's","St Mary's CA",Team)]
kenpom[,Team:=gsub("Saint Peter's","St Peter's",Team)]
kenpom[,Team:=gsub("Texas A&M Corpus Chris","TAM C. Christi",Team)]
kenpom[,Team:=gsub("Troy St","Troy",Team)]
kenpom[,Team:=gsub("Texas Southern Univ","TX Southern",Team)]
kenpom[,Team:=gsub("Louisiana Lafayette","Louisiana",Team)]
kenpom[,Team:=gsub("UTSA","UT San Antonio",Team)]
kenpom[,Team:=gsub("Western Michigan","W Michigan",Team)]
kenpom[,Team:=gsub("Green Bay","WI Green Bay",Team)]
kenpom[,Team:=gsub("Milwaukee","WI Milwaukee",Team)]
kenpom[,Team:=gsub("Western Kentucky","WKU",Team)]
kenpom[,Team:=gsub("College of Charleston","Col Charleston",Team)]
kenpom[,Team:=gsub("Loyola Chicago","Loyola-Chicago",Team)]
