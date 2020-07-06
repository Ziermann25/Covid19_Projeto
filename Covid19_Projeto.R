{# Check if the packages that we need are installed
  want = c("tidyverse", "data.table", "readr", "stringr", "zoo", "plyr", "dplyr", "COVID19", "usethis", "tidyr")
  have = want %in% rownames(installed.packages())
  # Install the packages that we miss
  if ( any(!have) ) { install.packages( want[!have] ) }
  # Load the packages
  junk <- lapply(want, library, character.only = T)
  # Remove the objects we created
  rm(have, want, junk)
}# import packages


# Git information
usethis::use_git_config(user.name = "Matheus Felipe Ziermann Vieira", # full name
                        user.email = "ziermann25@hotmail.com") # Semail
usethis::browse_github_token()

GITHUB_PAT= "3a5f09c033bb0132f12b63377e167eead1b8c6c3"


# database by city
{
  ## Import COVID cities database
  
  # Declare the download link 
  url = "https://data.brasil.io/dataset/covid19/caso.csv.gz"
  
  # Create temporary file
  tmp = tempfile()
  
  # Download the .gz file
  download.file(url,tmp)
  
  # Finish import process
  dcovid19 =   read_csv(gzfile(tmp),
                        col_types = cols(date = col_date(format = "%Y-%m-%d")), 
                        locale = locale(decimal_mark = ",", grouping_mark = ".", 
                                        encoding = "UTF-8"))
  # Create dcitypr database
  dcitypr = dcovid19 %>% filter(state == "PR" & place_type == "city" & date > "2020-03-15") %>%
    mutate(select = case_when(city_ibge_code == 4104808 ~ "Cascavel",
                              city_ibge_code == 4105805 ~ "Colombo",
                              city_ibge_code == 4106902 ~ "Curitiba",
                              city_ibge_code == 4108304 ~ "Foz do Igua?u",
                              city_ibge_code == 4109401 ~ "Guarapuava",
                              city_ibge_code == 4113700 ~ "Londrina",
                              city_ibge_code == 4115200 ~ "Maring?",
                              city_ibge_code == 4118204 ~ "Paranagu?",
                              city_ibge_code == 4119905 ~ "Ponta Grossa",
                              city_ibge_code == 4125506 ~ "S?o Jos? dos Pinhais",
                              TRUE ~ "Outras cidades")) %>%
    arrange(desc(date)) %>%
    group_by(date, select) %>%
    dplyr::summarize(confirmed = sum(confirmed),
                     deaths = sum(deaths),
                     population = sum(estimated_population_2019)) 
  
  # Include a time lag variables
  setDT(dcitypr)[, deaths_1 := shift(deaths, fill=0), by = select]
  setDT(dcitypr)[, confirmed_1 := shift(confirmed, fill=0), by = select]
  
  # Organize dcitypr database
  dcitypr = dcitypr %>% mutate(deaths_new = deaths - deaths_1,
                               confirmed_new = confirmed - confirmed_1) %>%
    ungroup() %>% select(date, select, confirmed, confirmed_new, deaths, deaths_new) %>% 
    arrange(desc(date))
  
  # Drop unnecessary databases
  remove(list = c("tmp", "url"))  
  
} # database by city


{
  ## Import COVID cities database
  
  # Declare the download link 
  url = "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/dados-pr.csv"
  
  # Create temporary file
  tmp = tempfile()
  
  # Download the .gz file
  download.file(url,tmp)
  
  # Finish import process
  dcovid19sy = read_delim(gzfile(tmp), ";", escape_double = FALSE, 
                          locale = locale(decimal_mark = ",", grouping_mark = ".", 
                                          encoding = "ISO-8859-1"), trim_ws = TRUE)
  
} # database by individual cases


covid_pr_sc=dcovid19 %>% filter(state==c("PR","SC"))
covid_sc=covid_pr_sc %>% filter(state==c("SC"))
covid_pr=covid_pr_sc %>% filter(state==c("PR"))

ggplot(dcovid19, aes(x=state,y=deaths))+geom_col()+expand_limits(y=0)+ggtitle("Mortes por estado")
ggplot(covid_pr_sc, aes(x=state,y=confirmed))+geom_col()+expand_limits(y=0)+ggtitle("Casos confirmado por estado")
ggplot(covid_pr, aes(x=date, y=deaths))+geom_line()+ggtitle("Mortes Paraná")
ggplot(covid_sc, aes(x=date, y=deaths))+geom_line()+ggtitle("Mortes Santa Catarina")


