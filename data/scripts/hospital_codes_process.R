library(tidyverse)
library(readxl)
# http://www.consultaesic.cgu.gov.br/busca/dados/Lists/Pedido/Attachments/524348/RESPOSTA_PEDIDO_PROTOCOLO%20-25820005484201626%20CNES_DEMANDA_SIC.xlsx
hosp <- read_xlsx("/Users/jhathaway/odrive/Amazon Cloud Drive/data/RESPOSTA_PEDIDO_PROTOCOLO -25820005484201626 CNES_DEMANDA_SIC.xlsx", skip = 10)
#locale = locale(encoding = 'ISO-8559-1')

colnames(hosp) <- c("name", "ibge", "municipality", "cnes", "hospital_name", "hospital_address", "street_number", "neighborhood", "notes", "cep",
                    "legal_nature", "health_professionals", "beds", "sus_beds")

hosp <- hosp %>%
  mutate(health_estbl_code = parse_character(cnes)) 

data_publish(hosp, name = "hospital_information", file_type = "csv", type = "discovered")


