library(tidyverse)
library(readxl)
library(devtools)


df22<-read_xlsx("data-raw/T2-2-FMS.xlsx")%>%
  gather(key="YEAR",value="TU_PROD_PERYEAR", 2:7)%>%
  mutate(
    YEAR=as.double(YEAR),
    TU_PROD_PERYEAR=as.double(TU_PROD_PERYEAR)
         )%>%
  select(COUNTRY, YEAR, TU_PROD_PERYEAR)


df23<-read_xlsx("data-raw/T2-3-FMS.xlsx")%>%
  gather(key="YEAR",value="UREQS_LOW", 2,4,6,8,10,12,14)%>%
  gather(key="YEAR",value="UREQS_HIGH", 2:8)%>%
  select(COUNTRY, YEAR, UREQS_LOW, UREQS_HIGH)%>%
  mutate(YEAR = substr(YEAR, 1, 4)#keeps only the year of the column-now-row name
    )%>%
  arrange(YEAR)



df24<-read_xlsx("data-raw/T2-4-FMS.xlsx")%>%
  gather(key="YEAR", value="CC", 3:9)%>%
  spread(key="CCTO", value = "CC") %>%
  arrange(COUNTRY)



df25_a<-read_xlsx("data-raw/T2-5-FMS.xlsx")%>%
  gather(key="YEAR", value="CR_LOW", 3,5,7,9,11,13, 15)%>%
  mutate(YEAR = substr(YEAR, 1, 4)#keeps only the year of the column-now-row name
    ) %>%
  select(COUNTRY, YEAR, CR, CR_LOW)

df25_b<-read_xlsx("data-raw/T2-5-FMS.xlsx")%>%
  gather(key="YEAR", value="CR_HIGH", 4,6,8,10,12,14, 16)%>%
  mutate(YEAR = substr(YEAR, 1, 4)#keeps only the year of the column-now-row name
    ) %>%
  select(COUNTRY, YEAR, CR, CR_HIGH)

df25<-merge(df25_a, df25_b, by = c("COUNTRY", "YEAR","CR"))



df26<-read_xlsx("data-raw/T2-6-FMS.xlsx")%>%
  gather(key="YEAR", value="ECAP", 3:9)%>%
  mutate(YEAR=as.double(YEAR),
         ECAP=as.double(ECAP))%>%
  select(COUNTRY, YEAR, METHOD, ECAP)



df27<-read_xlsx("data-raw/T2-7-FMS.xlsx")

df27_a<-df27%>%
  gather(key="YEAR", value="ER_LOW", 2,4,6,8,10,12,14)%>%
  mutate(YEAR = substr(YEAR, 1, 4)#keeps only the year of the column-now-row name
    ) %>%
  select(COUNTRY, YEAR, ER_LOW)

df27_b<-df27%>%
  gather(key="YEAR", value="ER_HIGH", 3,5,7,9,11,13,15)%>%
  mutate(YEAR = substr(YEAR, 1, 4)#keeps only the year of the column-now-row name
    ) %>%
  select(COUNTRY, YEAR, ER_HIGH)

df27<-merge(df27_a, df27_b)


df28<-read_xlsx("data-raw/T2-8-FMS.xlsx")%>%
  gather(key="YEAR", value="FABCAP", 3:9)%>%
  mutate(YEAR=as.double(YEAR),
         FABCAP=as.double(FABCAP))%>%
  select(COUNTRY, YEAR, FUELTYPE, FABCAP)%>%
  arrange(COUNTRY)



df29_a<-read_xlsx("data-raw/T2-9-FMS.xlsx")%>%
  gather(key="YEAR", value="FABREQ_LOW", 3,5,7,9,11,13, 15)%>%
  mutate(YEAR = substr(YEAR, 1, 4)#keeps only the year of the column-now-row name
    ) %>%
  select(COUNTRY, YEAR, FUELTYPE, FABREQ_LOW)

df29_b<-read_xlsx("data-raw/T2-9-FMS.xlsx")%>%
  gather(key="YEAR", value="FABREQ_HIGH", 4,6,8,10,12,14, 16)%>%
  mutate(YEAR = substr(YEAR, 1, 4)#keeps only the year of the column-now-row name
    ) %>%
  select(COUNTRY, YEAR, FUELTYPE, FABREQ_HIGH)

df29<-merge(df29_a, df29_b, by = c("COUNTRY", "YEAR","FUELTYPE"))

df210<-read_xlsx("data-raw/T2-10-FMS.xlsx")%>%
  gather(key="YEAR", value="SNFSTOCAP", 3:9)%>%
  mutate(YEAR=as.double(YEAR),
         SNFSTOCAP=as.double(SNFSTOCAP))%>%
  select(COUNTRY, YEAR, FUELTYPE, SNFSTOCAP)%>%
  arrange(COUNTRY)



df211_a<-read_xlsx("data-raw/T2-11-FMS.xlsx")%>%
  gather(key="YEAR", value="ARISINGS", 2,4,6,8,10,12,14)%>%
  mutate(
    YEAR = as.double(substr(YEAR, 1, 4)),
    ARISINGS=as.double(ARISINGS)
    )%>%
  select(COUNTRY, YEAR, ARISINGS)

df211_b<-read_xlsx("data-raw/T2-11-FMS.xlsx")%>%
  gather(key="YEAR", value="INSTORAGE", 3,5,7,9,11,13,15)%>%
  mutate(
    YEAR = as.double(substr(YEAR, 1, 4)),
    INSTORAGE=as.double(INSTORAGE)
    )%>%
  select(COUNTRY, YEAR, INSTORAGE)

df211<-merge(df211_a, df211_b)


df212<-read_xlsx("data-raw/T2-12-FMS.xlsx")%>%
  gather(key="YEAR", value="REPROCAP", 3:9)%>%
  mutate(YEAR=as.double(YEAR),
         REPRO=as.double(REPROCAP))%>%
  select(COUNTRY, YEAR, FUELTYPE, REPROCAP)%>%
  arrange(COUNTRY)

#write.csv(df210, "../data/T2-12-FMS-tidy.csv")

df213<-read_xlsx("data-raw/T2-13-FMS.xlsx")%>%
  gather(key="YEAR", value="PU_USE", 3:9)%>%
  mutate(YEAR=as.double(YEAR),
         RPU_USE=as.double(PU_USE))%>%
  select(COUNTRY, YEAR, FUELTYPE, PU_USE)%>%
  arrange(COUNTRY)


## Problem with original data !!


# df214<-read_xlsx("data-raw/T2-14-FMS.xlsx")%>%
#   gather(key="YEAR", value="PU_USE", 3:9)%>%
#   mutate(YEAR=as.double(YEAR),
#          RPU_USE=as.double(PU_USE))%>%
#   select(COUNTRY, YEAR, FUELTYPE, PU_USE)%>%
#   arrange(COUNTRY)

#write.csv(df214, "../data/T2-14-FMS-tidy.csv")
#datatable(df214)



df215<-read_xlsx("data-raw/T2-15-FMS.xlsx")%>%
  gather(key="YEAR", value="TOT_RET_USE", 2:5)%>%
  mutate(YEAR=as.double(YEAR),
         TOT_RET_USE=as.double(TOT_RET_USE))%>%
  select(COUNTRY, YEAR, TOT_RET_USE)%>%
  arrange(COUNTRY)

#write.csv(df215, "../data/T2-15-FMS-tidy.csv")


df216<-read_xlsx("data-raw/T2-16-FMS.xlsx")%>%
  gather(key="YEAR", value="TOT_RU_PROD", 2:5)%>%
  mutate(YEAR=as.double(YEAR),
         TOT_RU_PROD=as.double(TOT_RU_PROD))%>%
  select(COUNTRY, YEAR, TOT_RU_PROD)%>%
  arrange(COUNTRY)



df216<-read_xlsx("data-raw/T2-16-FMS.xlsx")%>%
  gather(key="YEAR", value="TOT_RU_PROD", 2:5)%>%
  mutate(YEAR=as.double(YEAR),
         TOT_RU_PROD=as.double(TOT_RU_PROD))%>%
  select(COUNTRY, YEAR, TOT_RU_PROD)%>%
  arrange(COUNTRY)


##

use_data(df22, overwrite = TRUE)
use_data(df23, overwrite = TRUE)
use_data(df24, overwrite = TRUE)
use_data(df25, overwrite = TRUE)
use_data(df26, overwrite = TRUE)
use_data(df27, overwrite = TRUE)
use_data(df28, overwrite = TRUE)
use_data(df28, overwrite = TRUE)
use_data(df29, overwrite = TRUE)
use_data(df210, overwrite = TRUE)
use_data(df211, overwrite = TRUE)
use_data(df212, overwrite = TRUE)
use_data(df213, overwrite = TRUE)
use_data(df215, overwrite = TRUE)
use_data(df216, overwrite = TRUE)



# df22<-read_csv("../data/T2-2-FMS-tidy.csv")
# df23<-read_csv("../data/T2-3-FMS-tidy.csv")
# df24<-read_csv("../data/T2-4-FMS-tidy.csv")
# df25<-read_csv("../data/T2-5-FMS-tidy.csv")
# df26<-read_csv("../data/T2-6-FMS-tidy.csv")
# df27<-read_csv("../data/T2-7-FMS-tidy.csv")
# df28<-read_csv("../data/T2-8-FMS-tidy.csv")
# df29<-read_csv("../data/T2-9-FMS-tidy.csv")
# df210<-read_csv("../data/T2-10-FMS-tidy.csv")
# df211<-read_csv("../data/T2-11-FMS-tidy.csv")




