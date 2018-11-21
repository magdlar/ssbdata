## options and libraries
options(encoding="UTF-8")
options(stringsAsFactors = FALSE)
library("httr") # for POST
library("rjstat") # for using JSON-stat format
library("dplyr") # just using piping "%>%" in a few places

## get labour force statistics
url <- c("https://data.ssb.no/api/v0/no/table/05613/") # table for employment and labour force

request <- '
{  "query": [    {      "code": "Region",      "selection": {        "filter": "item",        "values": [          "01",          "02",          "03",          "04",          "05",          "06",          "07",          "08",          "09",          "10",          "11",          "12",          "14",          "15",          "16",          "17",          "18",          "19",          "20"        ]      }    },    {      "code": "ContentsCode",      "selection": {        "filter": "item",        "values": [          "Sysselsatte"        ]      }    },    {      "code": "Tid",      "selection": {        "filter": "item",        "values": [          "1996",          "1997",          "1998",          "1999",          "2000",          "2001",          "2002",          "2003",          "2004",          "2005",          "2006",          "2007",          "2008",          "2009",          "2010",          "2011",          "2012",          "2013",          "2014",          "2015",          "2016",          "2017"        ]      }    }  ],  "response": {    "format": "json-stat"  }}
'

## get and clean the data
table <- POST(url, body = request, encode = "json", verbose()) 
table <- fromJSONstat(content(table, "text"))
table <- table[[1]]

## fix naming errors and prepare for aggregating Trøndelag
table[table=="Troms - Romsa"] <- "Troms"
table[table=="Finnmark - Finnmárku"] <- "Finnmark"
table[table=="Nord-Trøndelag (-2017)"] <- "Trøndelag"
table[table=="Sør-Trøndelag (-2017)"] <- "Trøndelag"

## aggregate the observations for Trøndelag
arbledighet <- aggregate(list(verdi = table$value), by=list(region = table$region, år = table$år, variable = table$arbeidsstyrkestatus), FUN=sum) # aggregate observations for Trøndelag

## get variables in long format 
arbledighet <- reshape(arbledighet, idvar = c("region", "år"), timevar = "variable", direction = "wide") # reshape to wide
names(arbledighet) <- c("region", "år", "arbeidsstyrke", "sysselsatte") # fix names
arbledighet$arbeidsledighet <- 1-(arbledighet$sysselsatte/arbledighet$arbeidsstyrke) # caulculate unemplyment as percentage
arbledighet$år <- arbledighet$år %>% as.numeric() # change year from character to numeric

## get absence statistics
url <- c("https://data.ssb.no/api/v0/no/table/08320/") # table for doctor approved sickleave

request <- '
{  "query": [    {      "code": "Region",      "selection": {        "filter": "item",        "values": [          "01",          "02",          "03",          "04",          "05",          "06",          "07",          "08",          "09",          "10",          "11",          "12",          "14",          "15",          "16",          "17",          "18",          "19",          "20"        ]      }    },    {      "code": "ContentsCode",      "selection": {        "filter": "item",        "values": [          "Sykefraversdagsverk",          "Sykefraversprosent"        ]      }    },    {      "code": "Tid",      "selection": {        "filter": "item",        "values": [          "2001K1",          "2001K2",          "2001K3",          "2001K4",          "2002K1",          "2002K2",          "2002K3",          "2002K4",          "2003K1",          "2003K2",          "2003K3",          "2003K4",          "2004K1",          "2004K2",          "2004K3",          "2004K4",          "2005K1",          "2005K2",          "2005K3",          "2005K4",          "2006K1",          "2006K2",          "2006K3",          "2006K4",          "2007K1",          "2007K2",          "2007K3",          "2007K4",          "2008K1 Ny",          "2008K2 Ny",          "2008K3 Ny",          "2008K4 Ny",          "2009K1",          "2009K2",          "2009K3",          "2009K4",          "2010K1",          "2010K2",          "2010K3",          "2010K4",          "2011K1",          "2011K2",          "2011K3",          "2011K4",          "2012K1",          "2012K2",          "2012K3",          "2012K4",          "2013K1",          "2013K2",          "2013K3",          "2013K4",          "2014K1",          "2014K2",          "2014K3",          "2014K4",          "2015K1",          "2015K2",          "2015K3",          "2015K4",          "2016K1",          "2016K2",          "2016K3",          "2016K4",          "2017K1",          "2017K2",          "2017K3",          "2017K4"        ]      }    }  ],  "response": {    "format": "json-stat"  }}
'

## get and clean the data
table <- POST(url, body = request, encode = "json", verbose()) 
table <- fromJSONstat(content(table, "text"))
table <- table[[1]]

table$kvartal <- gsub(" Ny","",table$kvartal) # remove "new" marker on some quarterly data
table <- reshape(table, idvar = c("region", "kvartal"), timevar = "statistikkvariabel", direction = "wide") # reshape to wide format
names(table) <- c("region","kvartal","sykdagsverk","sykprosent") # fix names
table$sykprosent <- table$sykprosent/100 # normalize percetages to be 0 to 1
table$avtdagsverk <- table$sykdagsverk/table$sykprosent # calculate the agreed working days
table <- subset(table, select=c("region","kvartal","sykdagsverk","avtdagsverk")) # drop the percentage column

## fix naming errors and prepare for aggregating Trøndelag
table[table=="Troms - Romsa"] <- "Troms"
table[table=="Finnmark - Finnmárku"] <- "Finnmark"
table[table=="Nord-Trøndelag (-2017)"] <- "Trøndelag"
table[table=="Sør-Trøndelag (-2017)"] <- "Trøndelag"

table$år <- substr(table$kvartal,1,4) %>% as.numeric() # extract year from the quarter strings

sykprosent <- aggregate(list(sykdagsverk=table$sykdagsverk,avtdagsverk=table$avtdagsverk), by=list(region = table$region, år = table$år), FUN=sum) # aggregate by year and for Trøndelag
sykprosent$sykprosent <- sykprosent$sykdagsverk/sykprosent$avtdagsverk # calculate the yearly sickleave as a percentage

## save the tables to files
saveRDS(arbledighet,file="arbledighet.RDS")
saveRDS(sykprosent,file="sykprosent.RDS")