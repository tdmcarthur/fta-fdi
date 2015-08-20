http://faculty.som.yale.edu/peterschott/sub_international.htm


New port idea code:
  
  
  
  exports.df  <- read.csv( "/Users/travismcarthur/Downloads/Port-level%20Exports(1).csv"  ,
                           stringsAsFactors=FALSE, skip=2)

# Country

# [1] "Asia"                  "Europe"                "Africa"               
#[4] "Australia and Oceania" "South/Central America"

exports.df$HS.2.digit <- substring(exports.df$Commodity, 1, 2)
exports.df$HS.4.digit <- substring(exports.df$Commodity, 1, 4)
exports.df$HS.6.digit <- substring(exports.df$Commodity, 1, 6)

exports.df$Total.Value...US. <- as.numeric(gsub(",", "", exports.df$Total.Value...US.) )

exports.df <- exports.df[exports.df$Country != "South/Central America", ]

exports.df$destination <- "Atlantic"

exports.df$destination[exports.df$Country %in% c("Asia", "Australia and Oceania")] <- "Pacific"

JIT.agg <- by(exports.df, INDICES=list(exports.df$Commodity), FUN = function(x) {
  X_PP <- sum( x$Total.Value...US.[x$destination=="Pacific" & x$Port=="Pacific"] )
  X_PA <- sum( x$Total.Value...US.[x$destination=="Pacific" & x$Port=="Atlantic"] )
  X_AP <- sum( x$Total.Value...US.[x$destination=="Atlantic" & x$Port=="Pacific"] )
  X_AA <- sum( x$Total.Value...US.[x$destination=="Atlantic" & x$Port=="Atlantic"] )
  
  X_PP/(X_PP+X_AP) + X_AA/(X_AA + X_PA)
} )


hist(unlist(JIT.agg))

#sort(unlist(JIT.agg))


JIT.agg.weighted <- by(exports.df, INDICES=list(exports.df$Commodity), FUN = function(x) {
  X_PP <- sum( x$Total.Value...US.[x$destination=="Pacific" & x$Port=="Pacific"] )
  X_PA <- sum( x$Total.Value...US.[x$destination=="Pacific" & x$Port=="Atlantic"] )
  X_AP <- sum( x$Total.Value...US.[x$destination=="Atlantic" & x$Port=="Pacific"] )
  X_AA <- sum( x$Total.Value...US.[x$destination=="Atlantic" & x$Port=="Atlantic"] )
  alpha <- (X_PP + X_AP)/(X_PP + X_PA + X_AP + X_AA)
  
  alpha * X_PP/(X_PP+X_AP) + (1 - alpha) * X_AA/(X_AA + X_PA)
} )


hist(unlist(JIT.agg.weighted))

#sort(unlist(JIT.agg))

cor(unlist(JIT.agg.weighted), unlist(JIT.agg), method="kendall", use="pairwise") 
# Hmm. The rank correlation isn't so strong
cor(unlist(JIT.agg.weighted), unlist(JIT.agg), use="pairwise") 

JIT.df <- data.frame(JIT.index=unclass(unlist(JIT.agg)))
JIT.df$Commodity <- names(unclass(unlist(JIT.agg)))
JIT.df$HS.2.digit <- substring(JIT.df$Commodity, 1, 2)
JIT.df$HS.4.digit <- substring(JIT.df$Commodity, 1, 4)
JIT.df$HS.6.digit <- substring(JIT.df$Commodity, 1, 6)

sd(JIT.df$JIT.index, na.rm=TRUE)
#aggregate(JIT.df$JIT.index,  by=list(JIT.df$HS.4.digit), FUN=sd, na.rm=TRUE)

mean(aggregate(JIT.df$JIT.index,  by=list(JIT.df$HS.4.digit), FUN=sd, na.rm=TRUE)[, 2], na.rm=TRUE)

mean(aggregate(JIT.df$JIT.index,  by=list(JIT.df$HS.2.digit), FUN=sd, na.rm=TRUE)[, 2], na.rm=TRUE)


JIT.df <- data.frame(JIT.index=unclass(unlist(JIT.agg.weighted)))
# NOTE: overwriting the orig JIT.df
JIT.df$Commodity <- names(unclass(unlist(JIT.agg)))
JIT.df$HS.2.digit <- substring(JIT.df$Commodity, 1, 2)
JIT.df$HS.4.digit <- substring(JIT.df$Commodity, 1, 4)
JIT.df$HS.6.digit <- substring(JIT.df$Commodity, 1, 6)

sd(JIT.df$JIT.index, na.rm=TRUE)
#aggregate(JIT.df$JIT.index,  by=list(JIT.df$HS.4.digit), FUN=sd, na.rm=TRUE)

mean(aggregate(JIT.df$JIT.index,  by=list(JIT.df$HS.4.digit), FUN=sd, na.rm=TRUE)[, 2], na.rm=TRUE)



hts.descrip.df <- read.csv("/Users/travismcarthur/Downloads/htsdata.csv", stringsAsFactors=FALSE)
# Got this from http://hts.usitc.gov/export

hts.descrip.df <- hts.descrip.df[nchar(hts.descrip.df$HTS.Number)==4, ]

JIT.df <- merge(JIT.df, hts.descrip.df, by.x="HS.4.digit", by.y="HTS.Number", all=TRUE)

JIT.description.df <- aggregate(JIT.df$JIT.index,  by=list(JIT.df$Description), FUN=mean, na.rm=TRUE)


JIT.description.df[ order(JIT.description.df[, 2] ), 2:1]




### Fooling around with Comtrade


# Thanks to http://comtrade.un.org/data/Doc/api/ex/r

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}


# install.packages("rjson")
library("rjson")
# install.packages("rjson")
library("XML")

usa.nz.mex.df <- get.Comtrade(r="842", p="484,554", ps=2010, px="H1", cc="AG6",fmt="csv", rg="1,2")



reporters.df <- fromJSON(file="http://comtrade.un.org/data/cache/partnerAreas.json")
reporters.df <- as.data.frame(matrix(unlist(reporters.df$results), ncol=2, byrow=TRUE), stringsAsFactors=FALSE)

colnames(reporters.df) <- c("code", "country")


commodities.df <- xmlToDataFrame("http://comtrade.un.org/ws/refs/getCommodityList.aspx?px=H1&cc=AG4")
#commodities.df <- as.data.frame(matrix(unlist(commodities.df$results), ncol=2, byrow=TRUE), stringsAsFactors=FALSE)

#colnames(reporters.df) <- c("code", "country")

reporters.df <- reporters.df[ ! reporters.df$country %in% c("All", "World"), ]


# http://unstats.un.org/unsd/tradekb/Knowledgebase/UN-Comtrade-Reference-Tables
# Use rg=2 for exports
# AG4 means the 4-digit HS
# http://comtrade.un.org/data/doc/api/

#set.seed(100)
#reporters.df <- reporters.df[sample(1:nrow(reporters.df)), ]
# Scramble it to get data "balance" between different pulls

#midway.point <- round(nrow(reporters.df))
#first.batch <- paste0(reporters.df$code[1:5], collapse=",")
#second.batch <- paste0(reporters.df$code[(midway.point+1):nrow(reporters.df)], collapse=",")


reporters.grouped <- gsub("(^[,]*)|([,]*$)", "", 
                          apply(
                            matrix(c(reporters.df$code, "", "", "", ""), ncol=5), 
                            1, FUN=paste0, collapse=","))

#usa.nz.mex.df <- get.Comtrade(r="842", p="242,212,450,,,", ps=2000, px="H1", cc="AG4",rg="2")


#usa.nz.mex.df <- get.Comtrade(r="842", p="all", ps=2000, px="H1", cc=paste0(commodities.df$code[1:100], collapse=","),rg="2")





one.hour <- as.difftime(1.05, format = "%X", units = "hours")

start.time <- Sys.time()
num.requests <- 0

test.data.ls <- list()

for ( target.year in 2014:1985) {
  for (target.partners in reporters.grouped) {
    
    test.data.ls[[length(test.data.ls)+1]] <- 
      get.Comtrade(r="842", p=target.partners, ps=target.year, px="H1", cc="AG4",rg="2", fmt="csv")$data
    
    Sys.sleep(1.2)
    num.requests <- num.requests + 1
    cat(num.requests, " ", target.year, " ", target.partners, "\n")
    while (num.requests==90) {
      Sys.sleep(5)
      print(Sys.time())
      if (Sys.time() - start.time > one.hour ) {
        start.time <- Sys.time()
        num.requests <- 0
      }
      
    }
    
  }
}


test.data.df <- do.call(rbind, test.data.ls)

sapply(test.data.ls, FUN=nrow)





# NOW Re-doing the JIT index calcs:

exports.df  <- read.csv( "/Users/travismcarthur/Desktop/RTA project/Data/Pacific-Atlantic ports HS4 digit continents 2003-2014.csv"  ,
                         stringsAsFactors=FALSE, skip=2)



exports.df$HS.2.digit <- substring(exports.df$Commodity, 1, 2)
exports.df$HS.4.digit <- substring(exports.df$Commodity, 1, 4)
#exports.df$HS.6.digit <- substring(exports.df$Commodity, 1, 6)

exports.df$Total.Value...US. <- as.numeric(gsub(",", "", exports.df$Total.Value...US.) )

exports.df <- exports.df[exports.df$Country != "South/Central America", ]

exports.df$destination <- "Atlantic"

exports.df$destination[exports.df$Country %in% c("Asia", "Australia and Oceania")] <- "Pacific"

JIT.agg <- by(exports.df, INDICES=list(exports.df$Commodity), FUN = function(x) {
  X_PP <- sum( x$Total.Value...US.[x$destination=="Pacific" & x$Port=="Pacific"] )
  X_PA <- sum( x$Total.Value...US.[x$destination=="Pacific" & x$Port=="Atlantic"] )
  X_AP <- sum( x$Total.Value...US.[x$destination=="Atlantic" & x$Port=="Pacific"] )
  X_AA <- sum( x$Total.Value...US.[x$destination=="Atlantic" & x$Port=="Atlantic"] )
  
  X_PP/(X_PP+X_AP) + X_AA/(X_AA + X_PA)
} )


hist(unlist(JIT.agg))

21    48                                      Bahrain
27    84                                       Belize (??????)
49   124                                       Canada
54   152                                        Chile
60   170                                     Colombia
64   188                                   Costa Rica
77   214                               Dominican Rep.
82   222                                  El Salvador














# Below is current code to work on:

for ( naics.agg.level in 6:2) {
# naics.agg.level <- 6
aggregation.level <- "naics"
# naics commodity sic


fta.dates.df <- matrix(c(
1989, "1220","Canada","CA",
1994, "2010","Mexico","MX",
2006, "2050","Guatemala","GT",
2006, "2110","El Salvador","SV",
2006, "2150","Honduras","HN",
2006, "2190","Nicaragua","NI",
2009, "2230","Costa Rica","CR",
2012, "2250","Panama","PA",
2007, "2470","Dominican Republic","DO",
2012, "3010","Colombia","CO",
2009, "3330","Peru","PE",
2004, "3370","Chile","CL",
1985, "5081","Israel","IL",
2001, "5110","Jordan","JO",
2006, "5250","Bahrain","BH",
2004, "5590","Singapore","SG",
2012, "5800","South Korea (Republic of Korea)","KR",
2005, "6021","Australia","AU",
2006, "7140","Morocco","MA",
2009, "5230","Oman","OM" ), ncol=4, byrow=TRUE
)

fta.dates.df <- as.data.frame(fta.dates.df)
fta.dates.df <- fta.dates.df[, 1:2]
colnames(fta.dates.df) <- c("fta.start.yr", "cty_code")
fta.dates.df$fta.start.yr <- as.numeric(as.character(fta.dates.df$fta.start.yr))



#install.packages("foreign")
#install.packages("XML")
library("foreign")
library("XML")
#exports.df <- read.dta("/Users/travismcarthur/Desktop/RTA project/Data/Schott US export data/exp_detl_yearly_112n.dta")


country.codes.df <- read.csv("http://www.census.gov/foreign-trade/schedules/c/country3.txt", skip=5, header=FALSE)

names(country.codes.df) <- c("cty_code","country.name","iso2Code")

#port.codes.df <- read.csv("http://www.census.gov/foreign-trade/schedules/d/dist3.txt", skip=6, header=FALSE, stringsAsFactors=FALSE)
#port.codes.df <- port.codes.df[-(527:530),]
#write.csv(port.codes.df[ port.codes.df[,1]!="", ], 
#  file="/Users/travismcarthur/Desktop/RTA project/Data/district code.csv", row.names=FALSE)
# Above was just to help strip out the port codes and be left with just districts.

district.codes.df <- read.csv("/Users/travismcarthur/Desktop/RTA project/Data/district code.csv", stringsAsFactors=FALSE)

naics.codes.df <- read.csv("http://www.census.gov/econ/cbp/download/NAICS2012.txt", stringsAsFactors=FALSE)


#country.data.ls <- list()

#for ( target.country.code in country.codes.df$iso2Code ) {
#  country.data.ls[[target.country.code]]<- 
#    tryCatch(xmlToDataFrame(paste0("http://api.worldbank.org/countries/", target.country.code) ),
#             error=function(e) {"country code not found"} )
#  cat(target.country.code, "\n")
#}

#country.data.ls <- country.data.ls[sapply(country.data.ls, FUN=length) > 1]

#country.data.df <- do.call( rbind, country.data.ls)


country.data.df <- xmlToDataFrame("/Users/travismcarthur/Desktop/RTA project/Data/countries.xml")
# originally from http://data.worldbank.org/developers/api-overview/country-queries , 
# but had problems with text encoding


#download.file(url="http://api.worldbank.org/countries/all/indicators/NY.GDP.MKTP.CD?date=2000&per_page=300", 
#              destfile= "/Users/travismcarthur/Desktop/RTA project/Data/NY.GDP.MKTP.CD secod test.xml")

#str(xmlToDataFrame("/Users/travismcarthur/Desktop/RTA project/Data/NY.GDP.MKTP.CD secod test.xml") )
#str(xmlToDataFrame("http://api.worldbank.org/countries/all/indicators/NY.GDP.MKTP.CD?date=2000&per_page=300") )
# Ok for some reason this is working now instead of being corrupted by some text encoding issue

country.gdp.ls <- list()

for (target.year in as.character(1989:2012)) {
  
  country.gdp.ls[[target.year]] <- xmlToDataFrame(paste0("http://api.worldbank.org/countries/all/indicators/NY.GDP.MKTP.CD?date=", 
      target.year, "&per_page=300"), stringsAsFactors = FALSE )
  
}

country.gdp.df <- do.call(rbind, country.gdp.ls)



#country.gdp.df <- xmlToDataFrame("/Users/travismcarthur/Desktop/RTA project/Data/NY.GDP.MKTP.CD.xml", stringsAsFactors=FALSE)
# Originally from http://api.worldbank.org/countries/all/indicators/NY.GDP.MKTP.CD?date=2000&per_page=300 ,
# but had problems with text encoding
# This is GDP in current (not constant) dollars. Makes sense since trade data is in current $$$.


country.gdp.df$value <- as.numeric(country.gdp.df$value)
country.gdp.df$date <- as.numeric(country.gdp.df$date)
colnames(country.gdp.df)[colnames(country.gdp.df)=="country"] <- "name"
colnames(country.gdp.df)[colnames(country.gdp.df)=="value"] <- "GDP"
colnames(country.gdp.df)[colnames(country.gdp.df)=="date"] <- "year"

country.data.df <- merge(country.data.df, country.gdp.df[, c("name", "year", "GDP")])

country.data.df$longitude <- as.numeric(as.character(country.data.df$longitude))
country.data.df$latitude <- as.numeric(as.character(country.data.df$latitude))

#install.packages("geosphere")
library("geosphere")

country.data.df$gc.dist<- distVincentyEllipsoid(c(-98.583333, 39.833333), as.matrix(country.data.df[, c("longitude", "latitude")]))

#longitude/latitude of point(s), in degrees 1; can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) or a SpatialPoints* object






# https://en.wikipedia.org/wiki/Geographic_center_of_the_contiguous_United_States
# https://www.fcc.gov/encyclopedia/degrees-minutes-seconds-tofrom-decimal-degrees
# -98.583333 + 180 = 81.41667

country.data.df$coast.destination <- "P"


country.data.df$coast.destination[country.data.df$longitude > -98.583333 & country.data.df$longitude < 81.41667] <- "A"

country.data.df$coast.destination[country.data.df$region %in% c("North America", "Latin America & Caribbean (all income levels)")] <- "N"

# country.data.df$region
#[1] "North America"                                  "Europe & Central Asia (all income levels)"     
#[3] "Latin America & Caribbean (all income levels)"  "Middle East & North Africa (all income levels)"
#[5] "South Asia"                                     "East Asia & Pacific (all income levels)"       
#[7] "Sub-Saharan Africa (all income levels)"   

names(district.codes.df)[names(district.codes.df)=="district.code"] <- "district"

country.data.df <- merge( country.codes.df, country.data.df)

country.data.for.agg.df <- country.data.df[, c("cty_code", "country.name", "coast.destination")]
country.data.for.agg.df <- unique(country.data.for.agg.df)

#schott.data.filepaths<- paste0("/Users/travismcarthur/Desktop/RTA project/Data/Schott US export data/exp_detl_yearly_", 
#89:112, "n.dta")

agg.for.JIT.ls <- list()
agg.for.full.dataset.ls <- list()

for (target.year in as.character(89:112)) {
  # target.year <- as.character(100)
  exports.df <- read.dta(
    paste0("/Users/travismcarthur/Desktop/RTA project/Data/Schott US export data/exp_detl_yearly_", 
           target.year, "n.dta")
  )
  
  exports.df <- merge(exports.df, country.data.for.agg.df)
  # NOTE: This will strip out minor territories that are not in WB database
  
  nrow(exports.df)
  exports.df <- merge(exports.df, district.codes.df)
  nrow(exports.df)
  # Should have same # of observations after this merge - ok dropped like 60 obs. Why?
  
  exports.df$naics <- substr(as.character(exports.df$naics), 1, naics.agg.level)
  
  agg.for.JIT.ls[[target.year]] <- aggregate(exports.df$all_val_yr, 
      by=exports.df[, c(aggregation.level, "coast.destination", "district.coast") ], 
      FUN=sum, na.rm=TRUE)
  agg.for.full.dataset.ls[[target.year]] <- aggregate(exports.df$all_val_yr, 
      by=exports.df[, c(aggregation.level, "cty_code", "country.name", "year") ], 
      FUN=sum, na.rm=TRUE)
  cat(target.year, "\n")
}

# NOTE: Check out this:::: "dom_or_for" . seems to be the same as "df" in the newer datasets



rm(exports.df)
agg.for.JIT.df <- do.call(rbind, agg.for.JIT.ls)
rm(agg.for.JIT.ls)
full.df <- do.call(rbind, agg.for.full.dataset.ls)
rm(agg.for.full.dataset.ls)
colnames(agg.for.JIT.df)[colnames(agg.for.JIT.df)=="x"] <- "export.value"
colnames(full.df)[colnames(full.df)=="x"] <- "export.value"


JIT.agg <- by(agg.for.JIT.df, INDICES=list(agg.for.JIT.df[, aggregation.level]), FUN = function(x) {
  X_PP <- sum( x$export.value[x$coast.destination=="P" & x$district.coast=="P"] )
  X_PA <- sum( x$export.value[x$coast.destination=="P" & x$district.coast=="A"] )
  X_AP <- sum( x$export.value[x$coast.destination=="A" & x$district.coast=="P"] )
  X_AA <- sum( x$export.value[x$coast.destination=="A" & x$district.coast=="A"] )
  
  alpha <- (X_PP + X_AP)/(X_PP + X_PA + X_AP + X_AA)
  
#  alpha * X_PP/(X_PP+X_AP) + (1 - alpha) * X_AA/(X_AA + X_PA)
  # NOTE: Using the aggregation above
  
  X_PP/(X_PP+X_AP) + X_AA/(X_AA + X_PA)
} )
# This automatically gets rid of the "N" coasts and destinations. And aggregates all years.


#hist(unlist(JIT.agg))

JIT.df <- data.frame(commodity=names(JIT.agg), jit.preference=unclass(unlist(JIT.agg)), stringsAsFactors = FALSE)

colnames(JIT.df)[1] <- aggregation.level


#[1] "naics"          "cty_code"       "country.name"   "year"          
#[5] "export.value"   "fta"            "jit.preference"




full.df$fta <- 0

for ( i in 1:nrow(fta.dates.df)) {
  full.df$fta[ 
    full.df$year >= fta.dates.df$fta.start.yr[i] & 
      full.df$cty_code==fta.dates.df$cty_code[i]
    ] <- 1
}







bits.df <- read.csv("/Users/travismcarthur/Desktop/RTA project/Data/ExportCountryBits.csv", stringsAsFactors=FALSE)

bits.df$US.partner <- sapply(strsplit(bits.df$Parties, ";"), FUN=function(x) x[!grepl("United States of America", x)] )
bits.df$US.partner  <- gsub("^ *", "", bits.df$US.partner )

bits.df$US.partner[!bits.df$US.partner %in% unique(full.df$country.name)]


bits.df$US.partner[bits.df$US.partner== "Congo, Democratic Republic of the"] <- "Congo, Democratic Republic of the Congo (formerly Zaire)"
bits.df$US.partner[bits.df$US.partner== "Bolivia, Plurinational State of"] <- "Bolivia"
bits.df$US.partner[bits.df$US.partner== "Congo"] <- "Congo, Republic of the Congo"
bits.df$US.partner[bits.df$US.partner== "Russian Federation"] <- "Russia"
bits.df$US.partner[bits.df$US.partner== "Moldova, Republic of"] <- "Moldova (Republic of Moldova)"
  

bits.df$Date.of.entry.into.force <- as.Date(bits.df$Date.of.entry.into.force, "%d/%m/%Y")

bits.df$Year.of.entry.into.force <- as.numeric(format(bits.df$Date.of.entry.into.force, "%Y"))

bits.df <- bits.df[!is.na(bits.df$Year.of.entry.into.force), ]



full.df$bit <- 0

for ( i in 1:nrow(bits.df)) {
  full.df$bit[ 
    as.numeric(as.charcater(full.df$year)) >= bits.df$Year.of.entry.into.force[i] & 
      full.df$country.name==bits.df$US.partner[i]
    ] <- 1
}




naics.scale.df <- read.csv("/Users/travismcarthur/Desktop/RTA project/Data/data for An Alternative Theory of the Plant Size Distribution, with Geography and Intra- and International Trade/sal97_scalefactor.asc", stringsAsFactors = FALSE, header=FALSE)
naics.scale.df <- naics.scale.df[, -1]
names(naics.scale.df) <- c("naics", "scale.factor")


full.df <- merge(full.df, naics.scale.df, all.x=TRUE)
# NOTE: This will only work with 6-digit NAICS for now



full.df <- merge( full.df, JIT.df)

full.df <- merge( full.df, country.data.df[, c("cty_code", "year", "GDP",  "gc.dist")], all.x=TRUE)

full.df$scale.factor <- full.df$scale.factor/100


#save.image(paste0("/Users/travismcarthur/Desktop/RTA project/Data/workspace yearly gdp naics", naics.agg.level,"-digit.Rdata"))
# load(paste0("/Users/travismcarthur/Desktop/RTA project/Data/workspace yearly gdp naics", naics.agg.level,"-digit.Rdata"))
# save.image(paste0("/Users/travismcarthur/Desktop/RTA project/Data/workspace weighted JIT naics", naics.agg.level,"-digit.Rdata"))

}



naics.agg.level <- 6

load(paste0("/Users/travismcarthur/Desktop/RTA project/Data/workspace naics", naics.agg.level,"-digit.Rdata"))



 full.df$year <- factor(full.df$year)
 full.df$commodity <- factor(full.df$commodity)
 full.df$naics <- factor(full.df$naics)

# "As observed above, the current version of plm is capable of working with a regular data.frame without any further transformation, provided that the individual and time indexes are in the first two columns, as in all the example datasets but Wages."
 


summary( test.lm <- lm(log(export.value) ~ I(jit.preference/sd(unique(jit.preference), na.rm=TRUE))  ,  
                data=full.df[ full.df$export.value > 0 
#                   & full.df$cty_code %in% fta.dates.df$cty_code[fta.dates.df$fta.start.yr > 1989 ] 
                &  substr(as.character(full.df$naics), 1, 2) %in% 31:33
  , ])
) 

 
 
summary( test.lm <- lm(log(export.value) ~ (log(gc.dist) + I(log(GDP)-mean(log(GDP), na.rm=TRUE)) + scale.factor + fta + I(jit.preference-mean(jit.preference, na.rm=TRUE)))^2 ,  
                data=full.df[ full.df$export.value > 0 
#                   & full.df$cty_code %in% fta.dates.df$cty_code[fta.dates.df$fta.start.yr > 1989 ] 
                &  substr(as.character(full.df$naics), 1, 2) %in% 31:33
  , ])
) 



# install.packages("lfe")
library("lfe")

#save.image("/Users/travismcarthur/Desktop/RTA project/Data/workspace after merging exports.Rdata")
# load("/Users/travismcarthur/Desktop/RTA project/Data/workspace after merging exports.Rdata")
#save.image("/Users/travismcarthur/Desktop/RTA project/Data/workspace after merging exports - NAICS.Rdata")
 
# (log(gc.dist) + log(GDP) + scale.factor + fta + jit.preference)^2 
# (log(gc.dist) + I(log(GDP)-mean(log(GDP), na.rm=TRUE)) + scale.factor + fta + I(jit.preference-mean(jit.preference, na.rm=TRUE)))^2
# re-centering the vars seems to have no effect on the parameter values
# log(export.value) ~ (log(gc.dist) + log(GDP) + scale.factor + fta + I(jit.preference/sd(unique(jit.preference), na.rm=TRUE)))^2   - log(gc.dist) -  log(GDP)  - scale.factor -  fta -   I(jit.preference/sd(unique(jit.preference), na.rm = TRUE))  -   log(gc.dist):log(GDP) - log(gc.dist):fta     -   log(GDP):fta    - scale.factor:I(jit.preference/sd(unique(jit.preference), na.rm = TRUE))   


# log(export.value) ~ (log(gc.dist) + log(GDP) + scale.factor + fta + I(scale(jit.preference)))^2   - log(gc.dist) -  log(GDP)  - scale.factor -  fta -   I(scale(jit.preference))  -   log(gc.dist):log(GDP) - log(gc.dist):fta     -   log(GDP):fta    - scale.factor:I(scale(jit.preference)) 

# log(export.value) ~  (scale(log(gc.dist)) + scale(log(GDP)) + scale(scale.factor) + fta + I(scale(jit.preference)))^2   - scale(log(gc.dist)) -  scale(log(GDP))  - scale(scale.factor) -  fta -   I(scale(jit.preference))  -   scale(log(gc.dist)):scale(log(GDP)) - scale(log(gc.dist)):fta     -   scale(log(GDP)):fta    - scale(scale.factor):I(scale(jit.preference)) 

# - scale(log(gc.dist), scale = F) -  scale(log(GDP), scale = F)  - scale(scale.factor) -  fta -   I(scale(jit.preference))  -   scale(log(gc.dist), scale = F):scale(log(GDP), scale = F) - scale(log(gc.dist), scale = F):fta     -   scale(log(GDP), scale = F):fta    - scale(scale.factor):I(scale(jit.preference)) 



log.gdp.median <- median(log( unique(full.df[, c("country.name", "year", "GDP")])$GDP), na.rm=TRUE)
log.gc.dist.median <- median(log( unique(full.df[, c("country.name", "gc.dist")])$gc.dist), na.rm=TRUE)

level.gdp.median <- median( unique(full.df[, c("country.name", "year", "GDP")])$GDP, na.rm=TRUE)
level.gc.dist.median <- median( unique(full.df[, c("country.name", "gc.dist")])$gc.dist, na.rm=TRUE)

jit.preference.median <- median( unique(full.df[, c("naics", "jit.preference")])$jit.preference, na.rm=TRUE)
jit.preference.sd <- sd( unique(full.df[, c("naics", "jit.preference")])$jit.preference, na.rm=TRUE)

scale.factor.median <- median( unique(full.df[, c("naics", "scale.factor")])$scale.factor, na.rm=TRUE)
scale.factor.sd <- sd( unique(full.df[, c("naics", "scale.factor")])$scale.factor, na.rm=TRUE)

log.scale.factor.median <- median( log(unique(full.df[, c("naics", "scale.factor")])$scale.factor), na.rm=TRUE)


# TODO: Probably want to calculate the median of jit.preference only from manufacturing


# summary()
#test.x <- runif(100)
#summary(scale(test.x) -  (test.x - mean(test.x))/sd(test.x) )
# So first subtract and then divide

# I(log(GDP) - log.gdp.median)
# I(log(gc.dist) - gc.dist.median)
# I(jit.preference - jit.preference.median)/jit.preference.sd
# I(scale.factor - scale.factor.median)/scale.factor.sd



test.felm <- felm( log(export.value) ~  ( I(log(gc.dist) - log.gc.dist.median)  +  I(log(GDP) - log.gdp.median)  + 
                                            I((scale.factor - scale.factor.median)/scale.factor.sd)  + 
                                            I((jit.preference - jit.preference.median)/jit.preference.sd) + fta )^2     | 
                                  country.name + year + naics   | 0 |  country.name , 
    data=full.df[ full.df$export.value > 0 
#                   & full.df$cty_code %in% fta.dates.df$cty_code[fta.dates.df$fta.start.yr > 1989 ] 
                &  substr(as.character(full.df$naics), 1, 2) %in% 31:33
  , ],
  exactDOF=TRUE) # weights=

# Hmm. Clustered standard errors  + country.name + year 
# Hmm. What does "country.name:year + country.name + year"  do compared to just "country.name:year"? on FE's?


rownames(test.felm$beta) <- gsub("I[(][(][^/]+/",  "", rownames(test.felm$beta))
rownames(test.felm$beta) <- gsub("[.]sd[)]", "", rownames(test.felm$beta))
rownames(test.felm$beta) <- gsub("I[^-]+(-) ", "", rownames(test.felm$beta))
rownames(test.felm$beta) <- gsub("[.]median[)]", "", rownames(test.felm$beta))
rownames(test.felm$coefficients) <- rownames(test.felm$beta)

summary(test.felm)

library("stargazer")

stargazer(test.felm,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/table1.tex",
          no.space=TRUE, single.row = TRUE, align=TRUE,
          title="Manufacturing exports only. FE on country, year, and NAICS classification. Clustered standard errors on country. Variables have been standardized by median and SD, except GDP and distance were not standardized by SD.")






test.felm <- felm( log(export.value) ~  ( I(log(gc.dist) - log.gc.dist.median)  +  I(log(GDP) - log.gdp.median)  + 
                                            I((scale.factor - scale.factor.median)/scale.factor.sd)  + 
                                            I((jit.preference - jit.preference.median)/jit.preference.sd) + fta )^2     | 
                                  country.name:year + naics   | 0 |  country.name , 
    data=full.df[ full.df$export.value > 0 
#                   & full.df$cty_code %in% fta.dates.df$cty_code[fta.dates.df$fta.start.yr > 1989 ] 
                &  substr(as.character(full.df$naics), 1, 2) %in% 31:33
  , ],
  exactDOF=TRUE) # weights=

# Hmm. Clustered standard errors  + country.name + year 
# Hmm. What does "country.name:year + country.name + year"  do compared to just "country.name:year"? on FE's?


rownames(test.felm$beta) <- gsub("I[(][(][^/]+/",  "", rownames(test.felm$beta))
rownames(test.felm$beta) <- gsub("[.]sd[)]", "", rownames(test.felm$beta))
rownames(test.felm$beta) <- gsub("I[^-]+(-) ", "", rownames(test.felm$beta))
rownames(test.felm$beta) <- gsub("[.]median[)]", "", rownames(test.felm$beta))
rownames(test.felm$coefficients) <- rownames(test.felm$beta)

summary(test.felm)

library("stargazer")

stargazer(test.felm,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/table5.tex",
          no.space=TRUE, single.row = TRUE, align=TRUE,
          title="Manufacturing exports only. FE on countryXyear, and NAICS classification. Clustered standard errors on country. Variables have been standardized by median and SD, except GDP and distance were not standardized by SD.")





test.felm <- felm( log(export.value) ~  ( I(log(gc.dist) - log.gc.dist.median)  +  I(log(GDP) - log.gdp.median)  + 
                                            I((scale.factor - scale.factor.median)/scale.factor.sd)  + 
                                            I((jit.preference - jit.preference.median)/jit.preference.sd) + fta )^2     | 
                                  country.name + year   | 0 |  country.name , 
    data=full.df[ full.df$export.value > 0 
#                   & full.df$cty_code %in% fta.dates.df$cty_code[fta.dates.df$fta.start.yr > 1989 ] 
                &  substr(as.character(full.df$naics), 1, 2) %in% 31:33
  , ],
  exactDOF=TRUE) # weights=


rownames(test.felm$beta) <- gsub("I[(][(][^/]+/",  "", rownames(test.felm$beta))
rownames(test.felm$beta) <- gsub("[.]sd[)]", "", rownames(test.felm$beta))
rownames(test.felm$beta) <- gsub("I[^-]+(-) ", "", rownames(test.felm$beta))
rownames(test.felm$beta) <- gsub("[.]median[)]", "", rownames(test.felm$beta))
rownames(test.felm$coefficients) <- rownames(test.felm$beta)

summary(test.felm)

library("stargazer")

stargazer(test.felm,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/table4.tex",
          no.space=TRUE, single.row = TRUE, align=TRUE,
          title="Manufacturing exports only. FE on country and year, but NOT NAICS classification. Clustered standard errors on countries. Variables have been standardized by median and SD, except GDP and distance were not standardized by SD.")







summary( test.lm <- lm(log(export.value) ~ ( I(log(gc.dist) - log.gc.dist.median)  +  I(log(GDP) - log.gdp.median)  + 
                                            I((scale.factor - scale.factor.median)/scale.factor.sd)  + 
                                            I((jit.preference - jit.preference.median)/jit.preference.sd) + fta )^2  ,  
                data=full.df[ full.df$export.value > 0 
#                   & full.df$cty_code %in% fta.dates.df$cty_code[fta.dates.df$fta.start.yr > 1989 ] 
                &  substr(as.character(full.df$naics), 1, 2) %in% 31:33
  , ])
) 


names(test.lm$coefficients) <- gsub("I[(][(][^/]+/", "", names(test.lm$coefficients))
names(test.lm$coefficients) <- gsub("[.]sd[)]", "", names(test.lm$coefficients))
names(test.lm$coefficients) <- gsub("I[^-]+(-) ", "", names(test.lm$coefficients))
names(test.lm$coefficients) <- gsub("[.]median[)]", "", names(test.lm$coefficients))

stargazer(test.lm ,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/table2.tex",
          no.space=TRUE, single.row = TRUE, align=TRUE,
          title="Manufacturing exports only. No FE or clustered standard errors. Variables have been standardized by median and SD, except GDP and distance were not standardized by SD.")

 
summary( test.lm <- lm(log(export.value) ~ ( I(log(gc.dist) - log.gc.dist.median)  +  I(log(GDP) - log.gdp.median)  + 
                                            I((scale.factor - scale.factor.median)/scale.factor.sd)  + 
                                            I((jit.preference - jit.preference.median)/jit.preference.sd) + fta ) ,  
                data=full.df[ full.df$export.value > 0 
#                   & full.df$cty_code %in% fta.dates.df$cty_code[fta.dates.df$fta.start.yr > 1989 ] 
                &  substr(as.character(full.df$naics), 1, 2) %in% 31:33
  , ])
) 

names(test.lm$coefficients) <- gsub("I[(][(][^/]+/", "", names(test.lm$coefficients))
names(test.lm$coefficients) <- gsub("[.]sd[)]", "", names(test.lm$coefficients))
names(test.lm$coefficients) <- gsub("I[^-]+(-) ", "", names(test.lm$coefficients))
names(test.lm$coefficients) <- gsub("[.]median[)]", "", names(test.lm$coefficients))

stargazer(test.lm ,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/table3.tex",
          no.space=TRUE, single.row = TRUE, align=TRUE,
          title="Manufacturing exports only. No FE or clustered standard errors. No interactions. Variables have been standardized by median and SD, except GDP and distance were not standardized by SD.")

 








jit.quantiles<- quantile(unique(full.df$jit.preference[substr(as.character(full.df$naics), 1, 2) %in% 31:33]), probs=c(.1, .5, .9), na.rm=TRUE)
# So we are taking the "unweighted quantile" - i.e. each industry only appears once when calculating the quantile

num.points <- 100

for ( targ.quantile in 1:3) {

  GDP.range <- range(log(full.df$GDP), na.rm=TRUE)
  GDP.range <- seq(from= GDP.range[1], to=GDP.range[2], length.out = num.points )
  y.value <- test.felm$coef["log(GDP):jit.preference", ] * jit.quantiles[targ.quantile] * GDP.range
  if (targ.quantile ==1 ) { 
    plot(x=GDP.range, y=y.value, type="l", ylim=c(min(y.value)-10, max(y.value)))
  } else {
    lines(x=GDP.range, y=y.value)
  }
  
}
# I'm not sure that the above is useful. This was suggested at https://www3.nd.edu/~rwilliam/xsoc63993/l55.pdf


gdp.quantiles<- quantile(unique(log(full.df$GDP[substr(as.character(full.df$naics), 1, 2) %in% 31:33])), probs=c(.1, .5, .9), na.rm=TRUE)

gdp.quantiles * test.felm$coef["log(GDP):I(jit.preference/sd(unique(jit.preference), na.rm = TRUE))", ] 

# gdp.quantiles * test.felm$coef["log(GDP):jit.preference", ] * sd(full.df$jit.preference, na.rm=TRUE)
# Ok, now the above should have been _divided by_ the sd, not multipled by it, I believe


lapply(full.df, FUN=sd, na.rm=TRUE)



predict(test.felm, newdata=produce.mean.df(full.df), use.fe = FALSE)

str(predict(test.felm, newdata=full.df, use.fe = FALSE))

- log(gc.dist) -  log(GDP)  - scale.factor -  fta -   I(jit.preference/sd(unique(jit.preference), na.rm = TRUE))  -   log(gc.dist):log(GDP) - log(gc.dist):fta     -   log(GDP):fta    - scale.factor:I(jit.preference/sd(unique(jit.preference), na.rm = TRUE))   


# install.packages("stargazer")
library("stargazer")

#stargazer(test.felm,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/table1.tex",
#          no.space=TRUE, single.row = TRUE, align=TRUE,
#          title="Manufacturing exports only. FE on country, year, and NAICS classification. Clustered standard errors on same")



lapply(full.df, FUN=sd, na.rm=TRUE)
sd(log(full.df$GDP), na.rm=TRUE)

summary(test.felm, robust=TRUE)

+ fta:I(jit.preference^2)
+ fta:I(jit.preference^3)
(fta + jit.preference + log(GDP))^2  

country.name + year + naics 
+ naics
country.name
*log(GDP)  + log(gc.dist)
(fta + jit.preference + log(GDP) )^2
 (bit + fta + jit.preference + log(GDP))^2 + fta*jit.preference*log(GDP) 
 - log(GDP):jit.preference


# cor.test(log(full.df$GDP), full.df$jit.preference, use="pair") 
# The correlation is very low

# A couple of countries, notably Iraq, Afghanistan, and Taiwan, do not have GDP data and therefore are dropped


system.time( test.felm <- felm( log(export.value) ~ year:jit.preference   | 
                                  country.name  | 0 | country.name , 
                                data=full.df[ full.df$export.value > 0 & 
                                                #                    full.df$cty_code %in% fta.dates.df$cty_code[fta.dates.df$fta.start.yr > 1989 ] &
                                                substr(as.character(full.df$naics), 1, 2) %in% 31:33, ]) # weights=
)

# Hmm. Clustered standard errors

summary(test.felm)




getfe(test.felm)


with(full.df[full.df$export.value > 0 & substr(as.character(full.df$naics), 1, 2) %in% 31:33, ], cor(log(export.value), jit.preference, use="pairwise.complete.obs") )



with(full.df[full.df$export.value > 0 & substr(as.character(full.df$naics), 1, 2) %in% 31:33, ], 
     plot(log(export.value), jit.preference) )





# "Clustering in the Linear Model"
# http://kurt.schmidheiny.name/teaching/panel.pdf
# http://cameron.econ.ucdavis.edu/research/Cameron_Miller_Cluster_Robust_October152013.pdf



index.range <- seq(.8, 2, .05)
plot(index.range, index.range * 5.5763 + index.range^2 * -1.8875)
# 9.74778  , -3.16567
5.5763
-1.8875

*jit.preference

# NAICS 31-33 is manufacturing

x1 + x2 | f1 + f2 | (Q|W ~ x3+x4) | clu1 + clu2


fta*as.factor(naics) 

*jit.preference 


& full.df$cty_code %in% fta.dates.df$cty_code

fta*jit.preference + fta*I(jit.preference^2)  + G(country.name)





+ fta*I(jit.preference^2) 


+ G(commodity) G(year) +  + G(country.name) + G(year)



system.time(summary(felm(log(y) ~ x1 + x2 +x3 +x4 + x5 + x6 + x7 +x8 + x9 + x10 + G(id)+G(sitq),
                         data=EMP, cluster=c("STATE_FIPS"))))


table(full.df$fta[full.df$cty_code==2050])


"commodity"      "cty_code"       "country.name"   "year"




full.df <- full.df[, c(
  "country.name", "year", "commodity", "cty_code", "export.value", "fta", "jit.preference"
)]

#install.packages("plm")
library("plm")

test.plm <- plm( export.value ~ fta*jit.preference, data=full.df, model="within")






# Delete below:

full.dataset.ls <- list()

for (target.year in as.character(89:112)) {
  
  full.dataset.ls[[target.year]] <- read.dta(
    paste0("/Users/travismcarthur/Desktop/RTA project/Data/Schott US export data/exp_detl_yearly_", 
           target.year, "n.dta")
  )
  full.dataset.ls[[target.year]] <- aggregate(full.dataset.ls[[target.year]], 
            by=exports.df[, "c("commodity", "coast.destination", "district.coast")" ], 
            FUN=sum, na.rm=TRUE)
  cat(target.year, "\n")
}

full.dataset.df <- do.call(rbind, full.dataset.ls)
rm(full.dataset.ls)








colnames(fta.dates.df) <- c("fta.start.yr" "cty_code")










exports.df
district
cty_code


















all_val_yr

exports.df













library("MASS")

test.correlated.data <- mvrnorm(n = 100000, mu=c(0,0,0), Sigma=matrix(c(1, .5, .5,
                                              .5, 1, .5,
                                              .5, .5, 1), ncol=3, byrow=TRUE) )



test.correlated.data  <- as.data.frame(test.correlated.data )
test.correlated.data  <- as.data.frame(lapply(test.correlated.data, FUN=cut, breaks=50))
test.correlated.data  <- as.data.frame(lapply(test.correlated.data, FUN=as.numeric))


summary( lm(V1 ~ V2*V3 , data=test.correlated.data ) )
summary( lm(V1 ~ V2*V3 + factor(V2) + factor(V3) , data=test.correlated.data ) )
# Ok, so even when data is correlated among the three variables, the interaction term has 
# a zero coefficient ( or at least it is not statistically different from zero)
# Either specification above works, BTW




