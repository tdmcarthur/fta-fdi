


produce.mean.df <- function(x) {
  mode.vars<- sapply(x, FUN=function(x) {is.factor(x) | is.character(x) } )
  new.data.ls <- list()
  
  for( i in 1:ncol(x)) {
    if (mode.vars[i]) {
      new.data.ls[[i]] <- Mode(x[, i])
    } else {
      new.data.ls[[i]] <- median(x[, i], na.rm=TRUE)
      # NOW TAKING MEDIAN
    }
  }
  ret <- do.call(data.frame, new.data.ls)
  colnames(ret) <- colnames(x)
  ret
}









# Thanks to https://github.com/skranz/regtools/blob/master/R/felm.r for below
# install.packages("restorepoint")
library("restorepoint")

predict.felm = function(object, newdata, use.fe = TRUE,...) {
  restore.point("predict.felm")  
  co = coef(object)
  
  rownames(newdata) = seq_along(newdata[,1])
  # model matrix without intercept
  mm = model.matrix(object = object$terms, data = newdata)[1, , drop=FALSE] 

  
  if (NROW(mm)<NROW(newdata)) {
    warning("Observations dropped from newdata due to NA.")
  }
  
  # remove intercept
  if (NCOL(mm)==length(co)+1) {
    mm = mm[,-1, drop=FALSE]

  }
#  print(mm)
#  print(co)
  
  mm <- mm[, !is.na(co), drop=FALSE]
  co <- co[!is.na(co)]
  
  y.pred = mm %*% co
#  print(y.pred)
  
  fe.vars = names(object$fe)
  if (use.fe & length(fe.vars)>0) {
    if (!all(fe.vars %in% colnames(mm))) {
      missing = setdiff(fe.vars, colnames(mm))
      warning("Cannot use fixed effects for ", paste0(missing, collapse=", "), " in prediction since no data is given.")
      fe.vars = setdiff(fe.vars, missing)
    }

    rows = as.integer(rownames(mm))
    nd = newdata[rows,]
    all.fe = getfe(object)
    fe.var = fe.vars[1]
    for (fe.var in fe.vars) {
      df = all.fe[all.fe$fe == fe.var,]
      frows = match(nd[[fe.var]],df$idx)    
      myfe = df$effect[frows]
      myfe[is.na(myfe)] = 0
      
      y.pred = y.pred + myfe
    }
  }
  y.pred
}






###################
###################
###################



# meaned.full.df <- produce.mean.df(full.df)

log(mean(full.df$GDP, na.rm=TRUE))
mean(log(full.df$GDP), na.rm=TRUE)
# So we do face Jensen's Inequality a  bit




epsilon <- 10^-4

meaned.full.df$fta <- 0
meaned.full.df$bit <- 0

meaned.full.df$GDP <- quantile(full.df$GDP, probs=c(.25), na.rm=TRUE)

meaned.full.df.dif <- meaned.full.df
meaned.full.df.dif$jit.preference <- meaned.full.df.dif$jit.preference + epsilon

(predict.felm(object=test.felm, newdata=rbind(meaned.full.df.dif,full.df), use.fe = FALSE) - 
  predict.felm(object=test.felm, newdata=rbind(meaned.full.df,full.df), use.fe = FALSE) ) /
  epsilon



str( model.matrix(object= log(export.value) ~ (log(gc.dist) + log(GDP) + scale.factor + fta + I(jit.preference/sd(unique(jit.preference), na.rm=TRUE)))^2   - log(gc.dist) -  log(GDP)  - scale.factor -  fta -   I(jit.preference/sd(unique(jit.preference), na.rm = TRUE))  -   log(gc.dist):log(GDP) - log(gc.dist):fta     -   log(GDP):fta    - scale.factor:I(jit.preference/sd(unique(jit.preference), na.rm = TRUE)), data=rbind(meaned.full.df, full.df))[1, , drop=FALSE] 
)
     
     )

model.matrix(object=test.felm , data=rbind(meaned.full.df, full.df))[1, , drop=FALSE] 

# Aha I figured out why the model matrix was throwing an error before. I took the SD within the model formula, so it reported
# the SD as NA and then the whole thing became NA

summary(with(full.df, jit.preference/sd(unique(jit.preference), na.rm=TRUE))
        
with(full.df, cor(jit.preference, log(gc.dist), use="pairwise"))

summary(with(full.df, scale(jit.preference)))



###################
###################
###################






# Arguments:
# The quantiles of GDP to calculate
# The formula for the model
# Maybe the datset








interaction.effects.quantiles <- function(gdp.quantiles, formula, quantile.var, change.var) {
  

  results.felm <- felm( formula , 
    data=full.df[ full.df$export.value > 0 
#                   & full.df$cty_code %in% fta.dates.df$cty_code[fta.dates.df$fta.start.yr > 1989 ] 
                &  substr(as.character(full.df$naics), 1, 2) %in% 31:33
  , ],
  exactDOF=TRUE) # weights=
  
  rownames(results.felm$beta) <- gsub("I[(][(][^/]+/",  "", rownames(results.felm$beta))
  rownames(results.felm$beta) <- gsub("[.]sd[)]", "", rownames(results.felm$beta))
  rownames(results.felm$beta) <- gsub("I[^-]+(-) ", "", rownames(results.felm$beta))
  rownames(results.felm$beta) <- gsub("[.]median[)]", "", rownames(results.felm$beta))
  rownames(results.felm$coefficients) <- rownames(results.felm$beta)
  # These changes to the coef names here probably have no effects

  
  #meaned.full.df <- produce.mean.df(full.df)
  
  #log.gdp.median <- median(log( unique(full.df[, c("country.name", "year", "GDP")])$GDP), na.rm=TRUE)
  #log.gc.dist.median <- median(log( unique(full.df[, c("country.name", "gc.dist")])$gc.dist), na.rm=TRUE)
  # Don't need to use above since Jensen's ineq does not affect median calculations
   level.gdp.median <- median( unique(full.df[, c("country.name", "year", "GDP")])$GDP, na.rm=TRUE)
   level.gc.dist.median <- median( unique(full.df[, c("country.name", "gc.dist")])$gc.dist, na.rm=TRUE)

  jit.preference.median <- median( unique(full.df[, c("naics", "jit.preference")])$jit.preference, na.rm=TRUE)
  scale.factor.median <- median( unique(full.df[, c("naics", "scale.factor")])$scale.factor, na.rm=TRUE)
  log.scale.factor.median <- log( median( unique(full.df[, c("naics", "scale.factor")])$scale.factor, na.rm=TRUE) )
  
  meaned.full.df <- data.frame(GDP = level.gdp.median, gc.dist = level.gc.dist.median, 
             jit.preference = jit.preference.median, scale.factor = scale.factor.median,
             fta=0, bit=0, log.scale.factor = log.scale.factor.median )
  

  epsilon <- 10^-4
  if (change.var == "GDP") epsilon <- 100
  if (change.var == "gc.dist") epsilon <- 1
    

  #meaned.full.df$fta <- 0
  #meaned.full.df$bit <- 0
  #log.gc.dist.median
  #log.gdp.median
  #scale.factor.median
  #jit.preference.median
  

  
  quantile.output.v <- c()
  
  for ( target.quantile in gdp.quantiles) {
    
  if (quantile.var=="GDP") {
    meaned.full.df$GDP <- quantile(unique(full.df[, c("country.name", "year", "GDP")])$GDP,
                                 probs = target.quantile, na.rm=TRUE)
  }
    
  if (quantile.var=="gc.dist") {
    meaned.full.df$gc.dist <- quantile(unique(full.df[, c("country.name", "gc.dist")])$gc.dist,
                                 probs = target.quantile, na.rm=TRUE)
  }
  print(quantile.var)
  if (quantile.var=="jit.preference") {
    meaned.full.df$jit.preference <- quantile(unique(full.df[, c("naics", "jit.preference")])$jit.preference,
                                 probs = target.quantile, na.rm=TRUE)
  }

  if (quantile.var=="scale.factor") {
    meaned.full.df$scale.factor <- quantile(unique(full.df[, c("naics", "scale.factor")])$scale.factor,
                                 probs = target.quantile, na.rm=TRUE)
  }
  
  if (quantile.var=="log.scale.factor") {
    meaned.full.df$log.scale.factor <- quantile(unique(full.df[, c("naics", "log.scale.factor")])$log.scale.factor,
                                 probs = target.quantile, na.rm=TRUE)
  }

  meaned.full.df.dif <- meaned.full.df
  meaned.full.df.dif[, change.var] <- meaned.full.df.dif[, change.var] + epsilon
  
  if ( change.var == "fta")  {meaned.full.df.dif$fta <- 1; epsilon <- 1}
  if ( change.var == "bit")  {meaned.full.df.dif$bit <- 1; epsilon <- 1}

  quantile.output.v[as.character(target.quantile)] <- 
    (predict.felm(object=results.felm, newdata=meaned.full.df.dif, use.fe = FALSE) - 
    predict.felm(object=results.felm, newdata=meaned.full.df, use.fe = FALSE) )  /
    epsilon
  
  }
  
  quantile.output.v
  
}

# Hmm. I think this is correct - I want the effect of 1 SD change, not one unit change, but I think the fact
# that the prediction takes into account the median and SD transformation does give the intended result.






output.reg.table <- function(formula, output.file, table.title, file.or.object) {
  test.felm <- felm(  formula,  
    data=full.df[ 
   # full.df$export.value > 0 
   #                   & full.df$cty_code %in% fta.dates.df$cty_code[fta.dates.df$fta.start.yr > 1989 ] 
   #             &
                  substr(as.character(full.df$naics), 1, 2) %in% 31:33
  , ],
  exactDOF=TRUE) # weights=


  rownames(test.felm$beta) <- gsub("I[(][(][^/]+/",  "", rownames(test.felm$beta))
  rownames(test.felm$beta) <- gsub("[.]sd[)]", "", rownames(test.felm$beta))
  rownames(test.felm$beta) <- gsub("I[^-]+(-) ", "", rownames(test.felm$beta))
  rownames(test.felm$beta) <- gsub("[.]median[)]", "", rownames(test.felm$beta))
  rownames(test.felm$coefficients) <- rownames(test.felm$beta)
  
#  test.felm$clustervcv[test.felm$clustervcv==0] <- NA
#  test.felm$robustvcv[test.felm$robustvcv==0] <- NA

  if (file.or.object=="file") {
    stargazer(test.felm,  out=paste0("/Users/travismcarthur/Desktop/RTA project/Preliminary results/", output.file),
          no.space=TRUE, single.row = TRUE, align=TRUE,
          dep.var.labels=as.character(formula)[2],
          title=table.title,
          add.lines=list(c("Adj. R$^{2}$ (proj model)", round(summary(results.felm)$P.adj.r.squared, 3) ) ) #,
#          report= c("vc*sp")
          )
    return(NULL)
  }
  
  if (file.or.object=="object") {
    return( summary(test.felm) )
  }
}


#paste0("Adj. R$^{2}$ (proj model) & \\multicolumn{1}{c}{", 
#            round(summary(results.felm)$P.adj.r.squared, 3), "}" )




# full.df$log.scale.factor <- log(full.df$scale.factor))




input.formula <- as.formula( log(export.value) ~  ( I(log.scale.factor - log.scale.factor.median)  + 
                                            I((jit.preference - jit.preference.median)/jit.preference.sd) + fta +
                                           I(log(gc.dist) - log.gc.dist.median)  +  I(log(GDP) - log.gdp.median) )^2     | 
                                  country.name + year + naics   | 0 |  country.name  )


output.reg.table(input.formula , "table1.tex", 
                "MODEL 1: FTA effect. Manufacturing exports only. FE on country, year, and NAICS classification. Clustered standard errors on country. Variables have been standardized by median and SD, except GDP and distance were not standardized by SD.", file.or.object="file") 



#int.eff.results.4 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "gc.dist", "jit.preference")
#int.eff.results.4 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "GDP", "fta")
#int.eff.results.5 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "jit.preference", "fta")
#int.eff.results.6 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "jit.preference", "bit")
# int.eff.results.2 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "GDP", "jit.preference")
#int.eff.results.2 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "jit.preference", "GDP")
#int.eff.results.3 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "GDP", "scale.factor")




int.eff.results.1 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "GDP", "jit.preference")
int.eff.results.2 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "GDP", "log.scale.factor")

int.eff.table.1 <- as.data.frame(matrix(c(int.eff.results.1, int.eff.results.2),   nrow=11, byrow=FALSE ) )
colnames(int.eff.table.1 ) <- c("jit.preference", "log.scale.factor")
rownames(int.eff.table.1 ) <- paste0((0:10)*10, "%")

stargazer(int.eff.table.1,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/interact-table1.tex",
          no.space=TRUE, single.row = TRUE, align=TRUE, summary=FALSE,
          title="MODEL 1: Marginal effect of one standard deviation change of JIT preference and one log point change in Scale Factor upon the log value of trade flow, conditional on different levels of partner GDP, by quantiles.")



int.eff.results.1 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "gc.dist", "jit.preference")
int.eff.results.2 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "gc.dist", "log.scale.factor")

int.eff.table.1 <- as.data.frame(matrix(c(int.eff.results.1, int.eff.results.2),   nrow=11, byrow=FALSE ) )
colnames(int.eff.table.1 ) <- c("jit.preference", "log.scale.factor")
rownames(int.eff.table.1 ) <- paste0((0:10)*10, "%")

stargazer(int.eff.table.1,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/interact-table2.tex",
          no.space=TRUE, single.row = TRUE, align=TRUE, summary=FALSE,
          title="MODEL 1: Marginal effect of one standard deviation change of JIT preference and one log point change in Scale Factor upon the log value of trade flow, conditional on different levels of partner distance, by quantiles.")


int.eff.results.1 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "jit.preference", "fta")

int.eff.table.1 <- as.data.frame(matrix(int.eff.results.1,   nrow=11, byrow=FALSE ) )
colnames(int.eff.table.1 ) <- c("FTA")
rownames(int.eff.table.1 ) <- paste0((0:10)*10, "%")

stargazer(int.eff.table.1,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/interact-table3.tex",
          no.space=TRUE, single.row = TRUE, align=TRUE, summary=FALSE,
          title="MODEL 1: Marginal effect of enacting an FTA upon the log value of trade flow, conditional on different levels of industry JIT Preference, by quantiles.")


int.eff.results.1 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "log.scale.factor", "fta")

int.eff.table.1 <- as.data.frame(matrix(int.eff.results.1,   nrow=11, byrow=FALSE ) )
colnames(int.eff.table.1 ) <- c("FTA")
rownames(int.eff.table.1 ) <- paste0((0:10)*10, "%")

stargazer(int.eff.table.1,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/interact-table4.tex",
          no.space=TRUE, single.row = TRUE, align=TRUE, summary=FALSE,
          title="MODEL 1: Marginal effect of enacting an FTA upon the log value of trade flow, conditional on different levels of industry Scale Factor, by quantiles.")





####################################################
###### START NEXT MODEL
####################################################



input.formula <- as.formula( log(export.value) ~  ( I(log.scale.factor - log.scale.factor.median)  + 
                                            I((jit.preference - jit.preference.median)/jit.preference.sd) + bit +
                                           I(log(gc.dist) - log.gc.dist.median)  +  I(log(GDP) - log.gdp.median) )^2     | 
                                  country.name + year + naics   | 0 |  country.name )

output.reg.table(input.formula , "bit-no-cross-cnty-year.tex", 
                "MODEL 2: BIT effect. Manufacturing exports only. FE on country, year, and NAICS classification. Clustered standard errors on country. Variables have been standardized by median and SD, except GDP and distance were not standardized by SD.", file.or.object="file") 




int.eff.results.1 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "GDP", "jit.preference")
int.eff.results.2 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "GDP", "log.scale.factor")

int.eff.table.1 <- as.data.frame(matrix(c(int.eff.results.1, int.eff.results.2),   nrow=11, byrow=FALSE ) )
colnames(int.eff.table.1 ) <- c("jit.preference", "log.scale.factor")
rownames(int.eff.table.1 ) <- paste0((0:10)*10, "%")

stargazer(int.eff.table.1,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/model-2-interact-table1.tex",
          no.space=TRUE, single.row = TRUE, align=TRUE, summary=FALSE,
          title="MODEL 2: Marginal effect of one standard deviation change of JIT preference and one log point change in Scale Factor upon the log value of trade flow, conditional on different levels of partner GDP, by quantiles.")




int.eff.results.1 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "gc.dist", "jit.preference")
int.eff.results.2 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "gc.dist", "log.scale.factor")

int.eff.table.1 <- as.data.frame(matrix(c(int.eff.results.1, int.eff.results.2),   nrow=11, byrow=FALSE ) )
colnames(int.eff.table.1 ) <- c("jit.preference", "log.scale.factor")
rownames(int.eff.table.1 ) <- paste0((0:10)*10, "%")

stargazer(int.eff.table.1,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/model-2-interact-table2.tex",
          no.space=TRUE, single.row = TRUE, align=TRUE, summary=FALSE,
          title="MODEL 2: Marginal effect of one standard deviation change of JIT preference and one log point change in Scale Factor upon the log value of trade flow, conditional on different levels of partner distance, by quantiles.")


int.eff.results.1 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "jit.preference", "bit")

int.eff.table.1 <- as.data.frame(matrix(int.eff.results.1,   nrow=11, byrow=FALSE ) )
colnames(int.eff.table.1 ) <- c("BIT")
rownames(int.eff.table.1 ) <- paste0((0:10)*10, "%")

stargazer(int.eff.table.1,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/model-2-interact-table3.tex",
          no.space=TRUE, single.row = TRUE, align=TRUE, summary=FALSE,
          title="MODEL 2: Marginal effect of enacting a BIT upon the log value of trade flow, conditional on different levels of industry JIT Preference, by quantiles.")


int.eff.results.1 <- interaction.effects.quantiles(seq(0, 1, by=.1), input.formula, "log.scale.factor", "bit")

int.eff.table.1 <- as.data.frame(matrix(int.eff.results.1,   nrow=11, byrow=FALSE ) )
colnames(int.eff.table.1 ) <- c("BIT")
rownames(int.eff.table.1 ) <- paste0((0:10)*10, "%")

stargazer(int.eff.table.1,  out="/Users/travismcarthur/Desktop/RTA project/Preliminary results/model-2-interact-table4.tex",
          no.space=TRUE, single.row = TRUE, align=TRUE, summary=FALSE,
          title="MODEL 2: Marginal effect of enacting a BIT upon the log value of trade flow, conditional on different levels of industry Scale Factor, by quantiles.")


















input.formula <- as.formula( log(export.value) ~  ( I(log(gc.dist) - log.gc.dist.median)  +  I(log(GDP) - log.gdp.median)  + 
                                            poly(I((scale.factor - scale.factor.median)/scale.factor.sd), 2, raw=TRUE)  + 
                                            poly(I((jit.preference - jit.preference.median)/jit.preference.sd), 2, raw=TRUE) + fta )^2     | 
                                  country.name + year + naics  | 0 |  country.name )


input.formula <- as.formula( log(export.value) ~  ( I(log(gc.dist) - log.gc.dist.median)  +  I(log(GDP) - log.gdp.median)  + 
                                            I((scale.factor - scale.factor.median)/scale.factor.sd)  + 
                                            I((jit.preference - jit.preference.median)/jit.preference.sd) + fta  )^2     | 
                                  country.name + year + naics  | 0 |  country.name )


jit.preference.median <- median( unique(full.df[, c("commodity", "jit.preference")])$jit.preference, na.rm=TRUE)
jit.preference.sd <- sd( unique(full.df[, c("commodity", "jit.preference")])$jit.preference, na.rm=TRUE)

input.formula <- as.formula( log(export.value) ~  ( I(log(gc.dist) - log.gc.dist.median)  +  I(log(GDP) - log.gdp.median)  + 
                                            I((jit.preference - jit.preference.median)/jit.preference.sd) + fta  )^2     | 
                                  country.name + year + commodity  | 0 |  country.name )

input.formula <- as.formula( log(export.value) ~  ( I(log(gc.dist) - log.gc.dist.median)  +  I(log(GDP) - log.gdp.median)  + 
                                            I((scale.factor - scale.factor.median)/scale.factor.sd) + fta  )^2     | 
                                  country.name + year + naics  | 0 |  country.name )

input.formula <- as.formula( log(export.value) ~  ( I((scale.factor - scale.factor.median)/scale.factor.sd) + fta  )^2     | 
                                  country.name + year + naics  | 0 |  country.name )

input.formula <- as.formula( log(export.value) ~  ( I((jit.preference - jit.preference.median)/jit.preference.sd) + fta  )^2     | 
                                  country.name + year + naics  | 0 |  country.name )

input.formula <- as.formula( log(export.value) ~  ( I(log(gc.dist) - log.gc.dist.median)  +  I(log(GDP) - log.gdp.median)  + 
                                            log(scale.factor)  + 
                                            jit.preference + bit  )^2     | 
                                  country.name:year + naics  | 0 |  country.name )

output.reg.table(input.formula, output.file="", table.title="", file.or.object="object")



# Note that the detected (interaction) effect for log.gc.dist is much lower than that for GDP because the range of log.gc.dist
# is much lower than that for GDP

  results.felm <- felm( input.formula , 
    data=full.df[ full.df$export.value > 0 
#                   & full.df$cty_code %in% fta.dates.df$cty_code[fta.dates.df$fta.start.yr > 1989 ] 
                &  substr(as.character(full.df$naics), 1, 2) %in% 31:33
  , ],
  exactDOF=TRUE) # weights=


summary( results.felm)

results.felm$clustervcv[results.felm$clustervcv==0]


full.df <- merge(full.df, unique(country.data.df[, c("cty_code", "coast.destination")]), all.x=TRUE)

destination.agg <- aggregate(export.value ~  coast.destination + naics, data=full.df, FUN=sum, na.rm=TRUE)

destination.prop.list <- list()

for ( i in unique(destination.agg$naics)) {
  destination.prop.list[[i]]<- destination.agg[destination.agg$naics==i & destination.agg$coast.destination=="A", "export.value"] /
    sum(destination.agg[destination.agg$naics==i & destination.agg$coast.destination %in% c("A", "P"), "export.value"], na.rm=TRUE)
}

destination.prop.df <- merge( data.frame(naics=names(destination.prop.list), prop.dest.EU=unlist(destination.prop.list)),
       unique(full.df[, c("naics", "jit.preference")] ) )

cor.test(~ prop.dest.EU + jit.preference, data = destination.prop.df)    
# Small corr; not stat signif.


cor.test( ~ jit.preference + scale.factor, 
  data=unique(full.df[, c("naics", "jit.preference", "scale.factor")])[, c("jit.preference", "scale.factor")])

cor.test( ~ jit.preference + scale.factor, 
  data=unique(full.df[, c("naics", "jit.preference", "scale.factor")])[, c("jit.preference", "scale.factor")])


test.set <- runif(1000)
median(log(test.set))
log(median(test.set))

cor.test(~ log(export.value) + I(jit.preference*log(GDP))  , data=full.df[full.df$export.value>0, ])
cor.test(~ log(export.value) + I(jit.preference*log(GDP)) + jit.preference + log(GDP)  , data=full.df[full.df$export.value>0, ])

cor(~ log(export.value) + I(jit.preference*log(GDP)) + jit.preference + log(GDP)  , data=full.df[full.df$export.value>0, ])

summary(lm( log(export.value) ~ jit.preference*log(GDP)  , data=full.df[full.df$export.value>0, ]) )
summary(lm( log(export.value) ~ jit.preference + log(GDP)  , data=full.df[full.df$export.value>0, ]) )


# save.image("/Users/travismcarthur/Desktop/RTA project/Data/workspace HS yearly GDP.Rdata" )


## NOTE TO SELF: I think the difference between country.name:year  and country.name:year  + country.name + year is just that it
# sort of miscalculates the degrees of freedom (thinks there are more params in the second specification)
# and therefore has slightly different standard errors and adjusted R-squareds





max.possible.panel.observations <- length(unique(full.df$year)) * length(unique(full.df$country.name)) * 
  length(unique( full.df$naics[substr(as.character(full.df$naics), 1, 2) %in% 31:33] ))


results.felm$N / max.possible.panel.observations
# Above is 0.5190807


test.data.frame <- data.frame(a=1, b=2, c="filler")
expand.grid(a=1:2, b=1:2)
merge(test.data.frame, expand.grid(a=1:2, b=1:2), all= TRUE)


expand.grid(a=1:2, b=c("A", "B"))


str(expand.grid( full.df$year, full.df$country.name ))

str(expand.grid( unique(full.df$year), unique(full.df$country.name ),
unique( full.df$naics[substr(as.character(full.df$naics), 1, 2) %in% 31:33] ) ) )


all.panel.combns <- expand.grid( unique(full.df$year), unique(full.df$country.name ),
  unique( full.df$naics[substr(as.character(full.df$naics), 1, 2) %in% 31:33] ) )
  
colnames(all.panel.combns) <- c("year", "country.name", "naics")
  
all.panel.combns <- merge(all.panel.combns, unique(full.df[, c("year", "country.name", "bit", "fta", "GDP", "gc.dist" )]), all=TRUE)


# May have a few "observations" drop off since not all countries will have GDP numbers 
# available in all years

all.panel.combns <- merge(all.panel.combns, unique(full.df[, c("naics", "scale.factor", "jit.preference" )]), all.x=TRUE)
# Don't want the legitimate naics-specific data to be lost when merging with full.df, so need to have
# the full panel expansion with each variable

prop.table(table(complete.cases(all.panel.combns)))
#    FALSE      TRUE 
#0.1028325 0.8971675 
# I think the result above indicates that we are OK

full.test.df <- merge(full.df[,  c("naics", "year", "country.name", "export.value")], 
  all.panel.combns, all= TRUE)
# Added to the incomplete cases with the merge above, but this is probbaly due to 
# specifying all=TRUE rather than all.y=TRUE
  
full.test.df$export.value[is.na(full.test.df$export.value)] <- 0

table(complete.cases(full.test.df))
table(complete.cases(all.panel.combns))
# Should probably do a spot-check to make sure that this thing works OK


full.test.df$log.scale.factor <- log(full.test.df$scale.factor)




input.formula <- as.formula( asinh(export.value) ~  ( I(log.scale.factor - log.scale.factor.median)  + 
                                            I((jit.preference - jit.preference.median)/jit.preference.sd) + fta +
                                           I(log(gc.dist) - log.gc.dist.median)  +  I(log(GDP) - log.gdp.median) )^2     | 
                                  country.name + year + naics   | 0 |  country.name  )


full.df <- full.test.df



output.reg.table(input.formula , "table1.tex", 
                "MODEL 1: FTA effect. Manufacturing exports only. FE on country, year, and NAICS classification. Clustered standard errors on country. Variables have been standardized by median and SD, except GDP and distance were not standardized by SD.", file.or.object="object") 





results.felm <- felm( input.formula , 
  data=full.test.df[ 
#                   & full.df$cty_code %in% fta.dates.df$cty_code[fta.dates.df$fta.start.yr > 1989 ] 
                substr(as.character(full.test.df$naics), 1, 2) %in% 31:33
, ],
exactDOF=TRUE) # weights=


summary( results.felm)


library(lfe)
library(stargazer)
library(restorepoint)



