## =======================================================================
## This functions reshapes output of model
## =======================================================================


process.harness.data<-function(filename, dir.inputs, experiment.version, dir.harness){
#Load needed libraries
 library(reshape2)
 library(data.table)
#load experimental design
  Exp.design<-read.csv(paste(dir.inputs,experiment.version,sep=""))

  #load harness data
  harness.data <- rbindlist(lapply(file.path(dir.harness, filename), fread), use.names = TRUE, fill = TRUE)
  #harness.data<-data.table(read.csv(paste(dir.harness,filename,sep=""))) ## <- ORIGINAL

  #convert integer values in Exp.design into numeric
  integers<-names(subset(sapply(Exp.design,is.integer),sapply(Exp.design,is.integer)==TRUE))
  Exp.design[,integers]<- lapply(Exp.design[,integers], as.numeric)
  Exp.design<-data.table(Exp.design)

  #merge Exp design with harnessed data
  harness.data<-merge(harness.data, Exp.design, by="Run.ID")

  #Reshape output and in  future calculate additional metrics
  harness.data[,TimeStep:=NULL]
  harness.data[,EndTime:=NULL]
  harness.data[,Budget.function.N:=NULL]
  harness.data[,Budget.function.S:=NULL]
  harness.data[,Budget.N:=NULL]
  harness.data[,Budget.S:=NULL]
#Adjust column names
  setnames(harness.data,c("Are.N","Ace.N","Are.S","Ace.S"),c("Are_N","Ace_N","Are_S","Ace_S"))
#Create levels for regions
  id.vars<-c("Run.ID","Future.ID","time",
            "S","Delta.Temp","CO2.Concentration",
            "alfa","epsilon","size.factor","Eta.re","Eta.ce","Gamma.re","Gamma.ce","Nu.re","Nu.ce","k.re","k.ce",
			"lambda.S","sigma.utility",
			"Climate.Model","Beta.Delta.Temp","CO2.base","CO2.Disaster","Delta.Temp.Disaster","qsi","Delta.S","S.0",
			"policy.name","Cost.S.Damage","policy.status","Policy.Duration","Policy.Start.Time",
            "ce.tax_N","RD.subsidy_N","RD.subsidy.GF_N","Tec.subsidy_N","Tec.subsidy.GF_N","ce.tax_S","RD.subsidy_S","Tec.subsidy_S",
			"dS.lag","rho")

 measure.vars<-subset(colnames(harness.data),!(colnames(harness.data)%in%id.vars))
#convert integer measure.vars into numeric
 
 integers <- names(which(sapply(harness.data[, ..measure.vars], is.integer)))
 if (length(integers) > 0) {
   harness.data[, (integers) := lapply(.SD, as.numeric), .SDcols = integers]
 }

#reshape to generate region
 harness.data <- data.table::melt(harness.data, id.vars=id.vars, measure.vars=measure.vars,  variable.name="Variable.Region")
 harness.data[, c("Variable", "Region") := tstrsplit(Variable.Region, "_")]
 harness.data[, Variable.Region := NULL]

 
 #reshape variables to wide
 harness.data <- data.table(dcast(harness.data,
                          Run.ID+
						  Future.ID+
						  time+
						  S+
						  Delta.Temp+
						  CO2.Concentration+
						  alfa+
						  epsilon+
						  size.factor+
						  Eta.re+
						  Eta.ce+
						  Gamma.re+
						  Gamma.ce+
						  Nu.re+
						  Nu.ce+
						  k.re+
						  k.ce+
						  lambda.S+
						  sigma.utility+
						  Climate.Model+
						  Beta.Delta.Temp+
						  CO2.base+
						  CO2.Disaster+
						  Delta.Temp.Disaster+
						  qsi+
						  Delta.S+
						  S.0+
						  policy.name+
                          Cost.S.Damage+
						  policy.status+
						  Policy.Duration+
						  Policy.Start.Time+
                          ce.tax_N+
						  RD.subsidy_N+
						  RD.subsidy.GF_N+
						  Tec.subsidy_N+
						  Tec.subsidy.GF_N+
						  ce.tax_S+
						  RD.subsidy_S+
						  Tec.subsidy_S+
			              dS.lag+
						  rho+
						  Region ~ Variable, value.var="value"))
return(harness.data)
}

## =======================================================================
## This functions reshapes output of model
## =======================================================================
process.prim.data<-function(filename,dir.harness){
  harness.data <-read.csv(paste(dir.harness,filename,sep=""))
  
  #First obtain the policy vector
   policy.elements<-c("Policy.Start.Time","Policy.Duration",
                     "ce.tax_N","RD.subsidy_N","RD.subsidy.GF_N","Tec.subsidy_N","Tec.subsidy.GF_N",
                     "ce.tax_S","RD.subsidy_S","Tec.subsidy_S")
   policy.table<-aggregate(harness.data[, policy.elements],
                           list(Run.ID=harness.data$Run.ID), max)
  
   #Create time slices
   #slice at 100 yrs
    slice.100<-harness.data[,subset(colnames(harness.data),!(colnames(harness.data)%in%policy.elements))]
	slice.100<-subset(slice.100,slice.100$time==2123)
   
	#slice at 300 yrs ||| SE HIZO UNA SIMULACIÓN DE MENOS AÑOS, POR ESO SE OMITEN
	#slice.300<-harness.data[,subset(colnames(harness.data),!(colnames(harness.data)%in%policy.elements))]
	#slice.300<-subset(slice.300,slice.300$time==2312)
  #  colnames(slice.300)<-paste(colnames(slice.300),".300yr",sep="")
	#slice.300$Run.ID<-slice.300$Run.ID.300yr
	#slice.300$Run.ID.300yr<-NULL
	#slice.300$time<-NULL
	
  #Aggregation metrics
   #aggregation at 100 yrs
    total.elements<-c("Y_N","Y_S","Consumption_N","Consumption_S","Utility.Consumer_N","Utility.Consumer_S")
    total.table<-aggregate(harness.data[harness.data$time<=2112,total.elements],list(Run.ID=harness.data$Run.ID[harness.data$time<=2112]),sum)
    colnames(total.table)<-c("Run.ID","Y.Total_N","Y.Total_S","Consumption.Total_N","Consumption.Total_S","Utility.Consumer.Total_N","Utility.Consumer.Total_S")
  
     #aggregation at 300 yrs
   # total.elements<-c("Y_N","Y_S","Consumption_N","Consumption_S","Utility.Consumer_N","Utility.Consumer_S")
  #  total.table.300<-aggregate(harness.data[,total.elements],list(Run.ID=harness.data$Run.ID),sum)
   # colnames(total.table.300)<-c("Run.ID","Y.Total_N.300","Y.Total_S.300","Consumption.Total_N.300","Consumption.Total_S.300","Utility.Consumer.Total_N.300","Utility.Consumer.Total_S.300")
   #merge all tables
    harness.data<-Reduce(function(...) { merge(..., all=TRUE) }, list(slice.100,
                                                                      total.table,
                                                                      policy.table))
    return(harness.data)
}

## =======================================================================
## This function merges process output with experimental design
## =======================================================================

merge.exp.design<-function(dir.inputs,experiment.version,data)
{
 #load experimental design
  Exp.design <- read.csv(paste(dir.inputs,experiment.version,sep=""))

  #convert integer values in Exp.design into numeric
  integers <- names(subset(sapply(Exp.design,is.integer),sapply(Exp.design,is.integer)==TRUE))
  Exp.design[,integers]<- lapply(Exp.design[,integers], as.numeric)
  Exp.design <- data.table(Exp.design)

  #merge Exp design with harnessed data
  data <- merge(data, Exp.design, by="Run.ID")
  
#Create levels for regions
  id.vars<-c("Run.ID","Future.ID","time",
             "S","Delta.Temp","CO2.Concentration",
            "alfa","epsilon","size.factor","Eta.re","Eta.ce","Gamma.re","Gamma.ce","Nu.re","Nu.ce","k.re","k.ce",
 			"lambda.S","sigma.utility",
 			"Climate.Model","Beta.Delta.Temp","CO2.base","CO2.Disaster","Delta.Temp.Disaster","qsi","Delta.S","S.0",
 			"policy.name")
 measure.vars <- subset(colnames(data),!(colnames(data)%in%id.vars))

 #convert integer measure.vars into numeric
  integers <- names(subset(sapply(data[,measure.vars,with=FALSE],is.integer),
                           sapply(data[,measure.vars,with=FALSE],is.integer)==TRUE))
  
  data[, (integers) := lapply(.SD, as.numeric), .SDcols = integers]
  
  return(data)
}
