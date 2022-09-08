# Install required R packages 

# install.packages('devtools')
# install_github('https://github.com/USDA-ERS/MTED-TabloToR.git')
# install_github('https://github.com/USDA-ERS/MTED-HARr.git')

# Load required packages

require(devtools)
require(tabloToR)
require(HARr)

 # Initialize a new object of class GEModel
GTAP = tabloToR::GEModel$new()

 # Load a TABLO file into the object (and interpret it)
 # Based on GTAP v.6 (https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=1367)
GTAP$loadTablo('gtap.tab')

 # Read in the data from HAR files
 # Based on GTAPv7 Y2004 
data = list (
  GTAPSETS = HARr::read_har('sets.har'),
  GTAPPARM = HARr::read_har('default.prm'),
  GTAPDATA = HARr::read_har('basedata.har'))
  
 # Load the data to the model
GTAP$loadData(data)

 # Get all variables in the model
allVariables = GTAP$data$variables

 # Specify the exogenous variables in the standard closure
exogenousVariables=c("afall","afcom","afeall","afecom","
  afereg","afesec","afreg","afsec","ams","aoall","aoreg
  ","aosec","atall","atd","atf","atm","ats","au","
  avaall","avareg","avasec","cgdslack","dpgov","dppriv"
  ,"dpsave","endwslack","incomeslack","pfactwld","pop",
  "profitslack","psaveslack","tf","tfd","tfm","tgd","
  tgm","tm","tms","to","tpd","tpm","tp","tradslack","tx","txs")
   
# Filter out all model variables that match the exogenous variable pattern
  
  exogenousModelVariables=allVariables[(Reduce(function
                                               (a,f)
    c(a,grep(sprintf('^%s\\[',f),allVariables)),
    exogenousVariables,c()
  ))]
  
  #Only select some of the qo variables that are exogenous (for factors)
  for(r in GTAP$data$REG)for( e in GTAP$data$ENDW_COMM)
    exogenousModelVariables=c(exogenousModelVariables,
                              sprintf('qo["%s","%s"]',e,r))
  
  shocks=array(0,dim=length(exogenousModelVariables),
               dimnames=list(exogenousModelVariables))
  
  #Specify shocks
  shocks['qo["UnSkLab","SSA"]']= -20
  
  #Specify shocks (a vector of values for all exogenous variables)
  GTAP$setShocks(shocks)

  #Solve the model in a single iteration(=Johansen)
  GTAP$solveModel(iter=1)
  
  