# pulling together all the WNV case study data to produce pretty figures

mainFolder <- getwd()
ParData <- paste0(mainFolder, '/Scripts')

# load all the par data
a <- read.csv(paste0(ParData, '/biteRateTemp.csv'), header=TRUE, stringsAsFactors = FALSE)
EDR <- read.csv(paste0(ParData, '/EDRTemp.csv'), header=TRUE, stringsAsFactors = FALSE)
V <- read.csv(paste0(ParData, '/eggViabilityTemp.csv'), header=TRUE, stringsAsFactors = FALSE)
EIP <- read.csv(paste0(ParData, '/EIPTemp.csv'), header=TRUE, stringsAsFactors = FALSE)
Fec <- read.csv(paste0(ParData, '/fecundityTemp.csv'), header=TRUE, stringsAsFactors = FALSE)
LDR <- read.csv(paste0(ParData, '/LDRTemp.csv'), header=TRUE, stringsAsFactors = FALSE)
Long <- read.csv(paste0(ParData, '/longevityTemp.csv'), header=TRUE, stringsAsFactors = FALSE)
S <- read.csv(paste0(ParData, '/survivalTemp.csv'), header=TRUE, stringsAsFactors = FALSE)

