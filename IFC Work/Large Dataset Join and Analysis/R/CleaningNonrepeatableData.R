#this dataset was given to me as part of a larger folder

sector.codes <- read.xlsx('Data/FIG Data Compiled_Master_updated August 26.xlsx', 
                        sheet=11)

sector.codes$`Sector.&.SME.Flag`<- NULL

x <- c('Sector.ID',
       'Tertiary.Sector.Code',
       'SME.Flag',
       'Business.Line.Project',
       'Business.Line.for.Profit.Tables',
       'Client.Type')

length(x) == length(colnames(sector.codes))

colnames(sector.codes) <- x

write.csv(sector.codes, file = 'Clean.data/FIGsectorCodes.csv')