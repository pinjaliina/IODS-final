# R Script for the University of Helsinki course Introduction to Open Data Science
# Initial Data Wrangling for the Final Assignment
# 
# Author: Pinja-Liina Jalkanen
# Created: Sat 4 Mar, 2017.
# #
# Source data: http://www.hri.fi/fi/dataset/helsinki-alueittain-2015
# The data includes certain City of Helsinki statistics by city district.
# For descriptions of the data (partially in English), see the following PDF:
# http://www.hel.fi/hel2/tietokeskus/julkaisut/pdf/16_05_27_Helsinki_alueittain_2015_Tikkanen.pdf
# The maintainer of the data is The City of Helsinki Urban Facts
# (http://www.hel.fi/www/tieke/en). The data has been downloaded from
# the Helsinki Region Infoshare (http://www.hri.fi/) on 04/03/17 with the
# licence CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/deed).

###########################
## Initialise the script ##
###########################

# Clear memory.
rm(list = ls())

# Define packages required by this script.
library(xlsx)
library(dplyr)
library(stringr)

# Reset graphical parameters and save the defaults.
plot.new()
.pardefault <- par(no.readonly = TRUE)
dev.off()

######################
## Do the wrangling ##
######################

# Set working directory.
setwd('/Users/pinsku/Dropbox/HY/Tilastotiede/IODS/IODS-final')

# Set the source xlsx file name.
srcfn <- 'Helsinki_alueittain_2015.xlsx'

# Read in source data.
ha_m_et_p_rows <- c(3,6:47) # Read in exactly these rows (use a spreadsheet to investigate!).
ha_main <- read.xlsx(srcfn, sheetName = 'Taulukko', rowIndex = ha_m_et_p_rows)
ha_pop <- read.xlsx(srcfn, sheetName = 'Väestöennuste', rowIndex = ha_m_et_p_rows)
ha_elections <- read.xlsx(srcfn, sheetName = 'Vaalit', rowIndex = c(2,4:45))

# Glimpse the data.
glimpse(ha_main) # Whopping 139 vars! (Open a spreadsheet app to _really_ glimpse!)
glimpse(ha_pop)
glimpse(ha_elections)

# Drop some unnecessary columns; esp. the ha_main has awfully lot of variables.
ha_main_cols_to_keep <- c(1,3,5,7,9,11,13,30,31,33,57,59,65,69,71,73,74,102,108,110,127,130,133)
ha_main <- dplyr::select(ha_main, ha_main_cols_to_keep)
ha_pop <- dplyr::select(ha_pop, c(1,21:31))
# From the 2015 national election data, keep the support share of only two of the parties:
# – Greens (VIHR): a value-liberal, slightly left-leaning pro-urbanisation party.
# – Finns (PS): a populist and value-conservative racism-flirting right wing party.
ha_elections <- dplyr::select(ha_elections, one_of(c('Alue', 'VIHR', 'PS')))

# Glimpse the data again.
glimpse(ha_main)
glimpse(ha_pop)
glimpse(ha_elections)

# Translate the remaining ha_main variable names to English and shorten them.
names(ha_main)[1] <- 'dist_name' # District name
names(ha_main)[2] <- 'fin_spk' # Finnish speakers, %
names(ha_main)[3] <- 'swe_spk' # Swedish speakers, %
names(ha_main)[4] <- 'oth_spk' # Foreign lang speakers, %
names(ha_main)[5] <- 'foreign' # Foreigners, %
names(ha_main)[6] <- 'foreign_bkg' # People with foreign background, %
names(ha_main)[7] <- 'hki_born' # Born in Helsinki, %
names(ha_main)[8] <- 'pop_dens' # Inhabitants per sq km
names(ha_main)[9] <- 'emp_dens' # Employee density per sq km
names(ha_main)[10] <- 'pop_tot' # Total population on the area
names(ha_main)[11] <- 'edu_sec_tot' # Total population with secondary (but no university) education
names(ha_main)[12] <- 'edu_uni_tot' # Total population with university education
names(ha_main)[13] <- 'inc_cap' # Mean income per capita
names(ha_main)[14] <- 'sq_m_cap' # Mean living space in sq m per capita
names(ha_main)[15] <- 'dw_tot' # Total amount of dwellings in the area
names(ha_main)[16] <- 'dw_rent_market' # Total amount of market financed rental dwellings
names(ha_main)[17] <- 'dw_rent_state' # Total amout of state-financed rental dwellings
names(ha_main)[18] <- 'health_cap' # Mean amount of public healthcare visits per capita.
names(ha_main)[19] <- 'inc_supp' # Receivers of income support, %
names(ha_main)[20] <- 'child_prot' # Child protection cases, % of pop.
names(ha_main)[21] <- 'unemp_rate' # Unemployment rate
names(ha_main)[22] <- 'emp_rate' # Employment rate
names(ha_main)[23] <- 'carless' # Households without a car, %

# Calculate some new variables based on the existing ones.
# % of pop. with at least secondary education.
ha_main['edu_sec'] <- round((ha_main['edu_sec_tot'] + ha_main['edu_uni_tot']) / ha_main['pop_tot'] * 100, digits = 1)
ha_main['dw_rent'] <- round((ha_main['dw_rent_market'] + ha_main['dw_rent_state']) / ha_main['dw_tot'] * 100, digits = 1)
# Calculate the mean population growth rate over 10 years.
for(i in 2:ncol(ha_pop)) {
  if(i > 2) {
    ha_pop[paste('y', as.character(i-2), sep = '')] <- ha_pop[i]/ha_pop[i-1]
  }
}
ha_pop['pop_growth'] <- round((rowMeans(ha_pop[13:22]) - 1) * 100, digits = 1)

# Separate district ID numbers from the names.
ha_main['dist_id'] <- str_extract(ha_main$dist_name, '[[:digit:]]{1,3}')
ha_pop['dist_id'] <- str_extract(ha_pop$Alue, '[[:digit:]]{1,3}')
ha_elections['dist_id'] <- str_extract(ha_elections$Alue, '[[:digit:]]{1,3}')

# Drop unnecessary columns.
ha_main[c(10:12,15:17)] <- NULL
ha_pop[1:22] <- NULL
ha_elections[1] <- NULL

# Tidy up area names.
ha_main['dist_name'] <- str_extract(ha_main$dist_name, '[A-Za-zÄÖäö]+[[:space:]][[:alpha:]]+')

# Join all of the data together.
helsinki <- dplyr::inner_join(ha_main, ha_pop, by = 'dist_id')
helsinki <- dplyr::inner_join(helsinki, ha_elections, by = 'dist_id')

# Omit rows with nonexisting values. This leaves out the Östersundom major
# district, which was only joined to the city on 2009. Note that while the
# dw_rent NAs are probably an error, the same rows are affected by
# the pop_growth NAs, which are NOT an error, but actual missing data.
helsinki <- na.omit(helsinki)

# Drop rows that refer to major districts, i.e. are actually summary rows.
helsinki <- helsinki[ ! helsinki$dist_id %in% c(1,2,3,4,5,6,7), ]

# Create new row names from district ID + name and drop the separate fields.
rownames(helsinki) <- paste(helsinki$dist_id, helsinki$dist_name)
helsinki <- helsinki[,-20]
helsinki <- helsinki[,-1]

# Round some of the data
helsinki['health_cap'] <- round(helsinki$health_cap, digits = 1)
helsinki['inc_supp'] <- round(helsinki$inc_supp, digits = 1)
helsinki['child_prot'] <- round(helsinki$child_prot, digits = 1)
helsinki['unemp_rate'] <- round(helsinki$unemp_rate, digits = 1)
helsinki['carless'] <- round(helsinki$carless, digits = 1)
helsinki['VIHR'] <- round(helsinki$VIHR, digits = 1)
helsinki['PS'] <- round(helsinki$PS, digits = 1)

# Explore the final data once more.
glimpse(helsinki)

#####################################
## Write out the data and clean up ##
#####################################

# Write the file.
write.table(helsinki, file = 'helsinki.csv', sep = '\t', col.names = TRUE)

# Clear memory.
rm(list = ls())
