data <- read.csv("SEPHER2.0.csv", colClasses=c(FIPS="character"))
dict <- read.csv("SEPHER2.0_dataDictionary.csv")
missing_columns <- setdiff(dict$Name, colnames(data))
nrow(dict[!(dict$Name  %in% missing_columns),])
new_dict <- dict[!(dict$Name  %in% missing_columns),]
missing_columns
nrow(dict)
nrow(new_dict)
to_drop <- c('EP_DISABL_2010', 'EPL_DISABL_2010', 'F_DISABL_2010', 'E_DISABL_2010',
            'AREA_SQMI_2000', 'TOTPOP_2000', 'TOTPOP_2014', 'TOTPOP_2016',
            'TOTPOP_2018', 'M_TOTPOP_2000', 'HU_2000', 'HU_2014', 'HU_2016',
            'HU_2018', 'M_HU_2000', 'HH_2000', 'HH_2014', 'HH_2016', 'HH_2018',
            'M_POV_2000', 'M_UNEMP_2000', 'M_PCI_2000', 'M_NOHSDP_2000',
            'M_LIMENG_2000', 'M_MUNIT_2000', 'M_MOBILE_2000', 'M_CROWD_2000',
            'M_NOVEH_2000', 'MP_POV_2000', 'MP_UNEMP_2000', 'MP_NOHSDP_2000',
            'MP_LIMENG_2000', 'MP_MUNIT_2000', 'MP_MOBILE_2000', 'MP_CROWD_2000',
            'MP_NOVEH_2000', 'SPL_THEME1_2000', 'SPL_THEME2_2000',
            'SPL_THEME3_2000', 'SPL_THEME4_2000', 'SPL_THEMES_2000', 'E_HH_2000',
            'E_HH_2010', 'M_HH_2000', 'M_HH_2010', 'M_AGE65_2000', 'M_AGE65_2010',
            'M_AGE17_2000', 'M_AGE17_2010', 'M_DISABL_2000', 'M_DISABL_2010',
            'M_SNGPNT_2000', 'M_SNGPNT_2010', 'M_MINRTY_2000', 'M_MINRTY_2010',
            'M_GROUPQ_2000', 'M_GROUPQ_2010', 'EP_PCI_2000', 'EP_PCI_2010',
            'MP_PCI_2000', 'MP_PCI_2010', 'MP_AGE65_2000', 'MP_AGE65_2010',
            'MP_AGE17_2000', 'MP_AGE17_2010', 'MP_DISABL_2000', 'MP_DISABL_2010',
            'MP_SNGPNT_2000', 'MP_SNGPNT_2010', 'MP_MINRTY_2000', 'MP_MINRTY_2010',
            'MP_GROUPQ_2000', 'MP_GROUPQ_2010', 'E_UNINSUR_2000', 'E_UNINSUR_2010',
            'M_UNINSUR_2000', 'M_UNINSUR_2010', 'EP_UNINSUR_2000',
            'EP_UNINSUR_2010', 'MP_UNINSUR_2000', 'MP_UNINSUR_2010',
            'E_DAYPOP_2000', 'E_DAYPOP_2010', 'CFLD_EVNTS', 'ERQK_EVNTS',
            'WFIR_EVNTS')
new_dict <- new_dict[!(new_dict$Name  %in% to_drop),]


STATE_CODE <- lapply(data$FIPS, function(x){as.numeric((substr(x, 1, 2)))})
data <- data[STATE_CODE <= 56,]
nrow(data)

new_data<-data[,!colnames(data) %in% to_drop]

setdiff(new_dict$Name,colnames(new_data))
setdiff(colnames(new_data),new_dict$Name)

#all columns are matched, now changing sequence of columns for ease

#Making FIPS the first row in dictionary
rowNumFIPS<-as.numeric(rownames(new_dict[new_dict$Name=="FIPS",])[1])-1
rowOrder<- c(rowNumFIPS,1:(rowNumFIPS-1),(rowNumFIPS+1):nrow(new_dict))
new_dict <- new_dict[rowOrder,]
#reindexing
rownames(new_dict) <- NULL

#reordering data according to dictionary
new_data<-new_data[,new_dict$Name]
colnames(new_data)

write.csv(new_data, "sepher2.0_cleaned.csv",row.names = FALSE)

write.csv(new_dict, "sepher2.0_cleaned_dataDictionary.csv",row.names = FALSE)

