


# Function to detect system type and user, to establish path to dropbox
find.dropbox<-function(){
  sinf<-Sys.info()
  switch(sinf[['sysname']],
         Windows = {dir=paste("C:/Users/",sinf[['user']],"/Dropbox",sep="")},
         Darwin = {dir=paste("/Users/",sinf[['user']],"/Dropbox",sep="")},
         Linux = {dir=paste("/home/", sinf[['user']],"/Dropbox",sep="")}
  )
  dir
}

# Detect user/platform & find dropbox:
to.dropbox<-find.dropbox()


##################

# Now with extended cedar creek data set from Forest:

pth<-paste(to.dropbox,"/sCAFE_SharedFolder/sCAFE_Data/Original/CedarCreek/e001/e001_Plant aboveground biomass data_062016.txt",sep="")

cdr<-read.table(pth,sep="\t", skip=135, header=T,nrows=48050)
tail(cdr)
head(cdr)

##################

# Clean the taxonomy:

# This table generated in 'cleaning_species_list.R', based on feedback from Forest & Adam:
sps.tab<-read.csv("/Users/colin/Dropbox/sCAFE_SharedFolder/sCAFE_Data/Original/CedarCreek/e001/sps_table_v3.csv")
names(sps.tab)[1]<-"Species"

# save only the important columns
sps.tab<-sps.tab[,c('Species','LCD_name','Frequency','Count_Species')]
sps.tab<-sps.tab[sps.tab$Count_Species==1,]

# use this look-up table to add corrected names to cdr
# Note: this is a filtered join, because of the Count_Species==1 choice above - it will drop cases of non-herbaceous species:
cdr2<-right_join(cdr,sps.tab[,c('Species','LCD_name')],by='Species')
cdr2$Species<-as.character(cdr2$Species)
cdr2$LCD_name<-as.character(cdr2$LCD_name)

head(cdr2)
table(cdr2$LCD_name)

# remove duplicate naming column
cdr2$Species<-cdr2$LCD_name
cdr2<-cdr2[,-which(names(cdr2)=="LCD_name")]

# Account for multiple entries, due to lumping species
cdr<- cdr2 %>% group_by(Exp, Year, Field, Plot, NTrt, NAdd, NitrAdd, NAtm.NAdd, Species) %>% summarise(Biomass=sum(Biomass))
#dim(cdr2)-dim(cdr)


### Additional subsetting:

# just year 1992
cdr92<-subset(cdr, cdr$Year==1992)

# just field D
cdr92D<-subset(cdr92, cdr92$Field=="D")

cdr92D<-cdr92D[,c('Plot','NTrt','NAdd','Species','Biomass')]

cedarcreek<-cdr92D

cedarcreek<-cedarcreek[cedarcreek$NTrt!=9,]

save(cedarcreek,file="/Users/colin/Research/Active/PricePartition/code/priceTools/data/cedarcreek.rda")


