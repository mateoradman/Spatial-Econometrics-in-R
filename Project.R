#Spatial Econometrics Project in R

#Load the libraries and make sure you have them installed. 
#Otherwise use install.packages("name_of_the_package") command
library(stargazer) # For nice output.
library(spdep)     # For the spatial analysis.
library(maptools)  # For mapping.
library(RColorBrewer) # For colour palettes.
library(classInt)  # For class intervals when mapping
library(Hmisc) #Useful tool for correlation matrices
library(corrplot) #Plotting correlation matrix

#Set the working directory you wish your plots to be saved in
setwd("~/your_folder_path")
#Load the data
wijkshp <- readShapeSpatial("~/your_folder_path/wijk_2017_v3.shp")
#Summary of the data to detect missing values
stargazer(wijkshp@data, type = "text")

#Filter out the area which is not land 
wijkshp <- wijkshp[wijkshp$WATER=="NEE",]

#Create centroids
centroids <- coordinates(wijkshp)

#Create a data frame 'df' with my variables of interest for easier manipulation and summary
df <- wijkshp@data[c("P_HUURWON","P_STADVERW", "AF_TREINST", "GEM_HH_GR", "AF_ONDVRT", "BEV_DICHTH", "WOZ")]

#Check whether the process is finished and all variables are neat
summary(df)

#Correlation matrix of all variables with p-values, rounded to two decimals
correlation <- rcorr(as.matrix(df), type = "pearson")
        round(correlation, 2)
correlation

#Plotted correlation
corrplot(correlation$r, type="upper", order="hclust", 
         p.mat = correlation$P, sig.level = 0.05, insig = "blank")

#Simple regression and ANOVA
lm.df <- lm(WOZ~P_HUURWON + P_STADVERW + AF_TREINST + GEM_HH_GR + AF_ONDVRT + BEV_DICHTH, data = df)
summary(lm.df)
anova(lm.df)


#Create a graph for the share of rental housing
pdf("P_HUURWON.pdf")
  brks <- classIntervals(wijkshp$P_HUURWON,5,style="quantile")$brks
  colors <- brewer.pal(length(brks)-1,"Oranges")
  plot(wijkshp,border=TRUE,col=(colors[findInterval(wijkshp$P_HUURWON,brks,all.inside=TRUE)]), lwd = .5)
  legend("topleft",legend=leglabs(brks,between="to"), fill=colors)
  title(main="Share of rental housing in the Netherlands")
dev.off()  

#Create a graph for share of houses connected to centralized urban heating
#NOTE: This variable has a lot of missing values which are transformed to 0,
#and R thinks their value is 0. This is not okay usually, but for the sake of the practice,
#I will plot this variable on a map.
pdf("PP_STADVERW.pdf")
  brks <- classIntervals(wijkshp$P_STADVERW,5,style="quantile")$brks
  colors <- brewer.pal(length(brks)-1,"Oranges")
  plot(wijkshp,border=TRUE,col=(colors[findInterval(wijskhp$P_STADVERW,brks,all.inside=TRUE)]))
  legend("topleft",legend=leglabs(brks,between="to"), fill=colors)
  title(main="Share of houses connected to centralized urban heating \n in the Netherlands")
dev.off()  

#Create a graph for the average housing price
pdf("WOZ.pdf")
brks <- classIntervals(wijkshp$WOZ,5,style="fisher")$brks
colors <- brewer.pal(length(brks)-1,"Blues")
plot(wijkshp,border=TRUE,col=(colors[findInterval(wijkshp$WOZ,round(brks, 0),all.inside=TRUE)]))
legend("topleft",legend=leglabs(brks,between="to"), fill=colors)
title(main="Average housing price in the Netherlands")
dev.off()  

#LISA 
#Distance based weight matrix (7.5 kilometer)
dist15 <- dnearneigh(centroids, 0, 7500, wijkshp$WK_CODE)
wdist15 <- nb2listw(dist15, style = "W", zero.policy=TRUE)
summary(wdist15, zero.policy=TRUE)

# === ESDA ===
moran.test(wijkshp$WOZ,wdist15, zero.policy=TRUE)
moran.mc(wijkshp$WOZ,wdist15,nsim=100, zero.policy=TRUE)

# The Local Moran's I
lisa <- localmoran(wijkshp$WOZ, wdist15, zero.policy = TRUE)

# Centering around the mean
cWOZ <- wijkshp$WOZ - mean(wijkshp$WOZ)
mWOZ <- lisa[, 1]
C_mWOZ <- mWOZ - mean(mWOZ)

#Creation of quadrants for the legend
quadrant <- vector(mode="numeric",length=nrow(lisa))
quadrant[cWOZ>0 & mWOZ>0] <- 1
quadrant[cWOZ <0 & mWOZ>0] <- 2     
quadrant[cWOZ>0 & mWOZ<0] <- 3
quadrant[cWOZ <0 & mWOZ<0] <- 4

#Setting the sig. level to 5%
signif <- 0.05

# places non-significant Moran's in the category "5"
quadrant[lisa[, 5]> signif] <- 5

# Lisa map
pdf(file="Lisa_map_R.pdf")
  colors <- c("red", "blue", "lightpink", "skyblue2", rgb(.95, .95, .95))
  par(mar=c(0,0,1,0)) # sets margin parameters for plot space
  plot(wijkshp, border="grey", col=colors[quadrant], main = "LISA Cluster Map of average housing price")
  legend("bottomright",legend=c("high-high","low-low","high-low","low-high"), fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)
dev.off() 

#Creating Gabriel matrix
gabriel <- graph2nb(gabrielneigh(centroids),sym=TRUE)

#Creating weighted Gabriel matrix
wgabriel <- nb2listw(gabriel, style = "W")


#Calculating Moran's I for WOZ
moran.test(wijkshp$WOZ, wgabriel)
moran.mc(wijkshp$WOZ, wgabriel, nsim = 100)
#Moran plot for WOZ
moran.plot(wijkshp$WOZ, wgabriel)

#Calculating Moran's I for wijkshp$P_HUURWON
moran.test(wijkshp$P_HUURWON, wgabriel)
moran.mc(wijkshp$P_HUURWON, wgabriel, nsim = 100)
#Moran plot for wijkshp$P_HUURWON
moran.plot(wijkshp$P_HUURWON, wgabriel)

#Calculating Moran's I for wijkshp$P_STADVERW
moran.test(wijkshp$P_STADVERW, wgabriel)
moran.mc(wijkshp$P_STADVERW, wgabriel, nsim = 100)
#Moran plot for wijkshp$P_STADVERW
moran.plot(wijkshp$P_STADVERW, wgabriel)

#Create spatially lagged independent variable (population density)
wBEV_DICHTH <- lag.listw(wgabriel, wijkshp$BEV_DICHTH)

#Linear model with a spatially lagged variable
slm.df <- lm(WOZ~P_HUURWON + P_STADVERW + AF_TREINST + GEM_HH_GR + AF_ONDVRT + BEV_DICHTH + wBEV_DICHTH, data = df)
#Check if everything is okay
summary(slm.df)

#Output file for standard linear model (lm.df) and spatially lagged model (slm.df) 
stargazer(lm.df,slm.df, out="reg.html")


