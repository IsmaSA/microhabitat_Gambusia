#### Microhabitat: 
setwd("C:/Users/Propietario/Desktop/Escritorio/Irmak_Sehan/Microhabitat")

df <- read_xlsx("Microhabitat_Phillip_IrmakV5_26.04.23.xlsx", sheet = "Sheet1")

head(df)
names(df)


library(maps)
library(rworldmap)
library(rworldxtra)

newmap <- getMap(resolution="high")
par(mar=c(0.5,0.5,0.5,0.5))
plot(newmap, col="tan1", border="black",bg="lightblue", 
     xlim=c(23,48), ylim=c(35,43))

setwd("C:/Users/Propietario/Desktop/Escritorio/Crustaceans0/crustaceans/qgis")

riversData <- rgdal::readOGR("tur_watcrsl_rvr_1m.shp")
plot(riversData, col= "steelblue2", add=T)



dms_to_decimal <- function(dms_str) {
  standardized_str <- gsub("[˚°']", "°", dms_str)
  standardized_str <- gsub("[″\"']", "\"", standardized_str)
  
  components <- strsplit(standardized_str, "[°′\"]")[[1]]
  
  if(length(components) == 3 && all(sapply(components, function(x) grepl("^\\d+$", x)))) {
    degrees <- as.numeric(components[1])
    minutes <- as.numeric(components[2])
    seconds <- as.numeric(components[3])
    
    decimal <- degrees + (minutes / 60) + (seconds / 3600)
    return(decimal)
  } else {
    return(NA)
  }
}

# Apply the function to the data
df$Latitude_decimal <- sapply(df$Latitude, dms_to_decimal)
df$Longitude_decimal <- sapply(df$Longitude, dms_to_decimal)




str(df)

points(df$Longitude_decimal , df$Latitude_decimal , 
       col="black", pch=23, cex=0.8, bg="red2")

scalebar(x = 24.5, y = 36.2, which = "both", unit = "km", cex = 0.8, 
         length = 20, col = "black", bg = "white", border = "black")
maps::map.scale(x = 24.5, y = 36.2, ratio = F, relwidth=0.25)


####################   Model #############################
str(df)
options(scipen = 999)
df[,c(2,7,8,9,11,12,13,22)] <- lapply(df[,c(2,7,8,9,11,12,13,22)], as.factor)

df$WM <- as.numeric(df$WM)
df$SAV <- as.numeric(df$SAV)
df$Velocity <- as.factor(df$Velocity)

library(lme4)
df$`Locality Name`
df$`Shade/sun`
# Fitting the model
cor(df[, c("SAV", "WM", "Shade/sun", "Substrata", "Velocity", "Turbidity", "Reeds")])

df_scaled <- df
variables_to_scale <- c("SAV", "WM", "DFB", "Depth", "DNV", "Substrata", "Velocity", "Turbidity", "Reeds") # Add or remove as appropriate
df_scaled[variables_to_scale] <- scale(df[variables_to_scale])



model1 <- glmer(`1/0` ~ SAV + WM + `Shade/sun` + Substrata + Velocity + Turbidity + Reeds + (1 | `Locality Name`), 
               data = df_scaled,  #or df
               family = binomial(link = "logit"),
               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))


summary(model1)


model2 <- glmer(`1/0` ~ SAV + WM + `Shade/sun` + Substrata + Velocity + Turbidity + Reeds + (1 | `Locality Name`), 
                data = df,  #or df
                family = binomial(link = "logit"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))


summary(model2)

AIC(model1,model2)



############# Try with GAMMS:  NOT WORKING

library(gamm4)

# Model 3
# Load the mgcv package
library(mgcv)
colnames(df_scaled)[22] <- "Presence"
colnames(df_scaled)[9] <- "Shade"
colnames(df_scaled)[2] <- "Locality"

# Convert model3 to a GAMM
model3_gamm <- gam(Presence ~ s(SAV,k=3) + s(WM,k=3) + s(Shade,k=3) + s(Substrata,k=3) + s(Velocity,k=3) + s(Turbidity,k=3) + s(Reeds,k=3) 
                   + s(Locality, bs="re"),
                   data = df_scaled,  #or df
                   family = binomial(link = "logit"))

# Summary of the model
summary(model3_gamm$gam)



# Model 4
model4_gamm <- gam(`1/0` ~ s(SAV) + s(WM) + s(`Shade/sun`) + s(Substrata) + s(Velocity) + s(Turbidity) + s(Reeds) + s(`Locality Name`, bs="re"), 
                   data = df,  # or df_scaled
                   family = binomial(link = "logit"))

# Summary of the model
summary(model4_gamm)



######################        CPUE        ############################
head(df)

str(df)
hist(df$`A(m)`)
colnames(df)[3] <- "Lentic"
colnames(df)[34] <- "Altitude"

df$Lentic <- as.factor(df$Lentic)
df$SC <- as.numeric(df$SC)
df$Altitude <- as.numeric(as.character(df$Altitude))

library(corrplot)
library(usdm)
library(randomForestSRC)
library(ggRandomForests)

Spatial_Dp <- df 
names(Spatial_Dp)

M_RFoc[] <- lapply(M_RFoc, function(x) {
  if (is.character(x)) return(factor(x))
  return(x)
})


### RANDOM FOREST FOR OCURRENCE
set.seed(1234)#sets a numerical starting point
M_RFoc=Spatial_Dp[c(1:8, 21, 11,12,13,14:19, 20, 21,23,34, 37,38)] ## create matrix
Oc.rf = rfsrc(CPUE ~ ., mtry=5, ntree=500, importance = "random",data=as.data.frame(M_RFoc)) # function to run RF
Oc.rf # (OOB) R squared: 0.3686226
plot (gg_error(Oc.rf)) # check if the number of tress is appropriate
Oc.rf.vimp = gg_vimp (Oc.rf) # calculate variable importance


Oc.rf.vimp$vars <- c("Longitude","Altitude","Dissolved oxygen","Dissolved oxygen saturated",
                     "Specific conductivity","pH","Salinity","Latitude", "Water temperature")

ggplot(Oc.rf.vimp, aes(x = reorder(vars, vimp), y = vimp)) +
  geom_bar(stat = 'identity', aes(fill = vimp > 0)) +  # Add fill aesthetic
  xlab("Variables") +
  ylab("Importance score") +
  #geom_text(aes(label = round(vimp, 2)), vjust = 0) +
  scale_fill_manual(values = c("TRUE" = "purple", "FALSE" = "brown")) +  # Manually set colors
  theme_bw() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))


plot (Oc.rf.vimp) # Plot variable importance




# GLM model: 
if (!requireNamespace("glmmTMB", quietly = TRUE)) {
  install.packages("glmmTMB")
}
library(glmmTMB)

# Fit the model
hist(df$CPUE)
# Renaming the levels of the Lentic variable
df$Lentic <- factor(df$Lentic, levels = c(0, 1), labels = c("Lentic", "Lotic"))

df_no_na$CPUE <- round(df_no_na$CPUE)
model <- glmmTMB(CPUE ~ Altitude + Longitude_decimal + (1 | Lentic),
                 data = df, 
                 family = nbinom2)

summary(model)

fitted_values <- fitted(model)
original_data <- df
original_data$fitted <- fitted_values


p1<- ggplot(df, aes(x = Latitude_decimal, y = fitted_values)) +
  geom_smooth(method = "glm", se = TRUE, aes(color = "Fitted Values Smoothed"), method.args = list(family = "poisson")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("CPUE") +
  xlab("Latitude") +
  facet_wrap(~ Lentic) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))



  
p2<- ggplot(df, aes(x = Altitude, y = fitted_values)) +
  geom_smooth(method = "glm", se = TRUE, aes(color = "Fitted Values Smoothed"), method.args = list(family = "poisson")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("CPUE") +
  xlab("Altitude (m)") +
  facet_wrap(~ Lentic) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))

plot_grid(p1,p2,ncol = 1)



##### CPUE  #########
names(df)

# 1. Random Forest -> CPUE (only sites with gambusia) -> GLMM
#Distance_bank	Depth	Distance_vegeation	Submerged_vegetation	Woody_material	shade_sun	Velocity	Turbidity	
#Reeds	Substrata	WT	DO2	DOS	pH	S

df1<- df %>% filter(`1/0` =="1")
df1$SC <- as.numeric(df1$SC)
names(df1)

M_RFoc=df1[c(21,4,5,6,7,8,9,10:19,37,38,34)] ## create matrix
Oc.rf = rfsrc(CPUE ~ ., mtry=5, ntree=500, importance = "random",data=as.data.frame(M_RFoc)) # function to run RF
Oc.rf # (OOB) R squared: 0.01
plot (gg_error(Oc.rf)) # check if the number of tress is appropriate
Oc.rf.vimp = gg_vimp (Oc.rf) # calculate variable importance


Oc.rf.vimp$vars <- c("Dissolved oxygen saturated","Distance from the bank","Latitude","Dissolved oxygen", "Velocity",
                     "Shade/sun","Substrata","Turbidity", "Reeds","Salinity","Specific conductivity", "Altitude",
                     "pH","Submerged aquatic vegetation","Longitude","Distance to nearest vegetation", 
                     "Water temperature","Depth","Woddy material")

ggplot(Oc.rf.vimp, aes(x = reorder(vars, vimp), y = vimp)) +
  geom_bar(stat = 'identity', aes(fill = vimp > 0)) +  # Add fill aesthetic
  xlab("Variables") +
  ylab("Importance score") +
  #geom_text(aes(label = round(vimp, 2)), vjust = 0) +
  scale_fill_manual(values = c("TRUE" = "purple", "FALSE" = "brown")) +  # Manually set colors
  theme_bw() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))




# Renaming the levels of the Lentic variable
df1$`Lentic/Lotic` <- factor(df1$`Lentic/Lotic`, levels = c(0, 1), labels = c("Lentic", "Lotic"))
names(df1)

model <- glmmTMB(CPUE ~  DFB+ Latitude_decimal +DOS+ WT+WM+Depth+ (1 | `Lentic/Lotic`),
                 data = df1, 
                 family = nbinom2)


summary(model)

fitted_values <- fitted(model)
original_data <- df1
original_data$fitted <- fitted_values


p1<- ggplot(df1, aes(x = Latitude_decimal, y = fitted_values)) +
  geom_smooth(method = "glm", se = TRUE, aes(color = "Fitted Values Smoothed"), method.args = list(family = "poisson")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("CPUE") +
  xlab("Latitude") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))
p1


p1<- ggplot(df1, aes(x = DFB, y = fitted_values)) +
  geom_smooth(method = "glm", se = TRUE, aes(color = "Fitted Values Smoothed"), method.args = list(family = "poisson")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("CPUE") +
  xlab("Distance from the bank") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))
p1


p1<- ggplot(df1, aes(x = DOS, y = fitted_values)) +
  geom_smooth(method = "glm", se = TRUE, aes(color = "Fitted Values Smoothed"), method.args = list(family = "poisson")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("CPUE") +
  xlab("Dissolved oxygen saturated") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))
p1

p1<- ggplot(df1, aes(x = WT, y = fitted_values)) +
  geom_smooth(method = "glm", se = TRUE, aes(color = "Fitted Values Smoothed"), method.args = list(family = "poisson")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("CPUE") +
  xlab("Water temperature") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))
p1

p1<- ggplot(df1, aes(x = WM, y = fitted_values)) +
  geom_smooth(method = "glm", se = TRUE, aes(color = "Fitted Values Smoothed"), method.args = list(family = "poisson")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("CPUE") +
  xlab("Woddy material") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))
p1

p1<- ggplot(df1, aes(x = Depth, y = fitted_values)) +
  geom_smooth(method = "glm", se = TRUE, aes(color = "Fitted Values Smoothed"), method.args = list(family = "poisson")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("CPUE") +
  xlab("Depth") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))
p1
#### Microhabitat #######
#2. Random Forest -> presence Absence (sites with and without gambusia -> GAM


df$SC <- as.numeric(df$SC)
names(df)

M_RFoc=df[c(22,4,5,6,7,8,9,10:19,37,38,34)] ## create matrix
Oc.rf = rfsrc(`1/0` ~ ., mtry=5, ntree=500, importance = "random",data=as.data.frame(M_RFoc)) # function to run RF
Oc.rf # (OOB) R squared: 0.01
plot (gg_error(Oc.rf)) # check if the number of tress is appropriate
Oc.rf.vimp = gg_vimp (Oc.rf) # calculate variable importance
Oc.rf.vimp<- Oc.rf.vimp %>% filter(set =="all")

Oc.rf.vimp$vars <- c("Velocity","Altitude","Submerged aquatic vegetation","pH","Longitude","Substrata",
                     "Latitude",  "Water temperature","Salinity","Dissolved oxygen saturated","Turbidity",
                     "Distance from the bank","Reeds", "Shade/sun","Distance to nearest vegetation", "Depth",
              "Dissolved oxygen","Specific conductivity", "Woddy material")

ggplot(Oc.rf.vimp, aes(x = reorder(vars, vimp), y = vimp)) +
  geom_bar(stat = 'identity', aes(fill = vimp > 0)) +  # Add fill aesthetic
  xlab("Variables") +
  ylab("Importance score") +
  #geom_text(aes(label = round(vimp, 2)), vjust = 0) +
  scale_fill_manual(values = c("TRUE" = "purple", "FALSE" = "brown")) +  # Manually set colors
  theme_bw() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

df2 <- df[,c(22,10,34,7,17,38,13,3)]

df2$Velocity <- factor(df2$Velocity)
df2$Substrata <- factor(df2$Substrata)
df2$`1/0` <- factor(df2$`1/0`)
df2$Longitude_decimal <- unname(df2$Longitude_decimal)

colnames(df2)[3] <- "Altitude"
colnames(df2)[1] <- "Presence"
colnames(df2)[8] <- "Lentic"

model1 <- gam(Presence ~  Velocity  + s(Altitude, k=5)  + s(SAV, k=5)+ s(pH, k=5)+ s(Longitude_decimal, k=5) + Substrata+ s(Lentic, bs="re"),
                 data = df2, 
             family = binomial(link = "logit"))


summary(model1)


fitted_values1 <- fitted(model1)
original_data2 <- df2
original_data2$fitted <- fitted_values1


p1<- ggplot(df2, aes(x = Velocity, y = fitted_values1)) +
  geom_smooth(method = "gam", se = TRUE, aes(color = "Fitted Values Smoothed")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("Presence/absence") +
  xlab("Velocity") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))
p1

p2<- ggplot(df2, aes(x = Altitude, y = fitted_values1)) +
  geom_smooth(method = "gam", se = TRUE, aes(color = "Fitted Values Smoothed")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("Presence/absence") +
  xlab("Altitude") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))

p3<- ggplot(df2, aes(x = SAV, y = fitted_values1)) +
  geom_smooth(method = "gam", se = TRUE, aes(color = "Fitted Values Smoothed")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("Presence/absence") +
  xlab("Submerged aquatic vegetation") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))

p4<- ggplot(df2, aes(x = pH, y = fitted_values1)) +
  geom_smooth(method = "gam", se = TRUE, aes(color = "Fitted Values Smoothed")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("Presence/absence") +
  xlab("pH") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))

p5<- ggplot(df2, aes(x = Longitude_decimal, y = fitted_values1)) +
  geom_smooth(method = "gam", se = TRUE, aes(color = "Fitted Values Smoothed")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("Presence/absence") +
  xlab("Longitude") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))

p6<- ggplot(df2, aes(x = Substrata, y = fitted_values1)) +
  geom_smooth(method = "gam", se = TRUE, aes(color = "Fitted Values Smoothed")) +
  theme_bw() +
  geom_point(aes(color = "Observed"),color="blue", alpha = 0.6) +
  ylab("Presence/absence") +
  xlab("Substrata") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c("Fitted Values Smoothed" = "red"))


