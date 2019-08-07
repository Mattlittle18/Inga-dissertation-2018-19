inga <- read.csv("updated_csv_Table_3_excelspreadsheet.csv")
summary(inga)
#things to check - fixed
#colimana is in there twice - fixed
#some species appear to have zero leaflets - fixed
#there is a high outlier for terminal leaflet size - fixed
#check basal leaflet size of 200, could be real - fixed
#check primary forest and disturbed forest and undisturbed for variably entered data - fixed
#it will be worth checking histograms to identify outliers and see which variables we might log-transform

hist(inga$Avg..no..leaflets)
hist(na.omit(inga$Avg_leaflet_size_rectangle_.cm2._terminal))
hist(na.omit(inga$Avg_leaflet_size_rectangle_.cm2._basal))
hist(na.omit(inga$Avg_leaf_size_.cm2.))
hist(inga$Avg..length.flower.peduncle..mm.)
hist(inga$Avg.length.legume..cm.)
hist(inga$min.Elevational.range..m.)
hist(inga$max.Elevational.range..m.)
hist(inga$Height.of.tree..m.)
hist(inga$Diameter.of.tree..cm.)
hist(inga$SLA)
hist(inga$SiO2)
hist(inga$N_leaf)
hist(inga$C_leaf)
hist(inga$P_leaf)
hist(inga$chlorophyll)
hist(inga$expansion)
hist(inga$X.ants.on.m2.of.leaf)
hist(inga$X.ants.EFN)
hist(na.omit(inga$wood.density))
hist(na.omit(inga$Field_max_height_.m.))
hist(na.omit(inga$Field_leaflet_area_.cm2.))
hist(na.omit(inga$X2_Field_leflet_area_.cm2.))
hist(na.omit(inga$Avg_FLA_.cm2.))


#pairwise correlations, let's check and see if basal leaflet size is correlated with any of the traits kyle 
#measured in the field
#this will allow us to see if this trait from the monograph can 'stand in' for a field measured trait

#checking pairwise relationships visually 
#simple linear modles 

# SLA vs Avg leaflet size rectangle cm2 basal 
plot(SLA~Avg_leaflet_size_rectangle_.cm2._basal,data=inga)
#this plot shows a relationship with one outlier with high leaflet size and high SLA

# Avg no leaflets vs ...
plot(Avg..no..leaflets~Avg_leaflet_size_rectangle_.cm2._terminal,data=inga)
summary(lm(Avg..no..leaflets~Avg_leaflet_size_rectangle_.cm2._terminal,data=inga))

plot(Avg..no..leaflets~Avg_leaflet_size_rectangle_.cm2._basal,data=inga)
summary(lm(Avg..no..leaflets~Avg_leaflet_size_rectangle_.cm2._basal,data=inga))

plot(Avg..no..leaflets~Avg_leaf_size_.cm2.,data=inga)
summary(lm(Avg..no..leaflets~Avg_leaf_size_.cm2.,data=inga))

plot(inga$Avg..no..leaflets~inga$Avg..length.flower.peduncle..mm.)
summary(lm(inga$Avg..no..leaflets~inga$Avg..length.flower.peduncle..mm.))

plot(inga$Avg..no..leaflets~inga$Avg.length.legume..cm.)
summary(lm(inga$Avg..no..leaflets~inga$Avg.length.legume..cm.))

plot(inga$Avg..no..leaflets~inga$Montane..rain..forest)
summary(lm(inga$Avg..no..leaflets~inga$Montane..rain..forest))

plot(inga$Avg..no..leaflets~inga$Lowland..rain..forest)
summary(lm(inga$Avg..no..leaflets~inga$Lowland..rain..forest))

plot(inga$Avg..no..leaflets~inga$cloud.forest)
summary(lm(inga$Avg..no..leaflets~inga$cloud.forest))

plot(inga$Avg..no..leaflets~inga$Flooded.Periodically.flooded.sites)
summary(lm(inga$Avg..no..leaflets~inga$Flooded.Periodically.flooded.sites))

plot(inga$Avg..no..leaflets~inga$non.flooded.sites)
summary(lm(inga$Avg..no..leaflets~inga$non.flooded.sites))

plot(inga$Avg..no..leaflets~inga$Primary.forest)
summary(lm(inga$Avg..no..leaflets~inga$Primary.forest))

plot(inga$Avg..no..leaflets~inga$Secondary.forest)
summary(lm(inga$Avg..no..leaflets~inga$Secondary.forest))

plot(inga$Avg..no..leaflets~inga$Disturbed.forest)
summary(lm(inga$Avg..no..leaflets~inga$Disturbed.forest))

plot(inga$Avg..no..leaflets~inga$Undisturbed.forest)
summary(lm(inga$Avg..no..leaflets~inga$Undisturbed.forest))

plot(inga$Avg..no..leaflets~inga$min.Elevational.range..m.)
summary(lm(inga$Avg..no..leaflets~inga$min.Elevational.range..m.))

plot(inga$Avg..no..leaflets~inga$max.Elevational.range..m.)
summary(lm(inga$Avg..no..leaflets~inga$max.Elevational.range..m.))

plot(inga$Avg..no..leaflets~inga$Height.of.tree..m.)
summary(lm(inga$Avg..no..leaflets~inga$Height.of.tree..m.))

plot(inga$Avg..no..leaflets~inga$Diameter.of.tree..cm.)
summary(lm(inga$Avg..no..leaflets~inga$Diameter.of.tree..cm.))

plot(inga$Avg..no..leaflets~inga$Conspectus)
summary(lm(inga$Avg..no..leaflets~inga$Conspectus))

plot(inga$Avg..no..leaflets~inga$SLA)
summary(lm(inga$Avg..no..leaflets~inga$SLA))

plot(inga$Avg..no..leaflets~inga$SiO2)
summary(lm(inga$Avg..no..leaflets~inga$SiO2))

plot(inga$Avg..no..leaflets~inga$N_leaf)
summary(lm(inga$Avg..no..leaflets~inga$N_leaf))

plot(inga$Avg..no..leaflets~inga$C_leaf)
summary(lm(inga$Avg..no..leaflets~inga$C_leaf))

plot(inga$Avg..no..leaflets~inga$P_leaf)
summary(lm(inga$Avg..no..leaflets~inga$P_leaf))

plot(inga$Avg..no..leaflets~inga$chlorophyll)
summary(lm(inga$Avg..no..leaflets~inga$chlorophyll))

plot(inga$Avg..no..leaflets~inga$expansion)
summary(lm(inga$Avg..no..leaflets~inga$expansion))

plot(inga$Avg..no..leaflets~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$Avg..no..leaflets~inga$X.ants.on.m2.of.leaf))

plot(inga$Avg..no..leaflets~inga$X.ants.EFN)
summary(lm(inga$Avg..no..leaflets~inga$X.ants.EFN))

plot(inga$Avg..no..leaflets~inga$wood.density)
summary(lm(inga$Avg..no..leaflets~inga$wood.density))

plot(inga$Avg..no..leaflets~inga$Field_max_height_.m.)
summary(lm(inga$Avg..no..leaflets~inga$Field_max_height_.m.))

plot(inga$Avg..no..leaflets~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$Avg..no..leaflets~inga$Field_leaflet_area_.cm2.))

plot(inga$Avg..no..leaflets~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$Avg..no..leaflets~inga$X2_Field_leflet_area_.cm2.))

plot(inga$Avg..no..leaflets~inga$Avg_FLA_.cm2.)
summary(lm(inga$Avg..no..leaflets~inga$Avg_FLA_.cm2.))

# Avg leaflet size rectangle cm2 ~ ...

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Avg_leaflet_size_rectangle_.cm2._basal)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Avg_leaflet_size_rectangle_.cm2._basal))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Avg_leaf_size_.cm2.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Avg_leaf_size_.cm2.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Avg..length.flower.peduncle..mm.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Avg..length.flower.peduncle..mm.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Avg.length.legume..cm.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Avg.length.legume..cm.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Montane..rain..forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Montane..rain..forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Lowland..rain..forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Lowland..rain..forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$cloud.forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$cloud.forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Flooded.Periodically.flooded.sites)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Flooded.Periodically.flooded.sites))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$non.flooded.sites)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$non.flooded.sites))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Primary.forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Primary.forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Secondary.forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Secondary.forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Disturbed.forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Disturbed.forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Undisturbed.forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Undisturbed.forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$min.Elevational.range..m.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$min.Elevational.range..m.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$max.Elevational.range..m.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$max.Elevational.range..m.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Height.of.tree..m.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Height.of.tree..m.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Diameter.of.tree..cm.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Diameter.of.tree..cm.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Conspectus)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Conspectus))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$SLA)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$SLA))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$SiO2)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$SiO2))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$N_leaf)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$N_leaf))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$C_leaf)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$C_leaf))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$P_leaf)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$P_leaf))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$chlorophyll)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$chlorophyll))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$expansion)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$expansion))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$X.ants.on.m2.of.leaf))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$X.ants.EFN)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$X.ants.EFN))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$wood.density)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$wood.density))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Field_max_height_.m.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Field_max_height_.m.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Field_leaflet_area_.cm2.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$X2_Field_leflet_area_.cm2.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Avg_FLA_.cm2.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._terminal~inga$Avg_FLA_.cm2.))

# Avg leaflet area basal cm2 ~ ...

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Avg_leaf_size_.cm2.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Avg_leaf_size_.cm2.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Avg..length.flower.peduncle..mm.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Avg..length.flower.peduncle..mm.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Avg.length.legume..cm.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Avg.length.legume..cm.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Montane..rain..forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Montane..rain..forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Lowland..rain..forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Lowland..rain..forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$cloud.forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$cloud.forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Flooded.Periodically.flooded.sites)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Flooded.Periodically.flooded.sites))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$non.flooded.sites)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$non.flooded.sites))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Primary.forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Primary.forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Secondary.forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Secondary.forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Disturbed.forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Disturbed.forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Undisturbed.forest)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Undisturbed.forest))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$min.Elevational.range..m.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$min.Elevational.range..m.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$max.Elevational.range..m.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$max.Elevational.range..m.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Height.of.tree..m.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Height.of.tree..m.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Diameter.of.tree..cm.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Diameter.of.tree..cm.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Conspectus)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Conspectus))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$SLA)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$SLA))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$SiO2)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$SiO2))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$N_leaf)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$N_leaf))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$C_leaf)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$C_leaf))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$P_leaf)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$P_leaf))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$chlorophyll)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$chlorophyll))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$expansion)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$expansion))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$X.ants.on.m2.of.leaf))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$X.ants.EFN)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$X.ants.EFN))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$wood.density)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$wood.density))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Field_max_height_.m.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Field_max_height_.m.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Field_leaflet_area_.cm2.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$X2_Field_leflet_area_.cm2.))

plot(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Avg_FLA_.cm2.)
summary(lm(inga$Avg_leaflet_size_rectangle_.cm2._basal~inga$Avg_FLA_.cm2.))

# Avg leaf size ~ ...

plot(inga$Avg_leaf_size_.cm2.~inga$Avg..length.flower.peduncle..mm.)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Avg..length.flower.peduncle..mm.))

plot(inga$Avg_leaf_size_.cm2.~inga$Avg.length.legume..cm.)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Avg.length.legume..cm.))

plot(inga$Avg_leaf_size_.cm2.~inga$Montane..rain..forest)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Montane..rain..forest))

plot(inga$Avg_leaf_size_.cm2.~inga$Lowland..rain..forest)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Lowland..rain..forest))

plot(inga$Avg_leaf_size_.cm2.~inga$cloud.forest)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$cloud.forest))

plot(inga$Avg_leaf_size_.cm2.~inga$Flooded.Periodically.flooded.sites)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Flooded.Periodically.flooded.sites))

plot(inga$Avg_leaf_size_.cm2.~inga$non.flooded.sites)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$non.flooded.sites))

plot(inga$Avg_leaf_size_.cm2.~inga$Primary.forest)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Primary.forest))

plot(inga$Avg_leaf_size_.cm2.~inga$Secondary.forest)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Secondary.forest))

plot(inga$Avg_leaf_size_.cm2.~inga$Disturbed.forest)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Disturbed.forest))

plot(inga$Avg_leaf_size_.cm2.~inga$Undisturbed.forest)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Undisturbed.forest))

plot(inga$Avg_leaf_size_.cm2.~inga$min.Elevational.range..m.)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$min.Elevational.range..m.))

plot(inga$Avg_leaf_size_.cm2.~inga$max.Elevational.range..m.)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$max.Elevational.range..m.))

plot(inga$Avg_leaf_size_.cm2.~inga$Height.of.tree..m.)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Height.of.tree..m.))

plot(inga$Avg_leaf_size_.cm2.~inga$Diameter.of.tree..cm.)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Diameter.of.tree..cm.))

plot(inga$Avg_leaf_size_.cm2.~inga$Conspectus)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Conspectus))

plot(inga$Avg_leaf_size_.cm2.~inga$SLA)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$SLA))

plot(inga$Avg_leaf_size_.cm2.~inga$SiO2)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$SiO2))

plot(inga$Avg_leaf_size_.cm2.~inga$N_leaf)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$N_leaf))

plot(inga$Avg_leaf_size_.cm2.~inga$C_leaf)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$C_leaf))

plot(inga$Avg_leaf_size_.cm2.~inga$P_leaf)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$P_leaf))

plot(inga$Avg_leaf_size_.cm2.~inga$chlorophyll)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$chlorophyll))

plot(inga$Avg_leaf_size_.cm2.~inga$expansion)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$expansion))

plot(inga$Avg_leaf_size_.cm2.~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$X.ants.on.m2.of.leaf))

plot(inga$Avg_leaf_size_.cm2.~inga$X.ants.EFN)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$X.ants.EFN))

plot(inga$Avg_leaf_size_.cm2.~inga$wood.density)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$wood.density))

plot(inga$Avg_leaf_size_.cm2.~inga$Field_max_height_.m.)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Field_max_height_.m.))

plot(inga$Avg_leaf_size_.cm2.~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Field_leaflet_area_.cm2.))

plot(inga$Avg_leaf_size_.cm2.~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$X2_Field_leflet_area_.cm2.))

plot(inga$Avg_leaf_size_.cm2.~inga$Avg_FLA_.cm2.)
summary(lm(inga$Avg_leaf_size_.cm2.~inga$Avg_FLA_.cm2.))

# Avg length flower peduncle mm ~ ...

plot(inga$Avg..length.flower.peduncle..mm.~inga$Avg.length.legume..cm.)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Avg.length.legume..cm.))

plot(inga$Avg..length.flower.peduncle..mm.~inga$Montane..rain..forest)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Montane..rain..forest))

plot(inga$Avg..length.flower.peduncle..mm.~inga$Lowland..rain..forest)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Lowland..rain..forest))

plot(inga$Avg..length.flower.peduncle..mm.~inga$cloud.forest)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$cloud.forest))

plot(inga$Avg..length.flower.peduncle..mm.~inga$Flooded.Periodically.flooded.sites)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Flooded.Periodically.flooded.sites))

plot(inga$Avg..length.flower.peduncle..mm.~inga$non.flooded.sites)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$non.flooded.sites))

plot(inga$Avg..length.flower.peduncle..mm.~inga$Primary.forest)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Primary.forest))

plot(inga$Avg..length.flower.peduncle..mm.~inga$Secondary.forest)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Secondary.forest))

plot(inga$Avg..length.flower.peduncle..mm.~inga$Disturbed.forest)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Disturbed.forest))

plot(inga$Avg..length.flower.peduncle..mm.~inga$Undisturbed.forest)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Undisturbed.forest))

plot(inga$Avg..length.flower.peduncle..mm.~inga$min.Elevational.range..m.)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$min.Elevational.range..m.))

plot(inga$Avg..length.flower.peduncle..mm.~inga$max.Elevational.range..m.)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$max.Elevational.range..m.))

plot(inga$Avg..length.flower.peduncle..mm.~inga$Height.of.tree..m.)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Height.of.tree..m.))

plot(inga$Avg..length.flower.peduncle..mm.~inga$Diameter.of.tree..cm.)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Diameter.of.tree..cm.))

plot(inga$Avg..length.flower.peduncle..mm.~inga$Conspectus)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Conspectus))

plot(inga$Avg..length.flower.peduncle..mm.~inga$SLA)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$SLA))

plot(inga$Avg..length.flower.peduncle..mm.~inga$SiO2)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$SiO2))

plot(inga$Avg..length.flower.peduncle..mm.~inga$N_leaf)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$N_leaf))

plot(inga$Avg..length.flower.peduncle..mm.~inga$C_leaf)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$C_leaf))

plot(inga$Avg..length.flower.peduncle..mm.~inga$P_leaf)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$P_leaf))

plot(inga$Avg..length.flower.peduncle..mm.~inga$chlorophyll)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$chlorophyll))

plot(inga$Avg..length.flower.peduncle..mm.~inga$expansion)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$expansion))

plot(inga$Avg..length.flower.peduncle..mm.~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$X.ants.on.m2.of.leaf))

plot(inga$Avg..length.flower.peduncle..mm.~inga$X.ants.EFN)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$X.ants.EFN))

plot(inga$Avg..length.flower.peduncle..mm.~inga$wood.density)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$wood.density))

plot(inga$Avg..length.flower.peduncle..mm.~inga$Field_max_height_.m.)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Field_max_height_.m.))

plot(inga$Avg..length.flower.peduncle..mm.~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Field_leaflet_area_.cm2.))

plot(inga$Avg..length.flower.peduncle..mm.~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$X2_Field_leflet_area_.cm2.))

plot(inga$Avg..length.flower.peduncle..mm.~inga$Avg_FLA_.cm2.)
summary(lm(inga$Avg..length.flower.peduncle..mm.~inga$Avg_FLA_.cm2.))

# Avg legume length cm ~ ...

plot(inga$Avg.length.legume..cm.~inga$Montane..rain..forest)
summary(lm(inga$Avg.length.legume..cm.~inga$Montane..rain..forest))

plot(inga$Avg.length.legume..cm.~inga$Lowland..rain..forest)
summary(lm(inga$Avg.length.legume..cm.~inga$Lowland..rain..forest))

plot(inga$Avg.length.legume..cm.~inga$cloud.forest)
summary(lm(inga$Avg.length.legume..cm.~inga$cloud.forest))

plot(inga$Avg.length.legume..cm.~inga$Flooded.Periodically.flooded.sites)
summary(lm(inga$Avg.length.legume..cm.~inga$Flooded.Periodically.flooded.sites))

plot(inga$Avg.length.legume..cm.~inga$non.flooded.sites)
summary(lm(inga$Avg.length.legume..cm.~inga$non.flooded.sites))

plot(inga$Avg.length.legume..cm.~inga$Primary.forest)
summary(lm(inga$Avg.length.legume..cm.~inga$Primary.forest))

plot(inga$Avg.length.legume..cm.~inga$Secondary.forest)
summary(lm(inga$Avg.length.legume..cm.~inga$Secondary.forest))

plot(inga$Avg.length.legume..cm.~inga$Disturbed.forest)
summary(lm(inga$Avg.length.legume..cm.~inga$Disturbed.forest))

plot(inga$Avg.length.legume..cm.~inga$Undisturbed.forest)
summary(lm(inga$Avg.length.legume..cm.~inga$Undisturbed.forest))

plot(inga$Avg.length.legume..cm.~inga$min.Elevational.range..m.)
summary(lm(inga$Avg.length.legume..cm.~inga$min.Elevational.range..m.))

plot(inga$Avg.length.legume..cm.~inga$max.Elevational.range..m.)
summary(lm(inga$Avg.length.legume..cm.~inga$max.Elevational.range..m.))

plot(inga$Avg.length.legume..cm.~inga$Height.of.tree..m.)
summary(lm(inga$Avg.length.legume..cm.~inga$Height.of.tree..m.))

plot(inga$Avg.length.legume..cm.~inga$Diameter.of.tree..cm.)
summary(lm(inga$Avg.length.legume..cm.~inga$Diameter.of.tree..cm.))

plot(inga$Avg.length.legume..cm.~inga$Conspectus)
summary(lm(inga$Avg.length.legume..cm.~inga$Conspectus))

plot(inga$Avg.length.legume..cm.~inga$SLA)
summary(lm(inga$Avg.length.legume..cm.~inga$SLA))

plot(inga$Avg.length.legume..cm.~inga$SiO2)
summary(lm(inga$Avg.length.legume..cm.~inga$SiO2))

plot(inga$Avg.length.legume..cm.~inga$N_leaf)
summary(lm(inga$Avg.length.legume..cm.~inga$N_leaf))

plot(inga$Avg.length.legume..cm.~inga$C_leaf)
summary(lm(inga$Avg.length.legume..cm.~inga$C_leaf))

plot(inga$Avg.length.legume..cm.~inga$P_leaf)
summary(lm(inga$Avg.length.legume..cm.~inga$P_leaf))

plot(inga$Avg.length.legume..cm.~inga$chlorophyll)
summary(lm(inga$Avg.length.legume..cm.~inga$chlorophyll))

plot(inga$Avg.length.legume..cm.~inga$expansion)
summary(lm(inga$Avg.length.legume..cm.~inga$expansion))

plot(inga$Avg.length.legume..cm.~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$Avg.length.legume..cm.~inga$X.ants.on.m2.of.leaf))

plot(inga$Avg.length.legume..cm.~inga$X.ants.EFN)
summary(lm(inga$Avg.length.legume..cm.~inga$X.ants.EFN))

plot(inga$Avg.length.legume..cm.~inga$wood.density)
summary(lm(inga$Avg.length.legume..cm.~inga$wood.density))

plot(inga$Avg.length.legume..cm.~inga$Field_max_height_.m.)
summary(lm(inga$Avg.length.legume..cm.~inga$Field_max_height_.m.))

plot(inga$Avg.length.legume..cm.~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$Avg.length.legume..cm.~inga$Field_leaflet_area_.cm2.))

plot(inga$Avg.length.legume..cm.~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$Avg.length.legume..cm.~inga$X2_Field_leflet_area_.cm2.))

plot(inga$Avg.length.legume..cm.~inga$Avg_FLA_.cm2.)
summary(lm(inga$Avg.length.legume..cm.~inga$Avg_FLA_.cm2.))

# Montane rain forest ~ ... note chi squared for association tests used with two 
# discrete variables

plot(inga$Montane..rain..forest~inga$Lowland..rain..forest)
chisq.test(table(inga$Montane..rain..forest, inga$Lowland..rain..forest))

plot(inga$Montane..rain..forest~inga$cloud.forest)
chisq.test(table(inga$Montane..rain..forest, inga$cloud.forest))

plot(inga$Montane..rain..forest~inga$Flooded.Periodically.flooded.sites)
chisq.test(table(inga$Montane..rain..forest, inga$Flooded.Periodically.flooded.sites))

plot(inga$Montane..rain..forest~inga$non.flooded.sites)
chisq.test(table(inga$Montane..rain..forest, inga$non.flooded.sites))

plot(inga$Montane..rain..forest~inga$Primary.forest)
chisq.test(table(inga$Montane..rain..forest, inga$Primary.forest))

plot(inga$Montane..rain..forest~inga$Secondary.forest)
chisq.test(table(inga$Montane..rain..forest, inga$Secondary.forest))

plot(inga$Montane..rain..forest~inga$Disturbed.forest)
chisq.test(table(inga$Montane..rain..forest, inga$Disturbed.forest))

plot(inga$Montane..rain..forest~inga$Undisturbed.forest)
chisq.test(table(inga$Montane..rain..forest, inga$Undisturbed.forest))

# general linear model for carrying out a logistic regression 
plot(inga$Montane..rain..forest~inga$min.Elevational.range..m.)
summary(glm(Montane..rain..forest ~ min.Elevational.range..m., 
            data = inga, family = binomial))

plot(inga$Montane..rain..forest~inga$max.Elevational.range..m.)
summary(glm(Montane..rain..forest ~ max.Elevational.range..m., 
            data = inga, family = binomial))

plot(inga$Montane..rain..forest~inga$Height.of.tree..m.)
summary(glm(Montane..rain..forest ~ Height.of.tree..m., 
            data = inga, family = binomial))

plot(inga$Montane..rain..forest~inga$Diameter.of.tree..cm.)
summary(glm(Montane..rain..forest ~ Diameter.of.tree..cm., 
            data = inga, family = binomial))

plot(inga$Montane..rain..forest~inga$Conspectus)
chisq.test(table(inga$Montane..rain..forest, inga$Conspectus))

plot(inga$Montane..rain..forest~inga$SLA)
summary(glm(inga$Montane..rain..forest ~ inga$SLA, 
            family = binomial))

plot(inga$Montane..rain..forest~inga$SiO2)
summary(glm(inga$Montane..rain..forest ~ inga$SiO2, 
            family = binomial))

plot(inga$Montane..rain..forest~inga$N_leaf)
summary(glm(inga$Montane..rain..forest ~ inga$N_leaf, 
            family = binomial))

plot(inga$Montane..rain..forest~inga$C_leaf)
summary(glm(inga$Montane..rain..forest ~ inga$C_leaf, 
            family = binomial))

plot(inga$Montane..rain..forest~inga$P_leaf)
summary(glm(inga$Montane..rain..forest ~ inga$P_leaf, 
            family = binomial))

plot(inga$Montane..rain..forest~inga$chlorophyll)
summary(glm(inga$Montane..rain..forest ~ inga$chlorophyll, 
            family = binomial))

plot(inga$Montane..rain..forest~inga$expansion)
summary(glm(inga$Montane..rain..forest ~ inga$expansion, 
            family = binomial))

plot(inga$Montane..rain..forest~inga$X.ants.on.m2.of.leaf)
summary(glm(inga$Montane..rain..forest ~ inga$X.ants.on.m2.of.leaf, 
            family = binomial))

plot(inga$Montane..rain..forest~inga$X.ants.EFN)
summary(glm(inga$Montane..rain..forest ~ inga$X.ants.EFN, 
            family = binomial))

plot(inga$Montane..rain..forest~inga$wood.density)
summary(glm(inga$Montane..rain..forest ~ inga$wood.density, 
            family = binomial))

plot(inga$Montane..rain..forest~inga$Field_max_height_.m.)
summary(glm(inga$Montane..rain..forest ~ inga$Field_max_height_.m., 
            family = binomial))

plot(inga$Montane..rain..forest~inga$Field_leaflet_area_.cm2.)
summary(glm(inga$Montane..rain..forest ~ inga$Field_leaflet_area_.cm2., 
            family = binomial))

plot(inga$Montane..rain..forest~inga$X2_Field_leflet_area_.cm2.)
summary(glm(inga$Montane..rain..forest ~ inga$X2_Field_leflet_area_.cm2., 
            family = binomial))

plot(inga$Montane..rain..forest~inga$Avg_FLA_.cm2.)
summary(glm(inga$Montane..rain..forest ~ inga$Avg_FLA_.cm2., 
            family = binomial))

# Lowland rain forest ~ ...

plot(inga$Lowland..rain..forest~inga$cloud.forest)
chisq.test(table(inga$Lowland..rain..forest, inga$cloud.forest))

plot(inga$Lowland..rain..forest~inga$Flooded.Periodically.flooded.sites)
chisq.test(table(inga$Lowland..rain..forest, inga$Flooded.Periodically.flooded.sites))

plot(inga$Lowland..rain..forest~inga$non.flooded.sites)
chisq.test(table(inga$Lowland..rain..forest, inga$non.flooded.sites))

plot(inga$Lowland..rain..forest~inga$Primary.forest)
chisq.test(table(inga$Lowland..rain..forest, inga$Primary.forest))

plot(inga$Lowland..rain..forest~inga$Secondary.forest)
chisq.test(table(inga$Lowland..rain..forest, inga$Secondary.forest))

plot(inga$Lowland..rain..forest~inga$Disturbed.forest)
chisq.test(table(inga$Lowland..rain..forest, inga$Disturbed.forest))

plot(inga$Lowland..rain..forest~inga$Undisturbed.forest)
chisq.test(table(inga$Lowland..rain..forest, inga$Undisturbed.forest))

plot(inga$Lowland..rain..forest~inga$min.Elevational.range..m.)
summary(glm(inga$Lowland..rain..forest ~ inga$min.Elevational.range..m., 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$max.Elevational.range..m.)
summary(glm(inga$Lowland..rain..forest ~ inga$max.Elevational.range..m., 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$Height.of.tree..m.)
summary(glm(inga$Lowland..rain..forest ~ inga$Height.of.tree..m., 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$Diameter.of.tree..cm.)
summary(glm(inga$Lowland..rain..forest ~ inga$Diameter.of.tree..cm., 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$Conspectus)
chisq.test(table(inga$Lowland..rain..forest, inga$Conspectus))

plot(inga$Lowland..rain..forest~inga$SLA)
summary(glm(inga$Lowland..rain..forest ~ inga$SLA, 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$SiO2)
summary(glm(inga$Lowland..rain..forest ~ inga$SiO2, 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$N_leaf)
summary(glm(inga$Lowland..rain..forest ~ inga$N_leaf, 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$C_leaf)
summary(glm(inga$Lowland..rain..forest ~ inga$C_leaf, 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$P_leaf)
summary(glm(inga$Lowland..rain..forest ~ inga$P_leaf, 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$chlorophyll)
summary(glm(inga$Lowland..rain..forest ~ inga$chlorophyll, 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$expansion)
summary(glm(inga$Lowland..rain..forest ~ inga$expansion, 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$X.ants.on.m2.of.leaf)
summary(glm(inga$Lowland..rain..forest ~ inga$X.ants.on.m2.of.leaf, 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$X.ants.EFN)
summary(glm(inga$Lowland..rain..forest ~ inga$X.ants.EFN, 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$wood.density)
summary(glm(inga$Lowland..rain..forest ~ inga$wood.density, 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$Field_max_height_.m.)
summary(glm(inga$Lowland..rain..forest ~ inga$Field_max_height_.m., 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$Field_leaflet_area_.cm2.)
summary(glm(inga$Lowland..rain..forest ~ inga$Field_leaflet_area_.cm2., 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$X2_Field_leflet_area_.cm2.)
summary(glm(inga$Lowland..rain..forest ~ inga$X2_Field_leflet_area_.cm2., 
            family = binomial))

plot(inga$Lowland..rain..forest~inga$Avg_FLA_.cm2.)
summary(glm(inga$Lowland..rain..forest ~ inga$Avg_FLA_.cm2., 
            family = binomial))

# cloud forest ~ ...

plot(inga$cloud.forest~inga$Flooded.Periodically.flooded.sites)
chisq.test(table(inga$cloud.forest, inga$Flooded.Periodically.flooded.sites))

plot(inga$cloud.forest~inga$non.flooded.sites)
chisq.test(table(inga$cloud.forest, inga$non.flooded.sites))

plot(inga$cloud.forest~inga$Primary.forest)
chisq.test(table(inga$cloud.forest, inga$Primary.forest))

plot(inga$cloud.forest~inga$Secondary.forest)
chisq.test(table(inga$cloud.forest, inga$Secondary.forest))

plot(inga$cloud.forest~inga$Disturbed.forest)
chisq.test(table(inga$cloud.forest, inga$Disturbed.forest))

plot(inga$cloud.forest~inga$Undisturbed.forest)
chisq.test(table(inga$cloud.forest, inga$Undisturbed.forest))

plot(inga$cloud.forest~inga$min.Elevational.range..m.)
summary(glm(inga$cloud.forest ~ inga$min.Elevational.range..m., 
            family = binomial))

plot(inga$cloud.forest~inga$max.Elevational.range..m.)
summary(glm(inga$cloud.forest ~ inga$max.Elevational.range..m., 
            family = binomial))

plot(inga$cloud.forest~inga$Height.of.tree..m.)
summary(glm(inga$cloud.forest ~ inga$Height.of.tree..m., 
            family = binomial))

plot(inga$cloud.forest~inga$Diameter.of.tree..cm.)
summary(glm(inga$cloud.forest ~ inga$Diameter.of.tree..cm., 
            family = binomial))

plot(inga$cloud.forest~inga$Conspectus)
chisq.test(table(inga$cloud.forest, inga$Conspectus))

plot(inga$cloud.forest~inga$SLA)
summary(glm(inga$cloud.forest ~ inga$SLA, 
            family = binomial))

plot(inga$cloud.forest~inga$SiO2)
summary(glm(inga$cloud.forest ~ inga$SiO2, 
            family = binomial))

plot(inga$cloud.forest~inga$N_leaf)
summary(glm(inga$cloud.forest ~ inga$N_leaf, 
            family = binomial))

plot(inga$cloud.forest~inga$C_leaf)
summary(glm(inga$cloud.forest ~ inga$C_leaf, 
            family = binomial))

plot(inga$cloud.forest~inga$P_leaf)
summary(glm(inga$cloud.forest ~ inga$P_leaf, 
            family = binomial))

plot(inga$cloud.forest~inga$chlorophyll)
summary(glm(inga$cloud.forest ~ inga$chlorophyll, 
            family = binomial))

plot(inga$cloud.forest~inga$expansion)
summary(glm(inga$cloud.forest ~ inga$expansion, 
            family = binomial))

plot(inga$cloud.forest~inga$X.ants.on.m2.of.leaf)
summary(glm(inga$cloud.forest ~ inga$X.ants.on.m2.of.leaf, 
            family = binomial))

plot(inga$cloud.forest~inga$X.ants.EFN)
summary(glm(inga$cloud.forest ~ inga$X.ants.EFN, 
            family = binomial))

plot(inga$cloud.forest~inga$wood.density)
summary(glm(inga$cloud.forest ~ inga$wood.density, 
            family = binomial))

plot(inga$cloud.forest~inga$Field_max_height_.m.)
summary(glm(inga$cloud.forest ~ inga$Field_max_height_.m., 
            family = binomial))

plot(inga$cloud.forest~inga$Field_leaflet_area_.cm2.)
summary(glm(inga$cloud.forest ~ inga$Field_leaflet_area_.cm2., 
            family = binomial))

plot(inga$cloud.forest~inga$X2_Field_leflet_area_.cm2.)
summary(glm(inga$cloud.forest ~ inga$X2_Field_leflet_area_.cm2., 
            family = binomial))

plot(inga$cloud.forest~inga$Avg_FLA_.cm2.)
summary(glm(inga$cloud.forest ~ inga$Avg_FLA_.cm2., 
            family = binomial))

# Flooded/periodically flodoed sites ~ ... 

plot(inga$Flooded.Periodically.flooded.sites~inga$non.flooded.sites)
chisq.test(table(inga$Flooded.Periodically.flooded.sites, inga$non.flooded.sites))

plot(inga$Flooded.Periodically.flooded.sites~inga$Primary.forest)
chisq.test(table(inga$Flooded.Periodically.flooded.sites, inga$Primary.forest))

plot(inga$Flooded.Periodically.flooded.sites~inga$Secondary.forest)
chisq.test(table(inga$Flooded.Periodically.flooded.sites, inga$Secondary.forest))

plot(inga$Flooded.Periodically.flooded.sites~inga$Disturbed.forest)
chisq.test(table(inga$Flooded.Periodically.flooded.sites, inga$Disturbed.forest))

plot(inga$Flooded.Periodically.flooded.sites~inga$Undisturbed.forest)
chisq.test(table(inga$Flooded.Periodically.flooded.sites, inga$Undisturbed.forest))

plot(inga$Flooded.Periodically.flooded.sites~inga$min.Elevational.range..m.)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$min.Elevational.range..m., 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$max.Elevational.range..m.)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$max.Elevational.range..m., 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$Height.of.tree..m.)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$Height.of.tree..m., 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$Diameter.of.tree..cm.)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$Diameter.of.tree..cm., 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$Conspectus)
chisq.test(table(inga$Flooded.Periodically.flooded.sites, inga$Conspectus))

plot(inga$Flooded.Periodically.flooded.sites~inga$SLA)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$SLA, 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$SiO2)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$SiO2, 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$N_leaf)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$N_leaf, 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$C_leaf)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$C_leaf, 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$P_leaf)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$P_leaf, 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$chlorophyll)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$chlorophyll, 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$expansion)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$expansion, 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$X.ants.on.m2.of.leaf)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$X.ants.on.m2.of.leaf, 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$X.ants.EFN)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$X.ants.EFN, 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$wood.density)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$wood.density, 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$Field_max_height_.m.)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$Field_max_height_.m., 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$Field_leaflet_area_.cm2.)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$Field_leaflet_area_.cm2., 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$X2_Field_leflet_area_.cm2.)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$X2_Field_leflet_area_.cm2., 
            family = binomial))

plot(inga$Flooded.Periodically.flooded.sites~inga$Avg_FLA_.cm2.)
summary(glm(inga$Flooded.Periodically.flooded.sites ~ inga$Avg_FLA_.cm2., 
            family = binomial))

# non flooded sites ~ ...

plot(inga$non.flooded.sites~inga$Primary.forest)
chisq.test(table(inga$non.flooded.sites, inga$Primary.forest))

plot(inga$non.flooded.sites~inga$Secondary.forest)
chisq.test(table(inga$non.flooded.sites, inga$Secondary.forest))

plot(inga$non.flooded.sites~inga$Disturbed.forest)
chisq.test(table(inga$non.flooded.sites, inga$Disturbed.forest))

plot(inga$non.flooded.sites~inga$Primary.forest)
chisq.test(table(inga$non.flooded.sites, inga$Undisturbed.forest))

plot(inga$non.flooded.sites~inga$min.Elevational.range..m.)
summary(glm(inga$non.flooded.sites ~ inga$min.Elevational.range..m., 
            family = binomial))

plot(inga$non.flooded.sites~inga$max.Elevational.range..m.)
summary(glm(inga$non.flooded.sites ~ inga$max.Elevational.range..m., 
            family = binomial))

plot(inga$non.flooded.sites~inga$Height.of.tree..m.)
summary(glm(inga$non.flooded.sites ~ inga$Height.of.tree..m., 
            family = binomial))

plot(inga$non.flooded.sites~inga$Diameter.of.tree..cm.)
summary(glm(inga$non.flooded.sites ~ inga$Diameter.of.tree..cm., 
            family = binomial))

plot(inga$non.flooded.sites~inga$Conspectus)
chisq.test(table(inga$non.flooded.sites, inga$Conspectus))

plot(inga$non.flooded.sites~inga$SLA)
summary(glm(inga$non.flooded.sites ~ inga$SLA, 
            family = binomial))

plot(inga$non.flooded.sites~inga$SiO2)
summary(glm(inga$non.flooded.sites ~ inga$SiO2, 
            family = binomial))

plot(inga$non.flooded.sites~inga$N_leaf)
summary(glm(inga$non.flooded.sites ~ inga$N_leaf, 
            family = binomial))

plot(inga$non.flooded.sites~inga$C_leaf)
summary(glm(inga$non.flooded.sites ~ inga$C_leaf, 
            family = binomial))

plot(inga$non.flooded.sites~inga$P_leaf)
summary(glm(inga$non.flooded.sites ~ inga$P_leaf, 
            family = binomial))

plot(inga$non.flooded.sites~inga$chlorophyll)
summary(glm(inga$non.flooded.sites ~ inga$chlorophyll, 
            family = binomial))

plot(inga$non.flooded.sites~inga$expansion)
summary(glm(inga$non.flooded.sites ~ inga$expansion, 
            family = binomial))

plot(inga$non.flooded.sites~inga$X.ants.on.m2.of.leaf)
summary(glm(inga$non.flooded.sites ~ inga$X.ants.on.m2.of.leaf, 
            family = binomial))

plot(inga$non.flooded.sites~inga$X.ants.EFN)
summary(glm(inga$non.flooded.sites ~ inga$X.ants.EFN, 
            family = binomial))

plot(inga$non.flooded.sites~inga$wood.density)
summary(glm(inga$non.flooded.sites ~ inga$wood.density, 
            family = binomial))

plot(inga$non.flooded.sites~inga$Field_max_height_.m.)
summary(glm(inga$non.flooded.sites ~ inga$Field_max_height_.m., 
            family = binomial))

plot(inga$non.flooded.sites~inga$Field_leaflet_area_.cm2.)
summary(glm(inga$non.flooded.sites ~ inga$Field_leaflet_area_.cm2., 
            family = binomial))

plot(inga$non.flooded.sites~inga$X2_Field_leflet_area_.cm2.)
summary(glm(inga$non.flooded.sites ~ inga$X2_Field_leflet_area_.cm2., 
            family = binomial))

plot(inga$non.flooded.sites~inga$Avg_FLA_.cm2.)
summary(glm(inga$non.flooded.sites ~ inga$Avg_FLA_.cm2., 
            family = binomial))

# Primary forest ~ ...

plot(inga$Primary.forest~inga$Secondary.forest)
chisq.test(table(inga$Primary.forest, inga$Secondary.forest))

plot(inga$Primary.forest~inga$Disturbed.forest)
chisq.test(table(inga$Primary.forest, inga$Disturbed.forest))

plot(inga$Primary.forest~inga$Undisturbed.forest)
chisq.test(table(inga$Primary.forest, inga$Undisturbed.forest))

plot(inga$Primary.forest~inga$min.Elevational.range..m.)
summary(glm(inga$Primary.forest ~ inga$min.Elevational.range..m., 
            family = binomial))

plot(inga$Primary.forest~inga$max.Elevational.range..m.)
summary(glm(inga$Primary.forest ~ inga$max.Elevational.range..m., 
            family = binomial))

plot(inga$Primary.forest~inga$Height.of.tree..m.)
summary(glm(inga$Primary.forest ~ inga$Height.of.tree..m., 
            family = binomial))

plot(inga$Primary.forest~inga$Diameter.of.tree..cm.)
summary(glm(inga$Primary.forest ~ inga$Diameter.of.tree..cm., 
            family = binomial))

plot(inga$Primary.forest~inga$Conspectus)
chisq.test(table(inga$Primary.forest, inga$Conspectus))

plot(inga$Primary.forest~inga$SLA)
summary(glm(inga$Primary.forest ~ inga$SLA, 
            family = binomial))

plot(inga$Primary.forest~inga$SiO2)
summary(glm(inga$Primary.forest ~ inga$SiO2, 
            family = binomial))

plot(inga$Primary.forest~inga$N_leaf)
summary(glm(inga$Primary.forest ~ inga$N_leaf, 
            family = binomial))

plot(inga$Primary.forest~inga$C_leaf)
summary(glm(inga$Primary.forest ~ inga$C_leaf, 
            family = binomial))

plot(inga$Primary.forest~inga$P_leaf)
summary(glm(inga$Primary.forest ~ inga$P_leaf, 
            family = binomial))

plot(inga$Primary.forest~inga$chlorophyll)
summary(glm(inga$Primary.forest ~ inga$chlorophyll, 
            family = binomial))

plot(inga$Primary.forest~inga$expansion)
summary(glm(inga$Primary.forest ~ inga$expansion, 
            family = binomial))

plot(inga$Primary.forest~inga$X.ants.on.m2.of.leaf)
summary(glm(inga$Primary.forest ~ inga$X.ants.on.m2.of.leaf, 
            family = binomial))

plot(inga$Primary.forest~inga$X.ants.EFN)
summary(glm(inga$Primary.forest ~ inga$X.ants.EFN, 
            family = binomial))

plot(inga$Primary.forest~inga$wood.density)
summary(glm(inga$Primary.forest ~ inga$wood.density, 
            family = binomial))

plot(inga$Primary.forest~inga$Field_max_height_.m.)
summary(glm(inga$Primary.forest ~ inga$Field_max_height_.m., 
            family = binomial))

plot(inga$Primary.forest~inga$Field_leaflet_area_.cm2.)
summary(glm(inga$Primary.forest ~ inga$Field_leaflet_area_.cm2., 
            family = binomial))

plot(inga$Primary.forest~inga$X2_Field_leflet_area_.cm2.)
summary(glm(inga$Primary.forest ~ inga$X2_Field_leflet_area_.cm2., 
            family = binomial))

plot(inga$Primary.forest~inga$Avg_FLA_.cm2.)
summary(glm(inga$Primary.forest ~ inga$Avg_FLA_.cm2., 
            family = binomial))

# Secondary forest ~ ... 
plot(inga$Secondary.forest~inga$Disturbed.forest)
chisq.test(table(inga$Secondary.forest, inga$Disturbed.forest))

plot(inga$Secondary.forest~inga$Undisturbed.forest)
chisq.test(table(inga$Secondary.forest, inga$Undisturbed.forest))

plot(inga$Secondary.forest~inga$min.Elevational.range..m.)
summary(glm(inga$Secondary.forest ~ inga$min.Elevational.range..m., 
            family = binomial))

plot(inga$Secondary.forest~inga$max.Elevational.range..m.)
summary(glm(inga$Secondary.forest ~ inga$max.Elevational.range..m., 
            family = binomial))

plot(inga$Secondary.forest~inga$Height.of.tree..m.)
summary(glm(inga$Secondary.forest ~ inga$Height.of.tree..m., 
            family = binomial))

plot(inga$Secondary.forest~inga$Diameter.of.tree..cm.)
summary(glm(inga$Secondary.forest ~ inga$Diameter.of.tree..cm., 
            family = binomial))

plot(inga$Secondary.forest~inga$Conspectus)
chisq.test(table(inga$Secondary.forest, inga$Conspectus))

plot(inga$Secondary.forest~inga$SLA)
summary(glm(inga$Secondary.forest ~ inga$SLA, 
            family = binomial))

plot(inga$Secondary.forest~inga$SiO2)
summary(glm(inga$Secondary.forest ~ inga$SiO2, 
            family = binomial))

plot(inga$Secondary.forest~inga$N_leaf)
summary(glm(inga$Secondary.forest ~ inga$N_leaf, 
            family = binomial))

plot(inga$Secondary.forest~inga$C_leaf)
summary(glm(inga$Secondary.forest ~ inga$C_leaf, 
            family = binomial))

plot(inga$Secondary.forest~inga$P_leaf)
summary(glm(inga$Secondary.forest ~ inga$P_leaf, 
            family = binomial))

plot(inga$Secondary.forest~inga$chlorophyll)
summary(glm(inga$Secondary.forest ~ inga$chlorophyll, 
            family = binomial))

plot(inga$Secondary.forest~inga$expansion)
summary(glm(inga$Secondary.forest ~ inga$expansion, 
            family = binomial))

plot(inga$Secondary.forest~inga$X.ants.on.m2.of.leaf)
summary(glm(inga$Secondary.forest ~ inga$X.ants.on.m2.of.leaf, 
            family = binomial))

plot(inga$Secondary.forest~inga$X.ants.EFN)
summary(glm(inga$Secondary.forest ~ inga$X.ants.EFN, 
            family = binomial))

plot(inga$Secondary.forest~inga$wood.density)
summary(glm(inga$Secondary.forest ~ inga$wood.density, 
            family = binomial))

plot(inga$Secondary.forest~inga$Field_max_height_.m.)
summary(glm(inga$Secondary.forest ~ inga$Field_max_height_.m., 
            family = binomial))

plot(inga$Secondary.forest~inga$Field_leaflet_area_.cm2.)
summary(glm(inga$Secondary.forest ~ inga$Field_leaflet_area_.cm2., 
            family = binomial))

plot(inga$Secondary.forest~inga$X2_Field_leflet_area_.cm2.)
summary(glm(inga$Secondary.forest ~ inga$X2_Field_leflet_area_.cm2., 
            family = binomial))

plot(inga$Secondary.forest~inga$Avg_FLA_.cm2.)
summary(glm(inga$Secondary.forest ~ inga$Avg_FLA_.cm2., 
            family = binomial))

# Disturbed forest ~ ...
plot(inga$Disturbed.forest~inga$Undisturbed.forest)
chisq.test(table(inga$Disturbed.forest, inga$Undisturbed.forest))

plot(inga$Disturbed.forest~inga$min.Elevational.range..m.)
summary(glm(inga$Disturbed.forest ~ inga$min.Elevational.range..m., 
            family = binomial))

plot(inga$Disturbed.forest~inga$max.Elevational.range..m.)
summary(glm(inga$Disturbed.forest ~ inga$max.Elevational.range..m., 
            family = binomial))

plot(inga$Disturbed.forest~inga$Height.of.tree..m.)
summary(glm(inga$Disturbed.forest ~ inga$Height.of.tree..m., 
            family = binomial))

plot(inga$Disturbed.forest~inga$Diameter.of.tree..cm.)
summary(glm(inga$Disturbed.forest ~ inga$Diameter.of.tree..cm., 
            family = binomial))

plot(inga$Disturbed.forest~inga$Conspectus)
chisq.test(table(inga$Disturbed.forest, inga$Conspectus))

plot(inga$Disturbed.forest~inga$SLA)
summary(glm(inga$Disturbed.forest ~ inga$SLA, 
            family = binomial))

plot(inga$Disturbed.forest~inga$SiO2)
summary(glm(inga$Disturbed.forest ~ inga$SiO2, 
            family = binomial))

plot(inga$Disturbed.forest~inga$N_leaf)
summary(glm(inga$Disturbed.forest ~ inga$N_leaf, 
            family = binomial))

plot(inga$Disturbed.forest~inga$C_leaf)
summary(glm(inga$Disturbed.forest ~ inga$C_leaf, 
            family = binomial))

plot(inga$Disturbed.forest~inga$P_leaf)
summary(glm(inga$Disturbed.forest ~ inga$P_leaf, 
            family = binomial))

plot(inga$Disturbed.forest~inga$chlorophyll)
summary(glm(inga$Disturbed.forest ~ inga$chlorophyll, 
            family = binomial))

plot(inga$Disturbed.forest~inga$expansion)
summary(glm(inga$Disturbed.forest ~ inga$expansion, 
            family = binomial))

plot(inga$Disturbed.forest~inga$X.ants.on.m2.of.leaf)
summary(glm(inga$Disturbed.forest ~ inga$X.ants.on.m2.of.leaf, 
            family = binomial))

plot(inga$Disturbed.forest~inga$X.ants.EFN)
summary(glm(inga$Disturbed.forest ~ inga$X.ants.EFN, 
            family = binomial))

plot(inga$Disturbed.forest~inga$wood.density)
summary(glm(inga$Disturbed.forest ~ inga$wood.density, 
            family = binomial))

plot(inga$Disturbed.forest~inga$Field_max_height_.m.)
summary(glm(inga$Disturbed.forest ~ inga$Field_max_height_.m., 
            family = binomial))

plot(inga$Disturbed.forest~inga$Field_leaflet_area_.cm2.)
summary(glm(inga$Disturbed.forest ~ inga$Field_leaflet_area_.cm2., 
            family = binomial))

plot(inga$Disturbed.forest~inga$X2_Field_leflet_area_.cm2.)
summary(glm(inga$Disturbed.forest ~ inga$X2_Field_leflet_area_.cm2., 
            family = binomial))

plot(inga$Disturbed.forest~inga$Avg_FLA_.cm2.)
summary(glm(inga$Disturbed.forest ~ inga$Avg_FLA_.cm2., 
            family = binomial))

# Undisturbed forest ~ ...

plot(inga$Undisturbed.forest~inga$min.Elevational.range..m.)
summary(glm(inga$Undisturbed.forest ~ inga$min.Elevational.range..m., 
            family = binomial))

plot(inga$Undisturbed.forest~inga$max.Elevational.range..m.)
summary(glm(inga$Undisturbed.forest ~ inga$max.Elevational.range..m., 
            family = binomial))

plot(inga$Undisturbed.forest~inga$Height.of.tree..m.)
summary(glm(inga$Undisturbed.forest ~ inga$Height.of.tree..m., 
            family = binomial))

plot(inga$Undisturbed.forest~inga$Diameter.of.tree..cm.)
summary(glm(inga$Undisturbed.forest ~ inga$Diameter.of.tree..cm., 
            family = binomial))

plot(inga$Undisturbed.forest~inga$Conspectus)
chisq.test(table(inga$Undisturbed.forest, inga$Conspectus))

plot(inga$Undisturbed.forest~inga$SLA)
summary(glm(inga$Undisturbed.forest ~ inga$SLA, 
            family = binomial))

plot(inga$Undisturbed.forest~inga$SiO2)
summary(glm(inga$Undisturbed.forest ~ inga$SiO2, 
            family = binomial))

plot(inga$Undisturbed.forest~inga$N_leaf)
summary(glm(inga$Undisturbed.forest ~ inga$N_leaf, 
            family = binomial))

plot(inga$Undisturbed.forest~inga$C_leaf)
summary(glm(inga$Undisturbed.forest ~ inga$C_leaf, 
            family = binomial))

plot(inga$Undisturbed.forest~inga$P_leaf)
summary(glm(inga$Undisturbed.forest ~ inga$P_leaf, 
            family = binomial))

plot(inga$Undisturbed.forest~inga$chlorophyll)
summary(glm(inga$Undisturbed.forest ~ inga$chlorophyll, 
            family = binomial))

plot(inga$Undisturbed.forest~inga$expansion)
summary(glm(inga$Undisturbed.forest ~ inga$expansion, 
            family = binomial))

plot(inga$Undisturbed.forest~inga$X.ants.on.m2.of.leaf)
summary(glm(inga$Undisturbed.forest ~ inga$X.ants.on.m2.of.leaf, 
            family = binomial))

plot(inga$Undisturbed.forest~inga$X.ants.EFN)
summary(glm(inga$Undisturbed.forest ~ inga$X.ants.EFN, 
            family = binomial))

plot(inga$Undisturbed.forest~inga$wood.density)
summary(glm(inga$Undisturbed.forest ~ inga$wood.density, 
            family = binomial))

plot(inga$Undisturbed.forest~inga$Field_max_height_.m.)
summary(glm(inga$Undisturbed.forest ~ inga$Field_max_height_.m., 
            family = binomial))

plot(inga$Undisturbed.forest~inga$Field_leaflet_area_.cm2.)
summary(glm(inga$Undisturbed.forest ~ inga$Field_leaflet_area_.cm2., 
            family = binomial))

plot(inga$Undisturbed.forest~inga$X2_Field_leflet_area_.cm2.)
summary(glm(inga$Undisturbed.forest ~ inga$X2_Field_leflet_area_.cm2., 
            family = binomial))

plot(inga$Undisturbed.forest~inga$Avg_FLA_.cm2.)
summary(glm(inga$Undisturbed.forest ~ inga$Avg_FLA_.cm2., 
            family = binomial))

# min elev. range ~ ... 

plot(inga$min.Elevational.range..m.~inga$max.Elevational.range..m.)
summary(lm(inga$min.Elevational.range..m.~inga$max.Elevational.range..m.))

plot(inga$min.Elevational.range..m.~inga$Height.of.tree..m.)
summary(lm(inga$min.Elevational.range..m.~inga$Height.of.tree..m.))

plot(inga$min.Elevational.range..m.~inga$Diameter.of.tree..cm.)
summary(lm(inga$min.Elevational.range..m.~inga$Diameter.of.tree..cm.))

plot(inga$min.Elevational.range..m.~inga$Conspectus)
summary(lm(inga$min.Elevational.range..m.~inga$Conspectus))

plot(inga$min.Elevational.range..m.~inga$SLA)
summary(lm(inga$min.Elevational.range..m.~inga$SLA))

plot(inga$min.Elevational.range..m.~inga$SiO2)
summary(lm(inga$min.Elevational.range..m.~inga$SiO2))

plot(inga$min.Elevational.range..m.~inga$N_leaf)
summary(lm(inga$min.Elevational.range..m.~inga$N_leaf))

plot(inga$min.Elevational.range..m.~inga$C_leaf)
summary(lm(inga$min.Elevational.range..m.~inga$C_leaf))

plot(inga$min.Elevational.range..m.~inga$P_leaf)
summary(lm(inga$min.Elevational.range..m.~inga$P_leaf))

plot(inga$min.Elevational.range..m.~inga$chlorophyll)
summary(lm(inga$min.Elevational.range..m.~inga$chlorophyll))

plot(inga$min.Elevational.range..m.~inga$expansion)
summary(lm(inga$min.Elevational.range..m.~inga$expansion))

plot(inga$min.Elevational.range..m.~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$min.Elevational.range..m.~inga$X.ants.on.m2.of.leaf))

plot(inga$min.Elevational.range..m.~inga$X.ants.EFN)
summary(lm(inga$min.Elevational.range..m.~inga$X.ants.EFN))

plot(inga$min.Elevational.range..m.~inga$wood.density)
summary(lm(inga$min.Elevational.range..m.~inga$wood.density))

plot(inga$min.Elevational.range..m.~inga$Field_max_height_.m.)
summary(lm(inga$min.Elevational.range..m.~inga$Field_max_height_.m.))

plot(inga$min.Elevational.range..m.~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$min.Elevational.range..m.~inga$Field_leaflet_area_.cm2.))

plot(inga$min.Elevational.range..m.~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$min.Elevational.range..m.~inga$X2_Field_leflet_area_.cm2.))

plot(inga$min.Elevational.range..m.~inga$Avg_FLA_.cm2.)
summary(lm(inga$min.Elevational.range..m.~inga$Avg_FLA_.cm2.))

# max elev range ~ ...

plot(inga$max.Elevational.range..m.~inga$Height.of.tree..m.)
summary(lm(inga$max.Elevational.range..m.~inga$Height.of.tree..m.))

plot(inga$max.Elevational.range..m.~inga$Diameter.of.tree..cm.)
summary(lm(inga$max.Elevational.range..m.~inga$Diameter.of.tree..cm.))

plot(inga$max.Elevational.range..m.~inga$Conspectus)
summary(lm(inga$max.Elevational.range..m.~inga$Conspectus))

plot(inga$max.Elevational.range..m.~inga$SLA)
summary(lm(inga$max.Elevational.range..m.~inga$SLA))

plot(inga$max.Elevational.range..m.~inga$SiO2)
summary(lm(inga$max.Elevational.range..m.~inga$SiO2))

plot(inga$max.Elevational.range..m.~inga$N_leaf)
summary(lm(inga$max.Elevational.range..m.~inga$N_leaf))

plot(inga$max.Elevational.range..m.~inga$C_leaf)
summary(lm(inga$max.Elevational.range..m.~inga$C_leaf))

plot(inga$max.Elevational.range..m.~inga$P_leaf)
summary(lm(inga$max.Elevational.range..m.~inga$P_leaf))

plot(inga$max.Elevational.range..m.~inga$chlorophyll)
summary(lm(inga$max.Elevational.range..m.~inga$chlorophyll))

plot(inga$max.Elevational.range..m.~inga$expansion)
summary(lm(inga$max.Elevational.range..m.~inga$expansion))

plot(inga$max.Elevational.range..m.~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$max.Elevational.range..m.~inga$X.ants.on.m2.of.leaf))

plot(inga$max.Elevational.range..m.~inga$X.ants.EFN)
summary(lm(inga$max.Elevational.range..m.~inga$X.ants.EFN))

plot(inga$max.Elevational.range..m.~inga$wood.density)
summary(lm(inga$max.Elevational.range..m.~inga$wood.density))

plot(inga$max.Elevational.range..m.~inga$Field_max_height_.m.)
summary(lm(inga$max.Elevational.range..m.~inga$Field_max_height_.m.))

plot(inga$max.Elevational.range..m.~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$max.Elevational.range..m.~inga$Field_leaflet_area_.cm2.))

plot(inga$max.Elevational.range..m.~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$max.Elevational.range..m.~inga$X2_Field_leflet_area_.cm2.))

plot(inga$max.Elevational.range..m.~inga$Avg_FLA_.cm2.)
summary(lm(inga$max.Elevational.range..m.~inga$Avg_FLA_.cm2.))

# Height of tree ~ ...

plot(inga$Height.of.tree..m.~inga$Diameter.of.tree..cm.)
summary(lm(inga$Height.of.tree..m.~inga$Diameter.of.tree..cm.))

plot(inga$Height.of.tree..m.~inga$Conspectus)
summary(lm(inga$Height.of.tree..m.~inga$Conspectus))

plot(inga$Height.of.tree..m.~inga$SLA)
summary(lm(inga$Height.of.tree..m.~inga$SLA))

plot(inga$Height.of.tree..m.~inga$SiO2)
summary(lm(inga$Height.of.tree..m.~inga$SiO2))

plot(inga$Height.of.tree..m.~inga$N_leaf)
summary(lm(inga$Height.of.tree..m.~inga$N_leaf))

plot(inga$Height.of.tree..m.~inga$C_leaf)
summary(lm(inga$Height.of.tree..m.~inga$C_leaf))

plot(inga$Height.of.tree..m.~inga$P_leaf)
summary(lm(inga$Height.of.tree..m.~inga$P_leaf))

plot(inga$Height.of.tree..m.~inga$chlorophyll)
summary(lm(inga$Height.of.tree..m.~inga$chlorophyll))

plot(inga$Height.of.tree..m.~inga$expansion)
summary(lm(inga$Height.of.tree..m.~inga$expansion))

plot(inga$Height.of.tree..m.~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$Height.of.tree..m.~inga$X.ants.on.m2.of.leaf))

plot(inga$Height.of.tree..m.~inga$X.ants.EFN)
summary(lm(inga$Height.of.tree..m.~inga$X.ants.EFN))

plot(inga$Height.of.tree..m.~inga$wood.density)
summary(lm(inga$Height.of.tree..m.~inga$wood.density))

plot(inga$Height.of.tree..m.~inga$Field_max_height_.m.)
summary(lm(inga$Height.of.tree..m.~inga$Field_max_height_.m.))

plot(inga$Height.of.tree..m.~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$Height.of.tree..m.~inga$Field_leaflet_area_.cm2.))

plot(inga$Height.of.tree..m.~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$Height.of.tree..m.~inga$X2_Field_leflet_area_.cm2.))

plot(inga$Height.of.tree..m.~inga$Avg_FLA_.cm2.)
summary(lm(inga$Height.of.tree..m.~inga$Avg_FLA_.cm2.))

# Diameter of tree ~ ... 

plot(Diameter.of.tree..cm.~ Conspectus, data = inga)
summary(lm(Diameter.of.tree..cm.~ Conspectus, data = inga))

plot(inga$Diameter.of.tree..cm.~inga$SLA)
summary(lm(inga$Diameter.of.tree..cm.~inga$SLA))

plot(inga$Diameter.of.tree..cm.~inga$SiO2)
summary(lm(inga$Diameter.of.tree..cm.~inga$SiO2))

plot(inga$Diameter.of.tree..cm.~inga$N_leaf)
summary(lm(inga$Diameter.of.tree..cm.~inga$N_leaf))

plot(inga$Diameter.of.tree..cm.~inga$C_leaf)
summary(lm(inga$Diameter.of.tree..cm.~inga$C_leaf))

plot(inga$Diameter.of.tree..cm.~inga$P_leaf)
summary(lm(inga$Diameter.of.tree..cm.~inga$P_leaf))

plot(inga$Diameter.of.tree..cm.~inga$chlorophyll)
summary(lm(inga$Diameter.of.tree..cm.~inga$chlorophyll))

plot(inga$Diameter.of.tree..cm.~inga$expansion)
summary(lm(inga$Diameter.of.tree..cm.~inga$expansion))

plot(inga$Diameter.of.tree..cm.~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$Diameter.of.tree..cm.~inga$X.ants.on.m2.of.leaf))

plot(inga$Diameter.of.tree..cm.~inga$X.ants.EFN)
summary(lm(inga$Diameter.of.tree..cm.~inga$X.ants.EFN))

plot(inga$Diameter.of.tree..cm.~inga$wood.density)
summary(lm(inga$Diameter.of.tree..cm.~inga$wood.density))

plot(inga$Diameter.of.tree..cm.~inga$Field_max_height_.m.)
summary(lm(inga$Diameter.of.tree..cm.~inga$Field_max_height_.m.))

plot(inga$Diameter.of.tree..cm.~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$Diameter.of.tree..cm.~inga$Field_leaflet_area_.cm2.))

plot(inga$Diameter.of.tree..cm.~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$Diameter.of.tree..cm.~inga$X2_Field_leflet_area_.cm2.))

plot(inga$Diameter.of.tree..cm.~inga$Avg_FLA_.cm2.)
summary(lm(inga$Diameter.of.tree..cm.~inga$Avg_FLA_.cm2.))

# conspectus ~ ...

plot(inga$Conspectus~inga$SLA)
summary(glm(inga$Conspectus~inga$SLA, family = binomial))

plot(inga$Conspectus~inga$SiO2)
summary(glm(inga$Conspectus~inga$SiO2, family = binomial))

plot(inga$Conspectus~inga$N_leaf)
summary(glm(inga$Conspectus~inga$N_leaf, family = binomial))

plot(inga$Conspectus~inga$C_leaf)
summary(glm(inga$Conspectus~inga$C_leaf, family = binomial))

plot(inga$Conspectus~inga$P_leaf)
summary(glm(inga$Conspectus~inga$P_leaf, family = binomial))

plot(inga$Conspectus~inga$chlorophyll)
summary(glm(inga$Conspectus~inga$chlorophyll, family = binomial))

plot(inga$Conspectus~inga$expansion)
summary(glm(inga$Conspectus~inga$expansion, family = binomial))

plot(inga$Conspectus~inga$X.ants.on.m2.of.leaf)
summary(glm(inga$Conspectus~inga$X.ants.on.m2.of.leaf, family = binomial))

plot(inga$Conspectus~inga$X.ants.EFN)
summary(glm(inga$Conspectus~inga$X.ants.EFN, family = binomial))

plot(inga$Conspectus~inga$wood.density)
summary(glm(inga$Conspectus~inga$wood.density, family = binomial))

plot(inga$Conspectus~inga$Field_max_height_.m.)
summary(glm(inga$Conspectus~inga$Field_max_height_.m., family = binomial))

plot(inga$Conspectus~inga$Field_leaflet_area_.cm2.)
summary(glm(inga$Conspectus~inga$Field_leaflet_area_.cm2., family = binomial))

plot(inga$Conspectus~inga$X2_Field_leflet_area_.cm2.)
summary(glm(inga$Conspectus~inga$X2_Field_leflet_area_.cm2., family = binomial))

plot(inga$Conspectus~inga$Avg_FLA_.cm2.)
summary(glm(inga$Conspectus~inga$Avg_FLA_.cm2., family = binomial))

# SLA ~ ...

plot(inga$SLA~inga$SiO2)
summary(lm(inga$SLA~inga$SiO2))

plot(inga$SLA~inga$N_leaf)
summary(lm(inga$SLA~inga$N_leaf))

plot(inga$SLA~inga$C_leaf)
summary(lm(inga$SLA~inga$C_leaf))

plot(inga$SLA~inga$P_leaf)
summary(lm(inga$SLA~inga$P_leaf))

plot(inga$SLA~inga$chlorophyll)
summary(lm(inga$SLA~inga$chlorophyll))

plot(inga$SLA~inga$expansion)
summary(lm(inga$SLA~inga$expansion))

plot(inga$SLA~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$SLA~inga$X.ants.on.m2.of.leaf))

plot(inga$SLA~inga$X.ants.EFN)
summary(lm(inga$SLA~inga$X.ants.EFN))

plot(inga$SLA~inga$wood.density)
summary(lm(inga$SLA~inga$wood.density))

plot(inga$SLA~inga$Field_max_height_.m.)
summary(lm(inga$SLA~inga$Field_max_height_.m.))

plot(inga$SLA~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$SLA~inga$Field_leaflet_area_.cm2.))

plot(inga$SLA~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$SLA~inga$X2_Field_leflet_area_.cm2.))

plot(inga$SLA~inga$Avg_FLA_.cm2.)
summary(lm(inga$SLA~inga$Avg_FLA_.cm2.))

# SiO2 ~ ...

plot(inga$SiO2~inga$N_leaf)
summary(lm(inga$SiO2~inga$N_leaf))

plot(inga$SiO2~inga$C_leaf)
summary(lm(inga$SiO2~inga$C_leaf))

plot(inga$SiO2~inga$P_leaf)
summary(lm(inga$SiO2~inga$P_leaf))

plot(inga$SiO2~inga$chlorophyll)
summary(lm(inga$SiO2~inga$chlorophyll))

plot(inga$SiO2~inga$expansion)
summary(lm(inga$SiO2~inga$expansion))

plot(inga$SiO2~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$SiO2~inga$X.ants.on.m2.of.leaf))

plot(inga$SiO2~inga$X.ants.EFN)
summary(lm(inga$SiO2~inga$X.ants.EFN))

plot(inga$SiO2~inga$wood.density)
summary(lm(inga$SiO2~inga$wood.density))

plot(inga$SiO2~inga$Field_max_height_.m.)
summary(lm(inga$SiO2~inga$Field_max_height_.m.))

plot(inga$SiO2~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$SiO2~inga$Field_leaflet_area_.cm2.))

plot(inga$SiO2~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$SiO2~inga$X2_Field_leflet_area_.cm2.))

plot(inga$SiO2~inga$Avg_FLA_.cm2.)
summary(lm(inga$SiO2~inga$Avg_FLA_.cm2.))

# N_leaf ~ ...

plot(inga$N_leaf~inga$C_leaf)
summary(lm(inga$N_leaf~inga$C_leaf))

plot(inga$N_leaf~inga$P_leaf)
summary(lm(inga$N_leaf~inga$P_leaf))

plot(inga$N_leaf~inga$chlorophyll)
summary(lm(inga$N_leaf~inga$chlorophyll))

plot(inga$N_leaf~inga$expansion)
summary(lm(inga$N_leaf~inga$expansion))

plot(inga$N_leaf~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$N_leaf~inga$X.ants.on.m2.of.leaf))

plot(inga$N_leaf~inga$X.ants.EFN)
summary(lm(inga$N_leaf~inga$X.ants.EFN))

plot(inga$N_leaf~inga$wood.density)
summary(lm(inga$N_leaf~inga$wood.density))

plot(inga$N_leaf~inga$Field_max_height_.m.)
summary(lm(inga$N_leaf~inga$Field_max_height_.m.))

plot(inga$N_leaf~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$N_leaf~inga$Field_leaflet_area_.cm2.))

plot(inga$N_leaf~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$N_leaf~inga$X2_Field_leflet_area_.cm2.))

plot(inga$N_leaf~inga$Avg_FLA_.cm2.)
summary(lm(inga$N_leaf~inga$Avg_FLA_.cm2.))

# C_leaf ~ ... CHANGE FROM FACTOR TO NUMERIC
as.numeric(as.factor(inga$C_leaf))

plot(inga$C_leaf~inga$P_leaf)
summary(lm(inga$C_leaf~inga$P_leaf))

plot(inga$C_leaf~inga$chlorophyll)
summary(lm(inga$C_leaf~inga$chlorophyll))

plot(inga$C_leaf~inga$expansion)
summary(lm(inga$C_leaf~inga$expansion))

plot(inga$C_leaf~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$C_leaf~inga$X.ants.on.m2.of.leaf))

plot(inga$C_leaf~inga$X.ants.EFN)
summary(lm(inga$C_leaf~inga$X.ants.EFN))

plot(inga$C_leaf~inga$wood.density)
summary(lm(inga$C_leaf~inga$wood.density))

plot(inga$C_leaf~inga$Field_max_height_.m.)
summary(lm(inga$C_leaf~inga$Field_max_height_.m.))

plot(inga$C_leaf~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$C_leaf~inga$Field_leaflet_area_.cm2.))

plot(inga$C_leaf~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$C_leaf~inga$X2_Field_leflet_area_.cm2.))

plot(inga$C_leaf~inga$Avg_FLA_.cm2.)
summary(lm(inga$C_leaf~inga$Avg_FLA_.cm2.))

# P_leaf ~ ...

plot(inga$P_leaf~inga$chlorophyll)
summary(lm(inga$P_leaf~inga$chlorophyll))

plot(inga$P_leaf~inga$expansion)
summary(lm(inga$P_leaf~inga$expansion))

plot(inga$P_leaf~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$P_leaf~inga$X.ants.on.m2.of.leaf))

plot(inga$P_leaf~inga$X.ants.EFN)
summary(lm(inga$P_leaf~inga$X.ants.EFN))

plot(inga$P_leaf~inga$wood.density)
summary(lm(inga$P_leaf~inga$wood.density))

plot(inga$P_leaf~inga$Field_max_height_.m.)
summary(lm(inga$P_leaf~inga$Field_max_height_.m.))

plot(inga$P_leaf~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$P_leaf~inga$Field_leaflet_area_.cm2.))

plot(inga$P_leaf~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$P_leaf~inga$X2_Field_leflet_area_.cm2.))

plot(inga$P_leaf~inga$Avg_FLA_.cm2.)
summary(lm(inga$P_leaf~inga$Avg_FLA_.cm2.))

# chlorophyll

plot(inga$chlorophyll~inga$expansion)
summary(lm(inga$chlorophyll~inga$expansion))

plot(inga$chlorophyll~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$chlorophyll~inga$X.ants.on.m2.of.leaf))

plot(inga$chlorophyll~inga$X.ants.EFN)
summary(lm(inga$chlorophyll~inga$X.ants.EFN))

plot(inga$chlorophyll~inga$wood.density)
summary(lm(inga$chlorophyll~inga$wood.density))

plot(inga$chlorophyll~inga$Field_max_height_.m.)
summary(lm(inga$chlorophyll~inga$Field_max_height_.m.))

plot(inga$chlorophyll~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$chlorophyll~inga$Field_leaflet_area_.cm2.))

plot(inga$chlorophyll~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$chlorophyll~inga$X2_Field_leflet_area_.cm2.))

plot(inga$chlorophyll~inga$Avg_FLA_.cm2.)
summary(lm(inga$chlorophyll~inga$Avg_FLA_.cm2.))

# expansion

plot(inga$expansion~inga$X.ants.on.m2.of.leaf)
summary(lm(inga$expansion~inga$X.ants.on.m2.of.leaf))

plot(inga$expansion~inga$X.ants.EFN)
summary(lm(inga$expansion~inga$X.ants.EFN))

plot(inga$expansion~inga$wood.density)
summary(lm(inga$expansion~inga$wood.density))

plot(inga$expansion~inga$Field_max_height_.m.)
summary(lm(inga$expansion~inga$Field_max_height_.m.))

plot(inga$expansion~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$expansion~inga$Field_leaflet_area_.cm2.))

plot(inga$expansion~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$expansion~inga$X2_Field_leflet_area_.cm2.))

plot(inga$expansion~inga$Avg_FLA_.cm2.)
summary(lm(inga$expansion~inga$Avg_FLA_.cm2.))

# X ants on m2 of leaf ~ ... 

plot(inga$X.ants.on.m2.of.leaf~inga$X.ants.EFN)
summary(lm(inga$X.ants.on.m2.of.leaf~inga$X.ants.EFN))

plot(inga$X.ants.on.m2.of.leaf~inga$wood.density)
summary(lm(inga$X.ants.on.m2.of.leaf~inga$wood.density))

plot(inga$X.ants.on.m2.of.leaf~inga$Field_max_height_.m.)
summary(lm(inga$X.ants.on.m2.of.leaf~inga$Field_max_height_.m.))

plot(inga$X.ants.on.m2.of.leaf~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$X.ants.on.m2.of.leaf~inga$Field_leaflet_area_.cm2.))

plot(inga$X.ants.on.m2.of.leaf~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$X.ants.on.m2.of.leaf~inga$X2_Field_leflet_area_.cm2.))

plot(inga$X.ants.on.m2.of.leaf~inga$Avg_FLA_.cm2.)
summary(lm(inga$X.ants.on.m2.of.leaf~inga$Avg_FLA_.cm2.))

# x ants EFN ~ ...

plot(inga$X.ants.EFN~inga$wood.density)
summary(lm(inga$X.ants.EFN~inga$wood.density))

plot(inga$X.ants.EFN~inga$Field_max_height_.m.)
summary(lm(inga$X.ants.EFN~inga$Field_max_height_.m.))

plot(inga$X.ants.EFN~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$X.ants.EFN~inga$Field_leaflet_area_.cm2.))

plot(inga$X.ants.EFN~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$X.ants.EFN~inga$X2_Field_leflet_area_.cm2.))

plot(inga$X.ants.EFN~inga$Avg_FLA_.cm2.)
summary(lm(inga$X.ants.EFN~inga$Avg_FLA_.cm2.))

# wood density ~ ...
plot(inga$wood.density~inga$Field_max_height_.m.)
summary(lm(inga$wood.density~inga$Field_max_height_.m.))

plot(inga$wood.density~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$wood.density~inga$Field_leaflet_area_.cm2.))

plot(inga$wood.density~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$wood.density~inga$X2_Field_leflet_area_.cm2.))

plot(inga$wood.density~inga$Avg_FLA_.cm2.)
summary(lm(inga$wood.density~inga$Avg_FLA_.cm2.))

# Field max height m ~ ...

plot(inga$Field_max_height_.m.~inga$Field_leaflet_area_.cm2.)
summary(lm(inga$Field_max_height_.m.~inga$Field_leaflet_area_.cm2.))

plot(inga$Field_max_height_.m.~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$Field_max_height_.m.~inga$X2_Field_leflet_area_.cm2.))

plot(inga$Field_max_height_.m.~inga$Avg_FLA_.cm2.)
summary(lm(inga$Field_max_height_.m.~inga$Avg_FLA_.cm2.))

# Field leaflet area cm2 ~ ... 

plot(inga$Field_leaflet_area_.cm2.~inga$X2_Field_leflet_area_.cm2.)
summary(lm(inga$Field_leaflet_area_.cm2.~inga$X2_Field_leflet_area_.cm2.))

plot(inga$Field_leaflet_area_.cm2.~inga$Avg_FLA_.cm2.)
summary(lm(inga$Field_leaflet_area_.cm2.~inga$Avg_FLA_.cm2.))

# X2 field leaflet area cm2 ~ ... 

plot(inga$X2_Field_leflet_area_.cm2.~inga$Avg_FLA_.cm2.)
summary(lm(inga$X2_Field_leflet_area_.cm2.~inga$Avg_FLA_.cm2.))



#plotting pairwise relationship between SLA and average leaflet size 
plot(SLA~Avg_leaf_size_.cm2.,data = inga)


#plotting kyle field measures of avg leaflet size against monograph and research avg leaflet sizes 
plot(na.omit(inga$Avg_FLA_.cm2.~inga$Avg_leaf_size_.cm2.))
# shows that they are positively correlated and therefore monograph estimates are pretty accurate 

plot(P_leaf~Avg_leaf_size_.cm2.,data=inga)
plot(SLA~wood.density,data=inga)

#we can also check these with simple models
summary(lm(P_leaf~Avg_leaf_size_.cm2.,data=inga))

summary(lm(log(P_leaf)~log(Avg_leaf_size_.cm2.),data=inga))

plot(log(P_leaf)~log(Avg_leaf_size_.cm2.),data=inga)


#to think about principal components analysis and identify axes of variation
#start with a small amount of monograph traits and Kyle traits
tmp_df <- princomp(na.omit(inga[,c(2,4:7,17:20,22:24,26:30)]),cor=T)
biplot(tmp_df)
str(tmp_df)
tmp_df$loadings
summary(tmp_df)
plot(tmp_df)


#do a lot of pairwise comparisons
#also check out principal components analyses once data are all cleaned up
#future steps will be thinking about imputation and how to include categorical variables
#the latter is done using gower's distance and principal coordinates analysis

#mice function for PCompA imputation of data 

# default function 
mice(data, m = 5, method = NULL, predictorMatrix, where = NULL,
     blocks, visitSequence = NULL, formulas, blots = NULL, post = NULL,
     defaultMethod = c("pmm", "logreg", "polyreg", "polr"), maxit = 5,
     printFlag = TRUE, seed = NA, data.init = NULL, ...)

# loading in subsets excluding categorical data
subset_70 <- read.csv('mice_70spp.csv') 
summary(subset_70)

imp <- mice(subset_70, m = 5, method = 'pmm')
PCompa1 <- complete(imp)

tmp_df_2 <- princomp(PCompa1[,c(2:25)], cor = T)
biplot(tmp_df_2)
str(tmp_df_2)
plot(tmp_df_2)

subset_270 <- read.csv('mice_270spp.csv')
summary(subset_270)

imp_2 <- mice(subset_270, m=5, method = 'pmm')
PCompa2 <- complete(imp_2)

tmp_df_3 <- princomp(PCompa2[,c(2:11)], cor = T)
biplot(tmp_df_3)

# Cleaning up biplots 
library(devtools)
library(ggbiplot)
print(ggbiplot(tmp_df_2, obs.scale = 1, var.scale = 1, 
               ellipse = TRUE, circle = TRUE))

print(ggbiplot(tmp_df_3, obs.scale = 0.5, var.scale = 1,  
               ellipse = TRUE, circle = TRUE) + 
        theme(legend.direction = 'horizontal', legend.position = 'top'))

# including categorical variables
library(mice)

# subset 70 spp - includes Kyle field data
subset_70 <- read.csv('mice_70spp.csv') 
summary(subset_70)

imp_3 <- mice(subset_70, m=5, method = 'pmm')
PCompa3 <- complete(imp_3)
names(PCompa3)
tmp_df_4 <- princomp(PCompa3[,c(2:25)], cor = T)
biplot(tmp_df_4)

biplot_1<-ggbiplot(tmp_df_4, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                   groups = PCompa3$Disturbed.forest, 
                   ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                   varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 2, 
                   color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Disturbed forest')
print(biplot_1)

biplot_1.1<-ggbiplot(tmp_df_4, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa3$Undisturbed.forest, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 2, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Undisturbed forest')
print(biplot_1.1)

biplot_1.2<-ggbiplot(tmp_df_4, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa3$Conspectus, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 2, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Conspectus')
print(biplot_1.2)

biplot_1.3<-ggbiplot(tmp_df_4, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa3$Montane..rain..forest, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 2, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Montane forest')
print(biplot_1.3)

biplot_1.4<-ggbiplot(tmp_df_4, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa3$Lowland..rain..forest, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 2, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Lowland forest')
print(biplot_1.4)

biplot_1.5<-ggbiplot(tmp_df_4, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa3$cloud.forest, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 2, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Cloud forest')
print(biplot_1.5)

biplot_1.6<-ggbiplot(tmp_df_4, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa3$Flooded.Periodically.flooded.sites, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 2, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Flooded/Periodically flooded sites')
print(biplot_1.6)

biplot_1.7<-ggbiplot(tmp_df_4, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa3$non.flooded.sites, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 2, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Non-flooded sites')
print(biplot_1.7)

biplot_1.8<-ggbiplot(tmp_df_4, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa3$Primary.forest, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 2, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Primary forest')
print(biplot_1.8)

biplot_1.9<-ggbiplot(tmp_df_4, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa3$Secondary.forest, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 2, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Secondary forest')
print(biplot_1.9)

# subset 277 - excludes Dexter field data 
subset_277 <- read.csv('mice_270spp.csv')
summary(subset_277)

imp_4 <- mice(subset_277, m=5, method = 'pmm')
PCompa4 <- complete(imp_4)
names(PCompa4)
tmp_df_5 <- princomp(PCompa4[,c(2:11)], cor = T)
biplot(tmp_df_5)

scores <- tmp_df_5$scores
dist(scores)
scores_table <- as.matrix(dist(scores))
rownames(scores_table) <- PCompa4$ï..Species
write.csv(scores_table, file = 'subset_277_tmp_5_scores_sppnames.csv')
biplot(tmp_df_5)
plot(scores)

biplot_2<-ggbiplot(tmp_df_5, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                   groups = PCompa4$Disturbed.forest, 
                   ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                   varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                   color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Disturbed forest')
print(biplot_2)

biplot_2.1<-ggbiplot(tmp_df_5, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa4$Undisturbed.forest, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Undsturbed forest')
print(biplot_2.1)

biplot_2.2<-ggbiplot(tmp_df_5, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa4$Montane..rain..forest, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Montane forest')
print(biplot_2.2)

biplot_2.3<-ggbiplot(tmp_df_5, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa4$Lowland..rain..forest, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Lowland forest')
print(biplot_2.3)

biplot_2.4<-ggbiplot(tmp_df_5, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa4$cloud.forest, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Cloud forest')
print(biplot_2.4)

biplot_2.5<-ggbiplot(tmp_df_5, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa4$Flooded.Periodically.flooded.sites, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Flooded/periodically flooded sites')
print(biplot_2.5)

biplot_2.6<-ggbiplot(tmp_df_5, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa4$non.flooded.sites, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Non-flooded sites')
print(biplot_2.6)

biplot_2.7<-ggbiplot(tmp_df_5, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa4$Primary.forest, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Primary forest')
print(biplot_2.7)

biplot_2.8<-ggbiplot(tmp_df_5, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa4$Secondary.forest, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Secondary forest')
print(biplot_2.8)

biplot_2.9<-ggbiplot(tmp_df_5, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     groups = PCompa4$Conspectus, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'Conspectus')
print(biplot_2.9)

# biplots of PCA for each subset 

# subset 277
character_subset277_alpha <- princomp(char_subset_277_table[,c(3:11)], cor = T)
scores277.alpha <- character_subset277_alpha$scores
dist(scores277.alpha)
scores_table277.alpha <- as.matrix(dist(scores277.alpha))
rownames(scores_table277.alpha) <- char_subset_277_table$Species
write.csv(scores_table277.alpha, file = 'subset_277_1_scores277_alpha.csv')

scores_table277.alpha <- read.csv('subset_277_1_scores277_alpha.csv')

biplot_alpha<-ggbiplot(character_subset277_alpha, choices = 1:2, scale = 1, 
                       obs.scale = 6, var.scale = 5,
                       ellipse = TRUE,labels = char_subset_277_table$Species, labels.size = 2, alpha = 0.2, 
                       varname.abbrev = FALSE, varname.size = 4.1, varname.adjust = 1.4, 
                       color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 277')
print(biplot_alpha)

# subset 70 
character_subset70_alpha <- princomp(char_subset_70_table[,c(3:5,7,8,9,10,12,13,14,17,18,19,20,31)], cor = T)
scores70.alpha <- character_subset70_alpha$scores
dist(scores70.alpha)
scores_table70.alpha <- as.matrix(dist(scores70.alpha))
rownames(scores_table70.alpha) <- char_subset_70_table$Species
write.csv(scores_table70.alpha, file = 'subset_70_1_scores70_alpha.csv')

scores_table70.alpha <- read.csv('subset_70_1_scores70_alpha.csv')

biplot_alpha_70<-ggbiplot(character_subset70_alpha, choices = 1:2, scale = 2, 
                          obs.scale = 1, var.scale = 5,
                          ellipse = TRUE,labels = char_subset_70_table$Species, labels.size = 2, alpha = 0.2, 
                          varname.abbrev = FALSE, varname.size = 4, varname.adjust = 1.4, 
                          color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 70')
print(biplot_alpha_70)

# characteristics defined in Q1 graphs 
# Buffering of environmental conditions
char_subset_277_table <- read.csv('subset_277_nonfunctionals_removed.csv')
char_subset_70_table <- read.csv('subset_70_nonfunctionals_removed.csv')

character_subset277_1 <- princomp(char_subset_277_table[,c(3:5)], cor = T)
scores277.1 <- character_subset277_1$scores
dist(scores277.1)
scores_table277.1 <- as.matrix(dist(scores277.1))
rownames(scores_table277.1) <- char_subset_277_table$Species
write.csv(scores_table277.1, file = 'subset_277_1_scores277_1_buffering.csv')


biplot_3.1<-ggbiplot(character_subset277_1, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 277 buffering')
print(biplot_3.1)

character_subset70_1 <- princomp(char_subset_70_table[,c(3:5, 31)], cor = T)
scores70.1 <- character_subset70_1$scores
dist(scores70.1)
scores_table70.1 <- as.matrix(dist(scores70.1))
rownames(scores_table70.1) <- char_subset_70_table$Species
write.csv(scores_table70.1, file = 'subset_70_1_scores70_buffering_sppnames.csv')

biplot_4.1<-ggbiplot(character_subset70_1, choices = 1:2, scale = 1, obs.scale = 0.5, var.scale = 1, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 4.05, varname.adjust = 1.2, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 70 buffering')
print(biplot_4.1)

# reduction of planting costs 

character_subset277_2 <- princomp(PCompa4[,c(8,9,12:20)], cor = T)
#character values 
character_subset70_2 <- princomp(char_subset_70_table[,c(10,19)], cor = T)
scores70.2 <- character_subset70_2$scores
dist(scores70.2)
scores_table70.2 <- as.matrix(dist(scores70.2))
rownames(scores_table70.2) <- char_subset_70_table$Species
write.csv(scores_table70.2, file = 'subset_70_2_scores70_growthrate.csv')

biplot_4.2<-ggbiplot(character_subset70_2, choices = 1:2, scale = 1, obs.scale = 0.5, var.scale = 1, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 4.05, varname.adjust = 1.2, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 70 growth')
print(biplot_4.2)

# maintenance of soil horizon 

character_subset277_3 <- princomp(char_subset_277_table[,c(3:5)], cor = T)
scores277.3 <- character_subset277_3$scores
dist(scores277.3)
scores_table277.3 <- as.matrix(dist(scores277.3))
rownames(scores_table277.3) <- char_subset_277_table$Species
write.csv(scores_table277.3, file = 'subset_277_3_scores277_soilhorizon_sppnames.csv')

biplot_3.3<-ggbiplot(character_subset277_3, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 277 maintenance of soil organic horizon')
print(biplot_3.3)

character_subset70_3 <- princomp(char_subset_70_table[,c(3:5,10,12:14,31)], cor = T)
scores70.3 <- character_subset70_3$scores
dist(scores70.3)
scores_table70.3 <- as.matrix(dist(scores70.3))
rownames(scores_table70.3) <- char_subset_70_table$Species
write.csv(scores_table70.3, file = 'subset_70_3_scores70_soilhorizon_sppnames.csv')

biplot_4.3<-ggbiplot(character_subset70_3, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 70 maintenance of soil organic horizon')
print(biplot_4.3)

# soil improvement and reducing nutrient demand 

character_subset70_4 <- princomp(char_subset_70_table[,c(12,14)], cor=T)
scores70.4 <- character_subset70_4$scores
dist(scores70.4)
scores_table70.4 <- as.matrix(dist(scores70.4))
rownames(scores_table70.4) <- char_subset_70_table$Species
write.csv(scores_table70.4, file = 'subset_70_4_scores70_soilimprovement_sppnames.csv')

biplot_4.4<-ggbiplot(character_subset70_4, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 70 soil improvement and reducing nutrient demand')
print(biplot_4.4)

# fruit and seed production 

character_subset277_5 <- princomp(PCompa4[,c(7)], cor = T)
# only one variable 

# adaptability to differing soil textures - categorical 

# carbon sequestration 

character_subset277_7 <- princomp(char_subset_277_table[,c(10,11)], cor = T)
scores277.7 <- character_subset277_7$scores
dist(scores277.7)
scores_table277.7 <- as.matrix(dist(scores277.7))
rownames(scores_table277.7) <- char_subset_277_table$Species
write.csv(scores_table277.7, file = 'subset_277_7_scores277_carbonseq_sppnames.csv')

biplot_3.7<-ggbiplot(character_subset277_7, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 277 carbon sequestration')
print(biplot_3.7)

character_subset70_7 <- princomp(char_subset_70_table[,c(19,8,9,20)], cor = T)
scores70.7 <- character_subset70_7$scores
dist(scores70.7)
scores_table70.7 <- as.matrix(dist(scores70.7))
rownames(scores_table70.7) <- char_subset_70_table$Species
write.csv(scores_table70.7, file = 'subset_70_7_scores70_carbonseq_sppnames.csv')

biplot_4.7<-ggbiplot(character_subset70_7, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 70 carbon sequestration')
print(biplot_4.7)

# fuelwood provision 

character_subset277_8 <- princomp(char_subset_277_table[,c(10,11)], cor = T)
scores277.8 <- character_subset277_8$scores
dist(scores277.8)
scores_table277.8 <- as.matrix(dist(scores277.8))
rownames(scores_table277.8) <- char_subset_277_table$Species
write.csv(scores_table277.8, file = 'subset_277_8_scores277_fuelwood_sppnames.csv')

biplot_3.8<-ggbiplot(character_subset277_8, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 277 fuelwood provision')
print(biplot_3.8)

character_subset70_8 <- princomp(char_subset_70_table[,c(8,9,19,20)], cor = T)
scores70.8 <- character_subset70_8$scores
dist(scores70.8)
scores_table70.8 <- as.matrix(dist(scores70.8))
rownames(scores_table70.8) <- char_subset_70_table$Species
write.csv(scores_table70.8, file = 'subset_70_8_scores70_fuelwood_sppnames.csv')

biplot_4.8<-ggbiplot(character_subset70_8, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 70 fuelwood provision')
print(biplot_4.8)

# weed suppression 

character_subset277_9 <- princomp(char_subset_277_table[,c(3:5)], cor = T)
scores277.9 <- character_subset277_9$scores
dist(scores277.9)
scores_table277.9 <- as.matrix(dist(scores277.9))
rownames(scores_table277.9) <- char_subset_277_table$Species
write.csv(scores_table277.9, file = 'subset_277_9_scores277_weed_sppnames.csv')

biplot_3.9<-ggbiplot(character_subset277_9, choices = 1:2, scale = 1, obs.scale = 0.2, var.scale = 1, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7,
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 277 weed suppression')
print(biplot_3.9)

character_subset70_9 <- princomp(char_subset_70_table[,c(3:5,10,31)], cor = T)
scores70.9 <- character_subset70_9$scores
dist(scores70.9)
scores_table70.9 <- as.matrix(dist(scores70.9))
rownames(scores_table70.9) <- char_subset_70_table$Species
write.csv(scores_table70.9, file = 'subset_70_9_scores70_weed_sppnames.csv')

biplot_4.9<-ggbiplot(character_subset70_9, choices = 1:2, scale = 1, obs.scale = 0.1, var.scale = 1, 
                     ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                     varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.1, 
                     color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 70 weed suppression')
print(biplot_4.9)

# pest suppression 

# ecological services 

character_subset70_11 <- princomp(char_subset_70_table[,c(6,17,18)], cor = T)
scores70.11 <- character_subset70_11$scores
dist(scores70.11)
scores_table70.11 <- as.matrix(dist(scores70.11))
rownames(scores_table70.11) <- char_subset_70_table$Species
write.csv(scores_table70.11, file = 'subset_70_11_scores70_ecological_sppnames.csv')

biplot_4.11<-ggbiplot(character_subset70_11, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                      ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                      varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                      color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 70 ecological services')
print(biplot_4.11)

# biomass production 

character_subset277_12 <- princomp(char_subset_277_table[,c(3,4,10,11)], cor = T)
scores277.12 <- character_subset277_12$scores
dist(scores277.12)
scores_table277.12 <- as.matrix(dist(scores277.12))
rownames(scores_table277.12) <- char_subset_277_table$Species
write.csv(scores_table277.12, file = 'subset_277_12_scores277_weed_sppnames.csv')

biplot_3.12<-ggbiplot(character_subset277_12, choices = 1:2, scale = 1, obs.scale = 1, var.scale = 1, 
                      ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                      varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                      color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 277 biomass production')
print(biplot_3.12)

character_subset70_12 <- princomp(char_subset_70_table[,c(3,4,10,11,19,20,31)], cor = T)
scores70.12 <- character_subset70_12$scores
dist(scores70.12)
scores_table70.12 <- as.matrix(dist(scores70.12))
rownames(scores_table70.12) <- char_subset_70_table$Species
write.csv(scores_table70.12, file = 'subset_70_12_scores277_weed_sppnames.csv')

biplot_4.12<-ggbiplot(character_subset70_12, choices = 1:2, scale = 1, obs.scale = 0.7, var.scale = 1, 
                      ellipse = TRUE,labels = NULL, labels.size = 1, alpha = 1, 
                      varname.abbrev = FALSE, varname.size = 3.6, varname.adjust = 1.7, 
                      color = muted("blue"), linetype = "solid") +
  theme(legend.direction = 'horizontal', legend.position = 'top', 
        panel.background = element_rect(fill = 'white', colour = "grey40"))+
  labs(title = 'subset 70 biomass production')
print(biplot_4.12)