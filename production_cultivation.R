production_cultivation = read.csv("production_cultivation.csv")
crop_variety = read.csv("crop_variety.csv")

colnames(production_cultivation) = c("Crop","State","Cultivation_A2_FL","Cultivation_C2","Cost_Production","Yield")
production_cultivation = production_cultivation[,1:5]
crop = "MAIZE"
state = "Karnataka"

production_cultivation = subset(production_cultivation,Crop == crop & State == state)

total_cost_production = (cost_cultivation_A2_FL + cost_cultivation_C2)*yield + cost_production 