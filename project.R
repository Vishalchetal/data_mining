# import data from csv
data = data.frame(fread("Crimes2001.csv",sep=",",
                        header = T,
                        data.table=F,
                        verbose = T,
                        integer64 = 'numeric',
                        stringsAsFactors = T))
# remove all the lines with NA
data.clean = na.omit(data)


###############################################
# Number of Crimes for Different Type of Crimes
###############################################
summary_primaryType = summary(data.clean$Primary.Type,
                              maxsum = 10)
# calculate the percentage of each category
piepercent = paste(round(100*summary_primaryType/sum(summary_primaryType),2),
                   "%")
# draw pie chart
pie(summary_primaryType,
    piepercent,
    col=rainbow((length(summary_primaryType))),
    main = "Distribution of Primary Type of Crimes")
# draw legend
legend("topright",
       names(summary_primaryType),
       cex=0.9,
       fill=rainbow(length(summary_primaryType)))

#################################
# Number of Crimes for Different Areas
######################################
summary_communityArea = table(data.clean$Community.Area)
# sort areas according to number of crimes
summary_communityArea = sort(summary_communityArea,
                             decreasing = TRUE)
# get the 20 areas with most number of cimes
summary_communityArea = summary_communityArea[1:20]
barplot(height = summary_communityArea, 
        names.arg = names(summary_communityArea), 
        xlab = "Area Code",
        ylab = "Number of Crimes",
        main="Crimes for Each Area",
        col="yellow")


##############################################
# Arrest Rate for Different Type of Crimes
##############################################
arrest_type_true = rep(0,10)
arrest_type_true_percentage = rep(0,10)
for(i in 1:10){
  arrest_type_true[i] = dim(data.clean[data.clean$Arrest == "true" &
          data.clean$Primary.Type == names(summary_primaryType[i]),])[1]
  
  arrest_type_true_percentage[i] = 
    paste(round(arrest_type_true[i]*100 / summary_primaryType[i],2),"%")
}
# draw pie chart
pie(arrest_type_true,
    arrest_type_true_percentage,
    col=rainbow((length(arrest_type_true))),
    main = "Arrest rate for different type of crimes")
# draw legend
legend("topright",
       names(summary_primaryType),
       cex=0.8,
       fill=rainbow(length(arrest_type_true)))



###############################################
# Number of Crimes for Different Type of Crimes for Area 25
###############################################
data.clean.25 = data.clean[data.clean$Community.Area == 25,]
summary_primaryType_25 = summary(data.clean.25$Primary.Type,maxsum = 10)
# calculate the percentage of each category
piepercent_25 = paste(
  round(100*summary_primaryType_25/sum(summary_primaryType_25),2)
  ,"%")
# draw pie chart
pie(summary_primaryType_25,
    piepercent_25,
    col=rainbow((length(summary_primaryType_25))),
    main = "Distribution of Primary Type of Crimes for Area 25")
# draw legend
legend("topright",
       names(summary_primaryType_25),
       cex=0.8,
       fill=rainbow(length(summary_primaryType_25)))


#######################################
# top 3 types of crimes for 5 areas with most crimes
#######################################areas = names(summary_communityArea[1:5])
areas = names(summary_communityArea[1:5])
y_array = rep(0,15)
label_array = rep(0,15)
area_array = rep(0,15)
color_array = rep(0,15)
color_pool=c('red','purple','blue','black','brown')
counter = 0
for (i in areas){
  tmp = data.clean[data.clean$Community.Area == i,]
  summary_tmp = sort(summary(tmp$Primary.Type),decreasing = TRUE)[1:3]
  for(j in seq(1:3)){
    area_array[counter*3+j] = i
    y_array[counter*3+j] = summary_tmp[j]
    label_array[counter*3+j] = names(summary_tmp[j])
    color_array[counter*3+j] = color_pool[counter+1]
  }
  counter = counter + 1
}
df = data.frame(area_array,
                y_array,
                label_array,
                color_array)
df$area_array = factor(df$area_array)
df$color_array = as.character(df$color_array)
dotchart(x=df$y_array,
         label=df$label_array,
         groups= df$area_array,
         xlab="C-Count",
         color=df$color_array,
         cex=.7,
         main="Top 3 types of crimes for 5 areas with most crimes")

###############################
# number of crimes per month
###############################

c = cbind(data.clean, 
          month = format(as.Date(data.clean$Date,"%m/%d/%Y"),"%m"))
plot(names(summary(c$month)),
     summary(c$month),
     type = "b", 
     main = "Number of Crimes per month",
     xlab="month",
     ylab="c-count")
