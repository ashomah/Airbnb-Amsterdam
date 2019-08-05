#####
### THIS SCRIPT PLOTS LISTINGS INFORMATION
#####

# INSTALL AND LOAD PACKAGES ----
packages_list <- c('ggplot2',
                   'ggalt',
                   'gridExtra',
                   'scales',
                   'grid',
                   'lattice',
                   'ggthemes',
                   'extrafont',
                   'plotly',
                   'plyr',
                   'leaflet',
                   'maps'
)

for (i in packages_list){
  if(!i%in%installed.packages()){
    install.packages(i, dependencies = TRUE, repos = "http://cran.us.r-project.org")
    library(i, character.only = TRUE)
    print(paste0(i, ' has been installed'))
  } else {
    print(paste0(i, ' is already installed'))
    library(i, character.only = TRUE)
  }
}


# READ DATASET ----
data <- read.csv('data_output/listings_clean.csv')


# VARIABLE TYPE CORRECTION ----
str(data)
data$host_name <- as.character(data$host_name)


# COLOR PALETTE AND FONTS ----
# We used the colors of the Airbnb logo for our charts.
color1 = rgb(255/255, 90/255, 96/255, 1)
color2 = 'white'
color3 = 'black'
color4 = rgb(90/255, 101/255, 255/255, 1)
font1 = 'Impact'
font2 = 'Trebuchet MS'
spacing <-15


# DISTRIBUTIONS ----
################################################################
png(filename="plots/review_scores_rating.png", width = 900, height = 600)
ggplot(data=data, aes(data$review_scores_rating)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 2, fill = color1) +
  xlim(40,100)+ scale_y_continuous(labels = comma, limits = c(0,3000))+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.2, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Review Scores Rating",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
dev.off()
################################################################
png(filename="plots/number_reviews.png", width = 900, height = 600, units = "px")
ggplot(data=data, aes(data$number_of_reviews)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 5, fill = color1) +
  xlim(0,300)+ ylim(0,500)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Number of Reviews",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
dev.off()
################################################################
png(filename="plots/price_2_nights_2_people.png", width = 900, height = 600, units = "px")
ggplot(data=data, aes(data$price_two_nights_two_people)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 5, fill = color1) +
  xlim(25,140)+ ylim(0,150)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.2, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Price for 2 nights for 2 guests",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
dev.off()
################################################################
png(filename="plots/beds.png", width = 900, height = 600, units = "px")
ggplot(data=data, aes(data$beds)) + 
  geom_histogram(col= color1, breaks=seq(0, 4, by=1),
                 aes(fill=color1), fill = color1) +
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))+ scale_y_continuous(labels = comma)
grid.text(unit(0.7, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Beds",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
dev.off()
################################################################
ggplot(data=data, aes(data$accommodates)) + 
  geom_histogram(col=color1, breaks=seq(0, 9, by=1),
                 aes(fill=color1), fill = color1) +
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))+ scale_y_continuous(labels = comma)
grid.text(unit(0.7, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Accomodates",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$bathrooms)) + 
  geom_histogram(col=color1, breaks=seq(0, 4, by=1),
                 aes(fill=color1), fill = color1) +
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))+ scale_y_continuous(labels = comma)
grid.text(unit(0.7, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Bathrooms",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$host_total_listings_count)) + 
  geom_histogram(col= color1, breaks=seq(0, 6, by=1),
                 aes(fill=color1), fill = color1) +
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))+ scale_y_continuous(labels = comma)
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Host's Total Listings",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$price)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 10, fill = color1) +
  xlim(0,600)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))+ scale_y_continuous(labels = comma)
grid.text(unit(0.7, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Price",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$weekly_price)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 10) +
  xlim(0,400)+ ylim(0,300)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.2, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Weekly Price",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$monthly_price)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 10, fill = color1) +
  xlim(0,380)+ ylim(0,140)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Monthly Price",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$security_deposit)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 3, fill = color1) +
  xlim(0,180)+ ylim(0,140)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.2, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Security Deposit",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$cleaning_fee)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 10,fill=color1) +
  xlim(0,380)+ ylim(0,150)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Cleaning Fee",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$minimum_nights)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 5, fill = color1) +
  xlim(0,100)+ ylim(0,150)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Minimum Nights",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$maximum_nights)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 5, fill = color1) +
  xlim(26,200)+ ylim(0,130)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Maximum Nights",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$availability_30)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 2, fill = color1) +
  xlim(0,35)+ scale_y_continuous(labels = comma,limits = c(0,1500))+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Availability 30",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$availability_60)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 2, fill = color1) +
  xlim(0,70)+ ylim(0,800)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Availability 60",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$availability_90)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 2, fill = color1) +
  xlim(0,100)+ ylim(0,660)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Availability 90",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$availability_365)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 2, fill = color1) +
  xlim(0,200)+ ylim(0,550)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Availability 365",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$reviews_per_month)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 1, fill = color1) +
  xlim(7,15)+ ylim(0,50)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.6, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Reviews per Month",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$review_scores_accuracy)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 1, fill = color1) +
  xlim(0,8)+ ylim(0,100)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.2, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Review Scores Accuracy",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$review_scores_cleanliness)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 1, fill = color1) +
  xlim(0,6)+ ylim(0,40)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.1, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Review Scores Cleanliness",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$review_scores_checkin)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 1, fill = color1) +
  xlim(0,6.5)+ ylim(0,21)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.1, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Review Scores Check-In",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data=data, aes(data$review_scores_communication)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 1, fill = color1) +
  xlim(0,6.5)+ ylim(0,21)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.45, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Review Scores Communication",
          gp=gpar(col=color1, fontsize=16))
################################################################
ggplot(data=data, aes(data$review_scores_location)) + 
  geom_histogram(col= color1,
                 aes(fill=color1),binwidth = 1, fill = color1) +
  xlim(0,6.5)+ ylim(0,10)+
  theme_tufte(base_size = 5, ticks=F)+
  theme(plot.margin = unit(c(10,10,10,10),'pt'),
        axis.title=element_blank(),
        axis.text = element_text(colour = color3, size = 10, family = font2),
        axis.text.x = element_text(hjust = 1, size = 10, family = font2),
        legend.position = 'None',
        plot.background = element_rect(fill = color2, color=color2))
grid.text(unit(0.1, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Review Scores Location",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))


# SCATTER PLOTS ----
################################################################
png(filename="plots/price_number_reviews.png", width = 900, height = 600, units = "px")
ggplot(data, aes(data$price, data$number_of_reviews, color = color1)) +
  geom_point(shape = 16, size = 1, show.legend = FALSE) +
  scale_x_continuous(labels = comma, limits = c(0,600))+ scale_y_continuous(labels = comma, limits = c(0,300))+
  theme_tufte()+ labs(x = "Price", y='Number of Reviews')+
  theme(axis.ticks = element_blank(),
        axis.text.y.left = element_text(hjust = 1.5, family = font2),
        axis.text.x.bottom = element_text(vjust = 5, family = font2))
grid.text(unit(0.5, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Price and Number of Reviews",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
dev.off()
################################################################
png(filename="plots/price_review_scores_rating.png", width = 900, height = 600, units = "px")
ggplot(data, aes(data$price, data$review_scores_rating, color = color1)) +
  geom_point(shape = 16, size = 1, show.legend = FALSE) +
  ylim(40,100) +scale_x_continuous(labels = comma, limits = c(0,1000))+
  theme_tufte()+ labs(x = "Price", y='Review Scores Rating')+
  theme(axis.ticks = element_blank(),
        axis.text.y.left = element_text(hjust = 1.5, family = font2),
        axis.text.x.bottom = element_text(vjust = 5, family = font2))
grid.text(unit(0.55, 'npc'), unit(0.2,"npc"), check.overlap = T,just = "left",
          label="Price and Review Scores Rating",
          gp=gpar(col=color1, fontsize=14, fontfamily = font2))
dev.off()
################################################################
ggplot(data, aes(data$price, data$availability_365, color = color1)) +
  geom_point(shape = 16, size = 1, show.legend = FALSE) +
  ylim(0,400) +scale_x_continuous(labels = comma, limits = c(0,1000))+
  theme_tufte()+ labs(x = "Price", y='Availability 365')+
  theme(axis.ticks = element_blank(),
        axis.text.y.left = element_text(hjust = 1.5, family = font2),
        axis.text.x.bottom = element_text(vjust = 5, family = font2))
grid.text(unit(0.5, 'npc'), unit(0.95,"npc"), check.overlap = T,just = "left",
          label="Price and Availability 365",
          gp=gpar(col=color1, fontsize=16, fontfamily = font2))
################################################################
ggplot(data, aes(data$weekly_price, data$price_two_nights_two_people, color = color1)) +
  geom_point(shape = 16, size = 1, show.legend = FALSE) +
  xlim(0,500)+ scale_y_continuous(labels = comma, limits = c(0,2500))+
  theme_tufte()+ labs(x = "Price", y='Price for 2 Nights and 2 People')+
  theme(axis.ticks = element_blank(),
        axis.text.y.left = element_text(hjust = 1.5, family = font2),
        axis.text.x.bottom = element_text(vjust = 5, family = font2))
grid.text(unit(0.35, 'npc'), unit(0.9,"npc"), check.overlap = T,just = "left",
          label="Price and Price for 2 Nights and 2 People",
          gp=gpar(col=color1, fontsize=14, fontfamily = font2))


# OTHER PLOTS ----
################################################################
png(filename="plots/type_neighborhood.png", width = 900, height = 600, units = "px")
ggplot(data,aes(x = data$neighbourhood, fill = data$instant_bookable)) +
  geom_bar()+
  theme_tufte(ticks=FALSE, base_size = 8)+ scale_y_continuous(labels=comma)+scale_fill_manual(values = alpha(c(color1, color4)))+
  theme(legend.position = 'None',
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.title = element_blank(),
        axis.text.y  = element_text(size = 8, family = font2))
grid.text(0.95, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Instant Book",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=16,fontface="bold"))
grid.text(0.95, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Pending Approval",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=16,fontface="bold"))
dev.off()
################################################################
png(filename="plots/type_cancellation.png", width = 900, height = 600, units = "px")
ggplot(data,aes(x = data$cancellation_policy, fill = data$room_type)) +
  geom_bar()+
  theme_tufte(ticks=FALSE, base_size = 8)+ scale_y_continuous(labels=comma)+scale_fill_manual(values = alpha(c(color1, color4,'green3')))+
  theme(legend.position = 'None',
        axis.text.x = element_text(angle = 35, hjust = 1, size = 10),
        axis.title = element_blank(),
        axis.text.y  = element_text(size = 8, family = font2))

grid.text(1, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Private Room",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=14,fontface="bold"))
grid.text(1, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Entire Home/Apt",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=14,fontface="bold"))
dev.off()
################################################################
png(filename="plots/type_host.png", width = 900, height = 600, units = "px")
ggplot(data[data$host_is_superhost != "",],aes(x = data$neighbourhood, fill = data$host_is_superhost)) +
  geom_bar()+
  theme_tufte(ticks=FALSE, base_size = 8)+ scale_y_continuous(labels=comma) + scale_fill_manual(values = alpha(c(color4, color1)))+
  theme(legend.position = 'None',
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.title = element_blank(),
        axis.text.y  = element_text(size = 8, family = font2))

grid.text(0.95, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Superhost",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=16,fontface="bold"))
grid.text(0.95, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Host",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=16,fontface="bold"))
dev.off()
################################################################
ggplot(data,aes(x = data$host_response_time[data$host_response_time!=c('NA')], fill = data$host_is_superhost)) +
  geom_bar()+ scale_fill_manual(values = alpha(c(color1, color4)))+
  theme_tufte(ticks=FALSE, base_size = 8)+ scale_y_continuous(labels=comma)+
  theme(legend.position = 'None',
        axis.title = element_blank(),
        axis.text.x  = element_text(size = 8, family = font2),
        axis.text.y  = element_text(size = 8, family = font2))

grid.text(0.95, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Superhost",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=16,fontface="bold"))
grid.text(0.95, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Host",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=16,fontface="bold"))
################################################################
ggplot(data,aes(x = data$room_type, fill = data$host_is_superhost)) +
  geom_bar()+ scale_y_continuous(labels=comma)+scale_fill_manual(values = alpha(c(color1, color4)))+
  theme_tufte(ticks=FALSE, base_size = 8)+
  theme(legend.position = 'None',
        axis.title = element_blank(),
        axis.text.x  = element_text(size = 8, family = font2),
        axis.text.y  = element_text(size = 8, family = font2))

grid.text(0.95, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Superhost",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=16,fontface="bold"))
grid.text(0.95, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Host",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=16,fontface="bold"))
################################################################
ggplot(data,aes(x = data$neighbourhood, fill = data$is_location_exact)) +
  geom_bar()+
  theme_tufte(ticks=FALSE, base_size = 8)+ scale_y_continuous(labels=comma)+scale_fill_manual(values = alpha(c(color1, color4)))+
  theme(legend.position = 'None',
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.title = element_blank(),
        axis.text.y  = element_text(size = 8, family = font2))

grid.text(1.1, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Location Accurate",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=16,fontface="bold"))
grid.text(1.1, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Location Not Accurate",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=16,fontface="bold"))
################################################################
ggplot(data,aes(x = data$cancellation_policy, fill = data$host_is_superhost)) +
  geom_bar()+
  theme_tufte(ticks=FALSE, base_size = 8)+ scale_y_continuous(labels=comma)+scale_fill_manual(values = alpha(c(color1, color4)))+
  scale_x_discrete()+
  theme(legend.position = 'None',
        axis.text.x = element_text(angle = 35,hjust = 1, size = 7),
        axis.title = element_blank(),
        axis.text.y  = element_text(size = 8, family = font2))

grid.text(1.1, unit(1,"npc") - unit(0.9,"line"), check.overlap = T,just = "right",vjust = 2,
          label=paste("Superhost",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color4, fontsize=16,fontface="bold"))
grid.text(1.1, unit(1,"npc") - unit(1,"line"), check.overlap = T,just = "right",
          label=paste("Host",paste(rep(" ",spacing), collapse='')),
          gp=gpar(col=color1, fontsize=16,fontface="bold"))



