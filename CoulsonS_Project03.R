# Name: Samuel Coulson
# Course: 44-149 Scientific Computing
# Assignment: Project 03
# Due Date: 4/20/2018
# Brief: Census
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments. Additional, I
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy

n <- 12
iters <- 10

census <- read.csv('us_census.csv')
contiguous <- census[!census$state %in% c('AK', 'HI', 'PR'),]
plot(contiguous$longitude, contiguous$latitude, type='p', col=contiguous$state)

chosen_counties <- sample(1:nrow(contiguous), n)

centers <- matrix(0, nrow = n, ncol = 2)

centers[,1] <- contiguous$latitude[chosen_counties]
centers[,2] <- contiguous$longitude[chosen_counties]

centers_df=contiguous[chosen_counties, 3:4]

dist_sq <- function(county, center){
  delta_x <- county[1, 'latitude'] - center[1]
  delta_y <- county[1, 'longitude'] - center[2]
  delta_x^2 + delta_y^2
}

delta_x <- contiguous[1, 'latitude'] - centers[1,1]
delta_y <- contiguous[1, 'longitude'] - centers[1,2]

print(delta_x^2 + delta_y^2)

#belongs_to[i] means the it county belongs_to the cluster at belongs_to[i]
belongs_to <- rep(0, nrow(contiguous))
for(a in 1:iters){
#finds closest cluster
  for(county in 1:nrow(contiguous)){
    closest_center <- 1
    closest_distance <- dist_sq(contiguous[county,], centers[1,])
    for(cluster in 2:n){
      d <- dist_sq(contiguous[county,], centers[cluster,])
      if(d < closest_distance){
        closest_distance <- d
        closest_center <- cluster
      }
    }
    belongs_to[county] <- closest_center
  }

  for(i in 1:n){
    interesting_cluster <- contiguous[belongs_to==i,]
    total_pop <- sum(interesting_cluster$population)
    new_lat <- sum(interesting_cluster$latitude * interesting_cluster$population) / total_pop
    new_long <- sum(interesting_cluster$longitude * interesting_cluster$population) / total_pop
    centers[i,1] <- new_lat
    centers[i,2] <- new_long
  }
  plot(contiguous$longitude, contiguous$latitude, type='p', col=belongs_to)
}