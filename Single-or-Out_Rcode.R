##################################################
# Install and Load Required Packages
##################################################

if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                         repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", 
                                     repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", 
                                     repos = "http://cran.us.r-project.org")
if(!require(interplot)) install.packages("interplot", 
                                    repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", 
                                         repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", 
                                     repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", 
                                     repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", 
                                            repos = "http://cran.us.r-project.org")
if(!require(gtable)) install.packages("gtable", 
                                         repos = "http://cran.us.r-project.org")
if(!require(lemon)) install.packages("lemon", 
                                      repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(markdown)) install.packages("markdown", 
                                        repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", 
                                         repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", 
                                       repos = "http://cran.us.r-project.org")

#################################################
# Wrangle the Data
#################################################

# Data is contained in two files: battedballs_2019_Raw.csv and hometofirst_2019_Raw.csv
# located at https://github.com/jfmusso/HarvardX/

# Import the data files into R data frames.

battedballs <- 
  read_csv("https://raw.githubusercontent.com/jfmusso/HarvardX/master/battedballs_2019_Raw.csv", 
           col_types = cols(
             game_date = col_date(format = "%m/%d/%Y"), 
             batter = col_character(), 
             player_name = col_character(), 
             stand = col_character(),  
             bb_type = col_factor(levels = c("ground_ball", "line_drive", "fly_ball", "popup")), 
             events = col_factor(), 
             des = col_character(), 
             launch_speed = col_double(), 
             launch_angle = col_double(), 
             hc_x = col_double(), 
             hc_y = col_double(), 
             hit_location = col_factor(), 
             if_fielding_alignment = col_factor(levels = c("Standard", "Strategic", "Infield shift")), 
             of_fielding_alignment = col_factor(), 
             hit_distance_sc = col_integer(), 
             launch_speed_angle = col_factor(), 
             game_pk = col_character(), 
             game_year = col_character(), 
             game_type = col_character(), 
             home_team = col_character(), 
             away_team = col_character(), 
             pitcher = col_factor(), 
             p_throws = col_character(), 
             description = col_character(), 
             type = col_character()
          )

  )

class(battedballs[])

hometofirst <- 
  read_csv("https://raw.githubusercontent.com/jfmusso/HarvardX/master/hometofirst_2019_Raw.csv", 
           col_types = cols(
             last_name = col_character(), 
             first_name = col_character(), 
             player_id = col_character(), 
             team_id = col_character(), 
             team = col_character(), 
             position = col_character(), 
             age = col_number(), 
             competitive_runs = col_integer(),
             hp_to_1b = col_double(), 
             sprint_speed = col_double()
           )
           
  )

class(hometofirst[])

# Remove players from hometofirst data frame if they have missing values for hp_to_1b.

hometofirst <- filter(hometofirst, hp_to_1b != "NA")

# Remove batted balls from battedballs data frame if they have missing values 
# for any of the following variables: hc_x, hc_y, if_fielding_alignment, or 
# of_fielding_alignment.

battedballs <- filter(battedballs, hc_x != "NA", hc_y != "NA", 
                      if_fielding_alignment != "NA", 
                      of_fielding_alignment != "NA")

# Combine battedballs and hometofirst data frames.

hometofirst <- rename(hometofirst, batter = player_id)
hometofirst <- hometofirst %>% dplyr::select(batter, team, position, age, hp_to_1b)
battedballs <- left_join(battedballs, hometofirst, by = "batter")

# Remove players from battedballs for whom there is no hp-to-1b data.

battedballs <- filter(battedballs, hp_to_1b != "NA")

# Remove batted balls hit into foul territory.

battedballs <- battedballs %>% mutate(foul = str_detect(des, "foul")) %>% 
  filter(foul == FALSE)

# Compute Spray Angle of each batted ball and add a new column containing results.

 # Transform x coordinate so that home plate is at 0.
battedballs <- battedballs %>% mutate(hc_x_Kolp = round(2.33 * (hc_x - 126), 2))

 # Transform y coordinate so that home plate is at 0.
battedballs <- battedballs %>% mutate(hc_y_Kolp = round(2.33 * (204.5 - hc_y), 2))

 # Compute spray_angle
battedballs <- battedballs %>% 
  mutate(spray_angle_Kolp = round((180 / pi) * atan(hc_x_Kolp / hc_y_Kolp), 1))

 # Adjust for side of plate batter stands on, so that batted balls hit to the side of the field 
 # the batter stands on (with respect to home plate) will always have a negative spray angle.

battedballs <- battedballs %>% 
  mutate(spray_angle_adj = ifelse(stand == "L", -spray_angle_Kolp, spray_angle_Kolp))

# Convert our categorical predictor variable, if_fielding_alignment, into a numeric variable.

battedballs <- battedballs %>% mutate(num_if_alignment = as.numeric(if_fielding_alignment))

# Add a more precise division of batted ball types (adv_bb_type) to more accurately categorize the values
# of our launch_angle predictor.

battedballs <- battedballs %>% 
  mutate(adv_bb_type = cut(battedballs$launch_angle, breaks = c(-90, -0.1, 10, 19, 26, 39, 90), 
                           labels = c("low_ground_ball", "high_ground_ball", "low_line_drive", 
                                      "high_line_drive", "fly_ball", "popup")))

# Add a categorical variable based on *launch_speed*.

battedballs <- battedballs %>% 
  mutate(launch_speed_cat = cut(battedballs$launch_speed, 
                                breaks = c(10, 69.9, 79.9, 89.9, 100, 125), 
                                labels = c("<70", "70-79.9", "80-89.9", "90-100", ">100")))

# Add categorical variables based on *spray_angle_Kolp* and *spray_angle_adj*.

battedballs <- battedballs %>% 
  mutate(spray_angle_Kolp_cat = cut(battedballs$spray_angle_Kolp, 
                                    breaks = c(-90, -45.1, -40, -31, -22, -9, 9, 25, 35, 43, 45, 90), 
                                    labels = c("-90:-45.1", "LF Line (-45:-40)", "3rd Baseman (-39.9:-31)", 
                                               "Hole 5-6 (-30.9:-22)", "Shortstop (-21.9:-9)", 
                                               "Hole Middle (-8.9:9)", "2nd Baseman (9.1:25)", 
                                               "Hole 3-4 (25.1:35)", "1st Baseman (35.1:43)", 
                                               "RF Line (43.1:45)", "45.1:90")))

battedballs <- battedballs %>% 
  mutate(spray_angle_adj_cat = cut(battedballs$spray_angle_adj, 
                                   breaks = c(-90, -45.1, -40, -31, -22, -9, 9, 25, 35, 43, 45, 90), 
                                   labels = c("-90:-45.1", "Pulled Down Line (-45:-40)", 
                                              "Pulled Corner Inf (-39.9:-31)", "Pulled Side Hole (-30.9:-22)", 
                                              "Pulled Mid Inf (-21.9:-9)", "Middle Hole (-8.9:9)", 
                                              "Oppo Mid Inf (9.1:25)", "Oppo Side Hole (25.1:35)", 
                                              "Oppo Corner Inf (35.1:43)", "Oppo Down Line (43.1:45)", 
                                              "45.1:90")))

# Add a categorical variable based on *hp_to_1b*.

battedballs <- battedballs %>% 
  mutate(hp_to_1b_cat = cut(battedballs$hp_to_1b, 
                                breaks = c(3.9, 4.2, 4.5, 4.8, 5.1), 
                                labels = c("3.9-4.2", "4.21-4.5", "4.51-4.8", "4.81-5.1")))

# Reorder the columns in battedballs.

battedballs <- battedballs %>% dplyr::select(game_date, batter, player_name, age, stand, position, team, 
       bb_type, adv_bb_type, events, des, hp_to_1b, hp_to_1b_cat, launch_speed, launch_speed_cat, 
       launch_angle, hc_x, hc_y, hc_x_Kolp, hc_y_Kolp, spray_angle_Kolp, spray_angle_Kolp_cat, 
       spray_angle_adj, spray_angle_adj_cat, hit_location, hit_distance_sc, 
       if_fielding_alignment, num_if_alignment, of_fielding_alignment, launch_speed_angle, 
       game_pk, game_year, game_type, home_team, away_team, pitcher, p_throws, description, type, foul)

#################################################
# Explore the Data
#################################################

# Visualize batted balls resulting in singles or outs.

set.seed(1)
battedballs_sample <- sample_n(battedballs, 4000, replace = FALSE)

ggplot(battedballs_sample, aes(hc_x_Kolp, hc_y_Kolp, color = events)) + 
  scale_color_manual(values = c("red", "royalblue3")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 1) + theme_linedraw() + labs(title = "Fair Batted Balls Using Transformed Coordinates")

# Launch Angle

# Visualize singles and outs, by type of batted ball.

ggplot(battedballs_sample, aes(hc_x_Kolp, hc_y_Kolp)) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 1) + theme_linedraw() + 
  labs(title = "Singles vs Outs, by Type of Batted Ball") + 
  facet_grid(bb_type ~ events)

# Visualize singles and outs, by adv_bb_type.

ggplot(battedballs_sample, aes(hc_x_Kolp, hc_y_Kolp)) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 1) + theme_linedraw() + 
  labs(title = "Singles vs Outs, by adv_bb_type") + 
  facet_grid(adv_bb_type ~ events)

# Create boxplots to visualize the distribution of launch_angle by singles and outs.

bb_outcome_by_launch_angle <- ggplot(battedballs, 
                                  aes(x = events, y = launch_angle, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-75, 75, by = 15), 
                     labels = c("-75", "-60", "-45", "-30", "-15", "0",  "15", "30", "45", "60", "75"), 
                     limits = c(-90, 90)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by Launch Angle") + 
  theme(legend.position = "none")

bb_outcome_by_launch_angle_data <- layer_data(bb_outcome_by_launch_angle)

launch_angle_single_1quartile <- bb_outcome_by_launch_angle_data[1, 3]
launch_angle_single_median <- bb_outcome_by_launch_angle_data[1, 4]
launch_angle_single_3quartile <- bb_outcome_by_launch_angle_data[1, 5]
launch_angle_out_1quartile <- bb_outcome_by_launch_angle_data[2, 3]
launch_angle_out_median <- bb_outcome_by_launch_angle_data[2, 4]
launch_angle_out_3quartile <- bb_outcome_by_launch_angle_data[2, 5]

ggplot(battedballs, 
       aes(x = events, y = launch_angle, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-75, 75, by = 15), 
                     labels = c("-75", "-60", "-45", "-30", "-15", "0",  "15", "30", "45", "60", "75"), 
                     limits = c(-90, 90)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by Launch Angle") + 
  theme(legend.position = "none") + 
  annotate("text", x = c(.55, .55, .55), 
           y = c(launch_angle_single_1quartile, launch_angle_single_median, launch_angle_single_3quartile), 
           label = c(launch_angle_single_1quartile, launch_angle_single_median, launch_angle_single_3quartile)) + 
  annotate("text", x = c(1.55,1.55, 1.55), 
           y = c(launch_angle_out_1quartile, launch_angle_out_median, launch_angle_out_3quartile),
           label = c(launch_angle_out_1quartile, launch_angle_out_median, launch_angle_out_3quartile))

# Compute the proportion of singles for each adv_bb_type.

la_prop_lgb <- (battedballs %>% filter(adv_bb_type == "low_ground_ball" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(adv_bb_type == "low_ground_ball") %>% summarize(n = n()))
la_prop_hgb <- (battedballs %>% filter(adv_bb_type == "high_ground_ball" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(adv_bb_type == "high_ground_ball") %>% summarize(n = n()))
la_prop_lld <- (battedballs %>% filter(adv_bb_type == "low_line_drive" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(adv_bb_type == "low_line_drive") %>% summarize(n = n()))
la_prop_hld <- (battedballs %>% filter(adv_bb_type == "high_line_drive" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(adv_bb_type == "high_line_drive") %>% summarize(n = n()))
la_prop_fb <- (battedballs %>% filter(adv_bb_type == "fly_ball" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(adv_bb_type == "fly_ball") %>% summarize(n = n()))
la_prop_pu <- (battedballs %>% filter(adv_bb_type == "popup" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(adv_bb_type == "popup") %>% summarize(n = n()))

# Plot the density of our Launch Angle predictor by result (single or out).

x_scale <- scale_x_continuous(breaks = seq(-75, 75, by = 15), 
                              labels = c("-75", "-60", "-45", "-30", "-15", "0",  "15", "30", "45", "60", "75"),
                              limits = NULL)
y_scale <- scale_y_continuous(limits = c(0, 0.045))
(la_density <- ggplot(battedballs, aes(x = launch_angle, y = stat(density), color = events)) + 
  geom_freqpoly(binwidth = 0.4) + 
  x_scale + 
  y_scale + 
  scale_color_manual(values = c(single = "red4", field_out = "royalblue3")) + 
  labs(title = "Density Plot of Launch Angle by Result (Single or Out)", 
       x = "Launch Angle (degrees)")) 

la_density_data <- layer_data(la_density)
la_density_data <- filter(la_density_data, y != "NA")

# Exit Velocity (launch_speed)

# Visualize singles and outs, by Exit Velocity.          .

ggplot(battedballs_sample, aes(hc_x_Kolp, hc_y_Kolp)) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 1) + theme_linedraw() + 
  labs(title = "Singles vs Outs, by launch_speed_cat") + 
  facet_grid(launch_speed_cat ~ events)

# Create boxplots to visualize the distribution of launch_speed by singles and outs.

bb_outcome_by_launch_speed <- ggplot(battedballs, 
                                     aes(x = events, y = launch_speed, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(10, 130, by = 15), 
                     labels = c("10", "25", "40", "55", "70", "85", "100",  "115", "130"), 
                     limits = c(10, 130)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by Exit Velocity") + 
  theme(legend.position = "none")

bb_outcome_by_launch_speed_data <- layer_data(bb_outcome_by_launch_speed)

launch_speed_single_1quartile <- bb_outcome_by_launch_speed_data[1, 3]
launch_speed_single_median <- bb_outcome_by_launch_speed_data[1, 4]
launch_speed_single_3quartile <- bb_outcome_by_launch_speed_data[1, 5]
launch_speed_out_1quartile <- bb_outcome_by_launch_speed_data[2, 3]
launch_speed_out_median <- bb_outcome_by_launch_speed_data[2, 4]
launch_speed_out_3quartile <- bb_outcome_by_launch_speed_data[2, 5]

ggplot(battedballs, 
       aes(x = events, y = launch_speed, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(10, 130, by = 15), 
                     labels = c("10", "25", "40", "55", "70", "85", "100",  "115", "130"), 
                     limits = c(10, 130)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by Exit Velocity") + 
  theme(legend.position = "none") + 
  annotate("text", x = c(.545, .545, .545), 
           y = c(launch_speed_single_1quartile, launch_speed_single_median, launch_speed_single_3quartile), 
           label = c(launch_speed_single_1quartile, launch_speed_single_median, launch_speed_single_3quartile)) + 
  annotate("text", x = c(1.55,1.55, 1.55), 
           y = c(launch_speed_out_1quartile, launch_speed_out_median, launch_speed_out_3quartile),
           label = c(launch_speed_out_1quartile, launch_speed_out_median, launch_speed_out_3quartile))

# Compute the proportion of singles for each launch_speed_cat.

ls_prop_lt70 <- (battedballs %>% filter(launch_speed_cat == "<70" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(launch_speed_cat == "<70") %>% summarize(n = n()))
ls_prop_to80 <- (battedballs %>% filter(launch_speed_cat == "70-79.9" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(launch_speed_cat == "70-79.9") %>% summarize(n = n()))
ls_prop_to90 <- (battedballs %>% filter(launch_speed_cat == "80-89.9" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(launch_speed_cat == "80-89.9") %>% summarize(n = n()))
ls_prop_100 <- (battedballs %>% filter(launch_speed_cat == "90-100" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(launch_speed_cat == "90-100") %>% summarize(n = n()))
ls_prop_gt100 <- (battedballs %>% filter(launch_speed_cat == ">100" & battedballs$events == "single") %>% summarize(n = n())) / 
  (battedballs %>% filter(launch_speed_cat == ">100") %>% summarize(n = n()))

# Plot the density of our Exit Velocity predictor (*launch_speed*) by result (single or out).

x_scale <- scale_x_continuous(breaks = seq(35, 125, by = 10), 
                              labels = c("35", "45", "55", "-65", "75", "85", 95,  "105", "115", "125"),
                              limits = c(35, 125))
y_scale <- scale_y_continuous(limits = c(0, 0.04))
(ls_density <- ggplot(battedballs, aes(x = launch_speed, y = stat(density), color = events)) + 
    geom_freqpoly(binwidth = 0.4) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(single = "red4", field_out = "royalblue3")) + 
    labs(title = "Density Plot of Exit Velocity by Result (Single or Out)", 
         x = "Exit Velocity (mph)")) 

ls_density_data <- layer_data(ls_density)
ls_density_data <- filter(ls_density_data, y != "NA", x != "NA")

# Spray Angle

# Create a random sample of 2,000 ground balls.

set.seed(1)
battedballs_gb_sample <- sample_n(filter(battedballs, launch_angle <= 10), 2000, replace = FALSE)

# Visualize singles and outs, by Spray Angle (Kolp's version).          .

spray_angle_Kolp_plot <- ggplot(battedballs_gb_sample, aes(hc_x_Kolp, hc_y_Kolp, color = if_fielding_alignment)) + 
  scale_color_manual(values = c(Standard = "black", Strategic = "cyan3", `Infield shift` = "magenta1")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 1) + theme_linedraw() + 
  labs(title = "Singles vs Outs, by spray_angle_Kolp") + 
  facet_grid(events ~ spray_angle_Kolp_cat)
gtable_show_names(spray_angle_Kolp_plot)
reposition_legend(spray_angle_Kolp_plot, position = 'center', panel = 'panel-9-2')

# Visualize singles and outs, by *spray_angle_adj*

spray_angle_adj_plot <- ggplot(battedballs_gb_sample, aes(hc_x_Kolp, hc_y_Kolp, color = if_fielding_alignment)) + 
  scale_color_manual(values = c(Standard = "black", Strategic = "cyan3", `Infield shift` = "magenta1")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 1) + theme_linedraw() + 
  labs(title = "Singles vs Outs, by spray_angle_adj") + 
  facet_grid(events ~ spray_angle_adj_cat)
gtable_show_names(spray_angle_adj_plot)
reposition_legend(spray_angle_adj_plot, position = 'center', panel = 'panel-1-1')

# Create boxplots to visualize the distribution of Spray Angle and Infield Alignment by singles and outs.

battedballs_gb <- battedballs %>% filter(launch_angle <= 10)
battedballs_gb_std <- battedballs %>% filter(launch_angle <= 10, if_fielding_alignment == "Standard")
battedballs_gb_strat <- battedballs %>% filter(launch_angle <= 10, if_fielding_alignment == "Strategic")
battedballs_gb_shift <- battedballs %>% filter(launch_angle <= 10, if_fielding_alignment == "Infield shift")

# Standard Infield Alignment
bb_outcome_by_spray_angle_Kolp_std <- ggplot(battedballs_gb_std, 
                                     aes(x = events, y = spray_angle_Kolp, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by spray_angle_Kolp and Standard Infield Alignment") + 
  theme(legend.position = "none")

bb_outcome_by_spray_angle_Kolp_std_data <- layer_data(bb_outcome_by_spray_angle_Kolp_std)

spray_angle_Kolp_single_std_1quartile <- bb_outcome_by_spray_angle_Kolp_std_data[1, 3]
spray_angle_Kolp_single_std_median <- bb_outcome_by_spray_angle_Kolp_std_data[1, 4]
spray_angle_Kolp_single_std_3quartile <- bb_outcome_by_spray_angle_Kolp_std_data[1, 5]
spray_angle_Kolp_out_std_1quartile <- bb_outcome_by_spray_angle_Kolp_std_data[2, 3]
spray_angle_Kolp_out_std_median <- bb_outcome_by_spray_angle_Kolp_std_data[2, 4]
spray_angle_Kolp_out_std_3quartile <- bb_outcome_by_spray_angle_Kolp_std_data[2, 5]

ggplot(battedballs_gb_std, 
       aes(x = events, y = spray_angle_Kolp, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by spray_angle_Kolp and Standard Infield Alignment") + 
  theme(legend.position = "none") + 
  annotate("text", x = c(.545, .545, .545), 
           y = c(spray_angle_Kolp_single_std_1quartile, spray_angle_Kolp_single_std_median, spray_angle_Kolp_single_std_3quartile), 
           label = c(spray_angle_Kolp_single_std_1quartile, spray_angle_Kolp_single_std_median, spray_angle_Kolp_single_std_3quartile)) + 
  annotate("text", x = c(1.55,1.55, 1.55), 
           y = c(spray_angle_Kolp_out_std_1quartile, spray_angle_Kolp_out_std_median, spray_angle_Kolp_out_std_3quartile),
           label = c(spray_angle_Kolp_out_std_1quartile, spray_angle_Kolp_out_std_median, spray_angle_Kolp_out_std_3quartile))

# Strategic Infield Alignment
bb_outcome_by_spray_angle_Kolp_strat <- ggplot(battedballs_gb_strat, 
                                             aes(x = events, y = spray_angle_Kolp, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by spray_angle_Kolp and Strategic Infield Alignment") + 
  theme(legend.position = "none")

bb_outcome_by_spray_angle_Kolp_strat_data <- layer_data(bb_outcome_by_spray_angle_Kolp_strat)

spray_angle_Kolp_single_strat_1quartile <- bb_outcome_by_spray_angle_Kolp_strat_data[1, 3]
spray_angle_Kolp_single_strat_median <- bb_outcome_by_spray_angle_Kolp_strat_data[1, 4]
spray_angle_Kolp_single_strat_3quartile <- bb_outcome_by_spray_angle_Kolp_strat_data[1, 5]
spray_angle_Kolp_out_strat_1quartile <- bb_outcome_by_spray_angle_Kolp_strat_data[2, 3]
spray_angle_Kolp_out_strat_median <- bb_outcome_by_spray_angle_Kolp_strat_data[2, 4]
spray_angle_Kolp_out_strat_3quartile <- bb_outcome_by_spray_angle_Kolp_strat_data[2, 5]

ggplot(battedballs_gb_strat, 
       aes(x = events, y = spray_angle_Kolp, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by spray_angle_Kolp and Strategic Infield Alignment") + 
  theme(legend.position = "none") + 
  annotate("text", x = c(.545, .545, .545), 
           y = c(spray_angle_Kolp_single_strat_1quartile, spray_angle_Kolp_single_strat_median, spray_angle_Kolp_single_strat_3quartile), 
           label = c(spray_angle_Kolp_single_strat_1quartile, spray_angle_Kolp_single_strat_median, spray_angle_Kolp_single_strat_3quartile)) + 
  annotate("text", x = c(1.55,1.55, 1.55), 
           y = c(spray_angle_Kolp_out_strat_1quartile, spray_angle_Kolp_out_strat_median, spray_angle_Kolp_out_strat_3quartile),
           label = c(spray_angle_Kolp_out_strat_1quartile, spray_angle_Kolp_out_strat_median, spray_angle_Kolp_out_strat_3quartile))

# Infield Shift Alignment
bb_outcome_by_spray_angle_Kolp_shift <- ggplot(battedballs_gb_shift, 
                                               aes(x = events, y = spray_angle_Kolp, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by spray_angle_Kolp and Infield Shift Alignment") + 
  theme(legend.position = "none")

bb_outcome_by_spray_angle_Kolp_shift_data <- layer_data(bb_outcome_by_spray_angle_Kolp_shift)

spray_angle_Kolp_single_shift_1quartile <- bb_outcome_by_spray_angle_Kolp_shift_data[1, 3]
spray_angle_Kolp_single_shift_median <- bb_outcome_by_spray_angle_Kolp_shift_data[1, 4]
spray_angle_Kolp_single_shift_3quartile <- bb_outcome_by_spray_angle_Kolp_shift_data[1, 5]
spray_angle_Kolp_out_shift_1quartile <- bb_outcome_by_spray_angle_Kolp_shift_data[2, 3]
spray_angle_Kolp_out_shift_median <- bb_outcome_by_spray_angle_Kolp_shift_data[2, 4]
spray_angle_Kolp_out_shift_3quartile <- bb_outcome_by_spray_angle_Kolp_shift_data[2, 5]

ggplot(battedballs_gb_shift, 
       aes(x = events, y = spray_angle_Kolp, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by spray_angle_Kolp and Infield Shift Alignment") + 
  theme(legend.position = "none") + 
  annotate("text", x = c(.545, .545, .545), 
           y = c(spray_angle_Kolp_single_shift_1quartile, spray_angle_Kolp_single_shift_median, spray_angle_Kolp_single_shift_3quartile), 
           label = c(spray_angle_Kolp_single_shift_1quartile, spray_angle_Kolp_single_shift_median, spray_angle_Kolp_single_shift_3quartile)) + 
  annotate("text", x = c(1.55,1.55, 1.55), 
           y = c(spray_angle_Kolp_out_shift_1quartile, spray_angle_Kolp_out_shift_median, spray_angle_Kolp_out_shift_3quartile),
           label = c(spray_angle_Kolp_out_shift_1quartile, spray_angle_Kolp_out_shift_median, spray_angle_Kolp_out_shift_3quartile))

# Compute the proportion of singles for each *spray_angle_Kolp_cat* and Infield Fielding Alignment.

(sa_std_prop_LFline <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "LF Line (-45:-40)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "LF Line (-45:-40)") %>% summarize(n = n())))

(sa_std_prop_3b <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "3rd Baseman (-39.9:-31)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "3rd Baseman (-39.9:-31)") %>% summarize(n = n())))

(sa_std_prop_hole56 <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Hole 5-6 (-30.9:-22)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Hole 5-6 (-30.9:-22)") %>% summarize(n = n())))

(sa_std_prop_ss <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Shortstop (-21.9:-9)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Shortstop (-21.9:-9)") %>% summarize(n = n())))

(sa_std_prop_holeMid <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Hole Middle (-8.9:9)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Hole Middle (-8.9:9)") %>% summarize(n = n())))

(sa_std_prop_2b <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "2nd Baseman (9.1:25)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "2nd Baseman (9.1:25)") %>% summarize(n = n())))

(sa_std_prop_hole34 <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Hole 3-4 (25.1:35)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "Hole 3-4 (25.1:35)") %>% summarize(n = n())))

(sa_std_prop_1b <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "1st Baseman (35.1:43)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "1st Baseman (35.1:43)") %>% summarize(n = n())))

(sa_std_prop_RFline <- (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "RF Line (43.1:45)" & events == "single") %>% summarize(n = n())) / 
  (battedballs_gb_std %>% filter(spray_angle_Kolp_cat == "RF Line (43.1:45)") %>% summarize(n = n())))

# Compute the proportion of singles for each *spray_angle_adj_cat* with an Infield Shift Alignment.

(sa_shift_prop_LFline <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Down Line (-45:-40)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Down Line (-45:-40)") %>% summarize(n = n())))

(sa_shift_prop_3b <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Corner Inf (-39.9:-31)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Corner Inf (-39.9:-31)") %>% summarize(n = n())))

(sa_shift_prop_hole56 <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Side Hole (-30.9:-22)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Side Hole (-30.9:-22)") %>% summarize(n = n())))

(sa_shift_prop_ss <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Mid Inf (-21.9:-9)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Pulled Mid Inf (-21.9:-9)") %>% summarize(n = n())))

(sa_shift_prop_holeMid <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Middle Hole (-8.9:9)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Middle Hole (-8.9:9)") %>% summarize(n = n())))

(sa_shift_prop_2b <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Mid Inf (9.1:25)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Mid Inf (9.1:25)") %>% summarize(n = n())))

(sa_shift_prop_hole34 <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Side Hole (25.1:35)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Side Hole (25.1:35)") %>% summarize(n = n())))

(sa_shift_prop_1b <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Corner Inf (35.1:43)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Corner Inf (35.1:43)") %>% summarize(n = n())))

(sa_shift_prop_RFline <- (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Down Line (43.1:45)" & events == "single") %>% summarize(n = n())) / 
    (battedballs_gb_shift %>% filter(spray_angle_adj_cat == "Oppo Down Line (43.1:45)") %>% summarize(n = n())))

# Plot the density of *spray_angle_Kolp* by result (single or out) and a Standard infield alignment.

x_scale <- scale_x_continuous(breaks = seq(-45, 45, by = 10), 
                              labels = c("-45", "-35", "-25", "-15", "-5", "5", "15", "25", "35", "45"), 
                              limits = c(-50, 50)) 
y_scale <- scale_y_continuous(limits = c(0, 0.035))
(sa_Kolp_density <- ggplot(battedballs_gb_std, aes(x = spray_angle_Kolp, y = stat(density), color = events)) + 
    geom_freqpoly(binwidth = 0.4) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(single = "red4", field_out = "royalblue3")) + 
    labs(title = "Density Plot of Spray Angle (Kolp) by Result (Single or Out) 
         and a Standard Infield Alignment", x = "Spray Angle Kolp (degrees)") + 
    theme(legend.position = c(.85, .9)))

sa_Kolp_density_data <- layer_data(sa_Kolp_density)
sa_Kolp_density_data <- filter(sa_Kolp_density_data, y != "NA", x != "NA")

# Plot the density of *spray_angle_adj* by result (single or out) and a shifted infield alignment.

x_scale <- scale_x_continuous(breaks = seq(-45, 45, by = 10), 
                              labels = c("-45", "-35", "-25", "-15", "-5", "5", "15", "25", "35", "45"), 
                              limits = c(-50, 50)) 
y_scale <- scale_y_continuous(limits = c(0, 0.035))
(sa_adj_density <- ggplot(battedballs_gb_shift, aes(x = spray_angle_adj, y = stat(density), color = events)) + 
    geom_freqpoly(binwidth = 1) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(single = "red4", field_out = "royalblue3")) + 
    labs(title = "Density Plot of spray_angle_adj by Result (Single or Out) 
         and a Shifted Infield Alignment", x = "spray_angle_adj (degrees)") + 
    theme(legend.position = c(.85, .9)))

sa_adj_density_data <- layer_data(sa_adj_density)
sa_adj_density_data <- filter(sa_adj_density_data, y != "NA", x != "NA")

# Home to First

# Create a subset of *battedballs* consisting only of Topped and Weakly Hit Ground Balls.

battedballs_lowgb_topweak <- battedballs %>% filter(bb_type == "ground_ball" & adv_bb_type == "low_ground_ball")
battedballs_lowgb_topweak <- battedballs_lowgb_topweak %>% filter(launch_speed_angle == "1" | 
                                                                    launch_speed_angle == "2")
                                              
set.seed(1)
battedballs_lowgb_topweak_sample <- sample_n(battedballs_lowgb_topweak, 3000, replace = FALSE)

# Visualize singles and outs, by Home to First predictor (topped and weakly hit ground balls only).          .

ggplot(battedballs_lowgb_topweak_sample, aes(hc_x_Kolp, hc_y_Kolp)) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 1) + theme_linedraw() + 
  labs(title = "Singles vs Outs, by hp_to_1b") + 
  facet_grid(hp_to_1b_cat ~ events)

# Create boxplots to visualize the distribution of hp_to_1b by singles and outs.

bb_outcome_by_hp_to_1b <- ggplot(battedballs_lowgb_topweak, 
                                     aes(x = events, y = hp_to_1b, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(3.9, 5.1, by = 0.1), 
                     labels = c("3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5",  "4.6", 
                                "4.7", "4.8", "4.9", "5.0", "5.1"), 
                     limits = c(3.9, 5.1)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by Home to First 
          (topped and weakly hit ground balls only)") + 
  theme(legend.position = "none")

bb_outcome_by_hp_to_1b_data <- layer_data(bb_outcome_by_hp_to_1b)

hp_to_1b_single_1quartile <- bb_outcome_by_hp_to_1b_data[1, 3]
hp_to_1b_single_median <- bb_outcome_by_hp_to_1b_data[1, 4]
hp_to_1b_single_3quartile <- bb_outcome_by_hp_to_1b_data[1, 5]
hp_to_1b_out_1quartile <- bb_outcome_by_hp_to_1b_data[2, 3]
hp_to_1b_out_median <- bb_outcome_by_hp_to_1b_data[2, 4]
hp_to_1b_out_3quartile <- bb_outcome_by_hp_to_1b_data[2, 5]

ggplot(battedballs_lowgb_topweak, 
       aes(x = events, y = hp_to_1b, color = events)) + 
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(3.9, 5.1, by = 0.1), 
                     labels = c("3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5",  "4.6", 
                                "4.7", "4.8", "4.9", "5.0", "5.1"), 
                     limits = c(3.9, 5.1)) + 
  scale_color_manual(values = c("red4", "royalblue3")) + 
  ggtitle("Singles vs Outs, by Home to First 
          (topped and weakly hit ground balls only)") + 
  theme(legend.position = "none") + 
  annotate("text", x = c(.545, .545, .545), 
           y = c(hp_to_1b_single_1quartile, hp_to_1b_single_median, hp_to_1b_single_3quartile), 
           label = c(hp_to_1b_single_1quartile, hp_to_1b_single_median, hp_to_1b_single_3quartile)) + 
  annotate("text", x = c(1.55,1.55, 1.55), 
           y = c(hp_to_1b_out_1quartile, hp_to_1b_out_median, hp_to_1b_out_3quartile),
           label = c(hp_to_1b_out_1quartile, hp_to_1b_out_median, hp_to_1b_out_3quartile))

# Create violin plots to visualize the distribution of hp_to_1b by singles and outs.

ggplot(battedballs_lowgb_topweak, 
       aes(x = events, y = hp_to_1b)) + 
  geom_violin(fill = "slategray1") + 
  scale_y_continuous(breaks = seq(3.9, 5.1, by = 0.1), 
                     labels = c("3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5",  "4.6", 
                                "4.7", "4.8", "4.9", "5.0", "5.1"), 
                     limits = c(3.9, 5.1)) + 
  ggtitle("Singles vs Outs, by Home to First 
          (topped and weakly hit ground balls only)") + 
  theme(legend.position = "none")

# Plot the density of our Home to First predictor (*hp_to_1b*) by result (single or out).

x_scale <- scale_x_continuous(breaks = seq(3.9, 5.1, by = 0.1), 
                              labels = c("3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5",  "4.6", 
                                         "4.7", "4.8", "4.9", "5.0", "5.1"), 
                              limits = c(3.9, 5.1))
y_scale <- scale_y_continuous() 
(h1b_density <- ggplot(battedballs_lowgb_topweak, aes(x = hp_to_1b, y = stat(density), color = events)) + 
    geom_freqpoly(binwidth = .08) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(single = "red4", field_out = "royalblue3")) + 
    labs(title = "Density Plot of Home to First by Result (Single or Out)", 
         x = "Home to First (seconds)")) 

h1b_density_data <- layer_data(h1b_density)
h1b_density_data <- filter(h1b_density_data, y != "NA", x != "NA")

######################################################
# Prepare Data for Use in Machine Learning Algorithms 
######################################################

# Make sure battedballs$events is coded as a factor with levels 
# single, followed by field_out.

battedballs$events <- factor(battedballs$events, levels = c("single", "field_out"))

# Confirm which level of battedballs$events has been assigned to 0.

contrasts(as.factor(battedballs$events)) 

# Center the predictor variables to reduce non-essential multicollinearity among the predictors and 
# improve the interpretation of their coefficients.

battedballs <- battedballs %>% mutate(c_launch_angle = launch_angle - mean(launch_angle))
battedballs <- battedballs %>% mutate(c_launch_speed = launch_speed - mean(launch_speed))
battedballs <- battedballs %>% mutate(c_spray_angle_Kolp = spray_angle_Kolp - mean(spray_angle_Kolp))
battedballs <- battedballs %>% mutate(c_spray_angle_adj = spray_angle_adj - mean(spray_angle_adj))
battedballs <- battedballs %>% mutate(c_hp_to_1b = hp_to_1b - mean(hp_to_1b))
battedballs <- battedballs %>% mutate(c_if_alignment = as.numeric(num_if_alignment) - mean(as.numeric(num_if_alignment)))

# Standardize the predictor variables.

battedballs <- battedballs %>% mutate(s_launch_angle = c_launch_angle / sd(launch_angle))
battedballs <- battedballs %>% mutate(s_launch_speed = c_launch_speed / sd(launch_speed))
battedballs <- battedballs %>% mutate(s_spray_angle_Kolp = c_spray_angle_Kolp / sd(spray_angle_Kolp))
battedballs <- battedballs %>% mutate(s_spray_angle_adj = c_spray_angle_adj / sd(spray_angle_adj))
battedballs <- battedballs %>% mutate(s_hp_to_1b = c_hp_to_1b / sd(hp_to_1b))
battedballs <- battedballs %>% mutate(s_if_alignment = c_if_alignment / sd(as.numeric(num_if_alignment)))

# Normalize the predictor variables.

battedballs <- battedballs %>% 
  mutate(n_launch_angle = (launch_angle - min(launch_angle)) / (max(launch_angle) - min(launch_angle)))
battedballs <- battedballs %>% 
  mutate(n_launch_speed = (launch_speed - min(launch_speed)) / (max(launch_speed) - min(launch_speed)))
battedballs <- battedballs %>% 
  mutate(n_spray_angle_Kolp = (spray_angle_Kolp - min(spray_angle_Kolp)) / (max(spray_angle_Kolp) - min(spray_angle_Kolp))) 
battedballs <- battedballs %>% 
  mutate(n_spray_angle_adj = (spray_angle_adj - min(spray_angle_adj)) / (max(spray_angle_adj) - min(spray_angle_adj))) 
battedballs <- battedballs %>% 
  mutate(n_hp_to_1b = (hp_to_1b - min(hp_to_1b)) / (max(hp_to_1b) - min(hp_to_1b))) 
battedballs <- battedballs %>% 
  mutate(n_if_alignment = (num_if_alignment - min(num_if_alignment)) / (max(num_if_alignment) - min(num_if_alignment))) 

# Partion battedballs.

set.seed(1)
test_index <- createDataPartition(y = battedballs$events, times = 1, p = 0.20, 
                                  list = FALSE)
train_set <- battedballs[-test_index,]
test_set <- battedballs[test_index,]

#################################################
# Build and Assess a Baseline Model
#################################################

# Develop a baseline model that guesses the outcomes in the test_set.

set.seed(1)
bl_preds <- sample(c("single", "field_out"), size = length(test_index), 
                   replace = TRUE, prob = c(0.25, 0.75)) %>% 
    factor(levels = levels(test_set$events))
(bl_confusionM <- confusionMatrix(data = bl_preds, reference = test_set$events))
table(predicted = bl_preds, actual = test_set$events)
(bl_tpr <- round(sensitivity(bl_preds, reference = factor(test_set$events)), 6))
(bl_tnr <- round(specificity(bl_preds, reference = factor(test_set$events)), 6))
(bl_fpr <- round(1 - bl_tnr, 6))
(bl_truescore <- round((2 * bl_tpr * bl_tnr) / (bl_tpr + bl_tnr) ,6))

#################################################
# Build and Assess a Logistic Regression Model
#################################################

# Confirm which level of train_set$events has been assigned to 0.

contrasts(as.factor(train_set$events))

# Fit a model to train_set$events using logistic regression.

fit_logit <- train_set %>% mutate(y = as.numeric(events == "field_out")) %>% 
  glm(y ~ s_launch_angle + s_launch_speed + s_spray_angle_Kolp + s_spray_angle_adj + 
        s_hp_to_1b + s_if_alignment, data = ., family = "binomial")

# Make sure each regression coefficient is statistically significant (p < .05).

summary(fit_logit)
exp(coef(fit_logit))
exp(confint(fit_logit))

# Using our logistic regression model (fit_logit), estimate the probability
# of a single for each observation (batted ball) in test_set.

p_hat_logit <- predict(fit_logit, newdata = test_set, type = "response") 
test_set <- add_column(test_set, p_hat_logit)

# Apply a decision rule allowing the model to predict whether a particular
# batted ball in test_set will result in a single.

  # Determine the cutoff in the decision rule by identifying the cutoff (probability) 
  # that maximizes the truescore.

cutoffs <- test_set$p_hat_logit
truescores <- map_dbl(cutoffs, function(x){
  truescore_preds <- ifelse(test_set$p_hat_logit >= x, "single", "field_out") %>% 
    factor(levels = levels(test_set$events))
  truescore_tprs <- round(sensitivity(truescore_preds, reference = factor(test_set$events)), 6)
  truescore_tnrs <- round(specificity(truescore_preds, reference = factor(test_set$events)), 6)
  truescores <- round((2 * truescore_tprs * truescore_tnrs) / (truescore_tprs + truescore_tnrs) ,6) 
})
test_set <- mutate(test_set, truescores = truescores)
(max_truescore <- max(truescores))
(max_truescore_cutoff <- round(cutoffs[which.max(truescores)], 6))
qplot(x = cutoffs, y = truescores) + annotate("text", x = 0.593, y = 0.39, 
                                              label = "Max. Truescore (0.4081) at Cutoff (0.7500)")

  # Apply a decision rule based on the cutoff that maximizes the truescore.

max_truescore_preds <- ifelse(p_hat_logit >= max_truescore_cutoff, "single", "field_out") %>% 
  factor(levels = levels(test_set$events))
test_set <- add_column(test_set, max_truescore_preds)
(max_truescore_tpr <- round(sensitivity(max_truescore_preds, reference = factor(test_set$events)), 6))
(max_truescore_tnr <- round(specificity(max_truescore_preds, reference = factor(test_set$events)), 6))
(max_truescore_fpr <- round(1 - max_truescore_tnr, 6))


  # Determine the cutoff in the decision rule by identifying the probability that 
  # minimizes the distance from the model's ROC curve to the coordinate (0, 1).

pred <- prediction(test_set$p_hat_logit, test_set$events, label.ordering = c("field_out", "single"))
perf <- performance(pred, measure="tpr", x.measure="fpr")
rocr_perf_elements <- tibble(cutoffs = perf@alpha.values[[1]], 
                             FPR = round(perf@x.values[[1]], 6), 
                             TPR = round(perf@y.values[[1]], 6))
rocr_perf_elements <- rocr_perf_elements %>% mutate(distance = sqrt((1 - TPR)^2 + (FPR)^2))

(min_distance <- round(min(rocr_perf_elements$distance), 6))
(min_distance_cutoff <- round(rocr_perf_elements$cutoffs[which.min(rocr_perf_elements$distance)], 6))
(min_distance_tpr <- round(rocr_perf_elements$TPR[which.min(rocr_perf_elements$distance)], 6))
(min_distance_fpr <- round(rocr_perf_elements$FPR[which.min(rocr_perf_elements$distance)], 6))
(min_distance_tnr <- round(1 - min_distance_fpr, 6))

# Create a ROC curve visualizing the optimal cutoff points identified 
# by our two methods: Max Truescore and Min Distance.

plot(perf, main = "ROC Curve Showing Single-Out TPR-FPR Trade-Offs 
     at Max Truescore Cutoff and Min Distance Cutoff", 
     print.cutoffs.at = c(0.749984, 0.764628, 0.8213, 0.7022, 0.6646), 
     cutoff.label.function = function(x) { round(x, 4) }, 
     points.pch = c(15, 17, 21, 21, 21), 
     points.col = c("dark red", "blue", "black", "black", "black"), 
     points.cex = c(1.8, 1.8, 1.8, 1.8, 1.8), text.font = c(2, 2, 2, 2, 2), 
     text.pos = c(4, 2, 4, 4, 4), 
     cutoff.label.text = c("Max Truescore", "Min Distance", "ref", "ref", "ref"), 
     xaxis.tck = 1, yaxis.tck = 1, xaxis.col.tick = "light gray", 
     yaxis.col.tick = "light gray") 
legend("topleft", inset = 0.1, title = "Cutoff Method", 
       legend = c("Max Truescore", "ROC Min Distance to (0, 1)"), 
       pch = c(15, 17), col = c("dark red", "blue"))

# Compute the truescore of the optimal cutoff point identified by minimizing the distance.

(min_distance_truescore <- round((2 * min_distance_tpr * min_distance_tnr) / (min_distance_tpr + min_distance_tnr), 6))

# Compute the distance of the optimal cutoff point identified by maximizing the truescore.

(max_truescore_distance <- round(sqrt((1 - max_truescore_tpr)^2 + (max_truescore_fpr)^2), 6))

# Create a table displaying the assessment results obtained thus far.

assessment_results <- tibble(Model = c("Baseline", "Logistic Regression", "Logistic Regression"), 
                               `Cutoff Method` = c(NA, "Max Truescore", "ROC Min Distance to (0,1)"),
                               `Truescore` = c(bl_truescore, max_truescore, min_distance_truescore), 
                               TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr),
                               TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr), 
                               ROC_Distance = c(NA, max_truescore_distance, min_distance), 
                               `TPR (repeated)` = c(bl_tpr, max_truescore_tpr, min_distance_tpr), 
                               FPR = c((1 - bl_tnr), max_truescore_fpr, min_distance_fpr), 
                               `Best Cutoff` = c(NA, max_truescore_cutoff, min_distance_cutoff))
knitr::kable(assessment_results[1:3, ], caption = "Assessment Results - First Logistic Regression Model")

# Fit a refined logistic regression model (r_fit_logit) with interaction terms to 
# train_set$events.

r_fit_logit <- train_set %>% mutate(y = as.numeric(events == "field_out")) %>% 
  glm(y ~ s_launch_angle + s_launch_speed + s_spray_angle_Kolp + 
        s_spray_angle_adj + s_hp_to_1b + s_if_alignment + 
        s_spray_angle_Kolp:s_if_alignment + s_spray_angle_adj:s_if_alignment,  
      data = ., family = "binomial")

# Make sure each regression coefficient is statistically significant (p < .05).

summary(r_fit_logit)
exp(coef(r_fit_logit))
exp(confint(r_fit_logit))

# Create a conditional effect plot to visualize how infield alignment affects 
# the coefficient for s_spray_angle_adj.

interplot(m = r_fit_logit, var1 = "s_spray_angle_adj", var2 = "s_if_alignment") + 
  geom_point(color = "dark red") + 
  ggtitle("Estimated Coefficient for Spray Angle Effect on Single,  
          by Alignment of Infielders") + 
  theme(plot.title = element_text(face = "bold")) + 
  xlab("Infield Alignment Effect (Standard, Strategic, Shift)") + 
  ylab("Estimated Coefficient for s_spray_angle_adj") + 
  theme_bw()

# Create a conditional effect plot to visualize how infield alignment affects 
# the coefficient for s_spray_angle_Kolp.

interplot(m = r_fit_logit, var1 = "s_spray_angle_Kolp", var2 = "s_if_alignment") + 
  geom_point(color = "dark red") + 
  ggtitle("Estimated Coefficient for Spray Angle Effect on Single,  
          by Alignment of Infielders") + 
  theme(plot.title = element_text(face = "bold")) + 
  xlab("Infield Alignment Effect (Standard, Strategic, Shift)") + 
  ylab("Estimated Coefficient for s_spray_angle_Kolp") + 
  theme_bw()

# Using our refined logistic regression model (r_fit_logit), estimate the probability
# of a single for each observation (batted ball) in test_set.

r_p_hat_logit <- predict(r_fit_logit, newdata = test_set, type = "response") 
test_set <- add_column(test_set, r_p_hat_logit)

# Apply a decision rule allowing the refined model to predict whether a particular
# batted ball in test_set will result in a single.

  # Determine the cutoff in the decision rule by identifying the cutoff (probability) 
  # that maximizes the truescore.

r_cutoffs <- test_set$r_p_hat_logit
r_truescores <- map_dbl(r_cutoffs, function(x){
  r_truescore_preds <- ifelse(test_set$r_p_hat_logit >= x, "single", "field_out") %>% 
    factor(levels = levels(test_set$events))
  r_truescore_tprs <- round(sensitivity(r_truescore_preds, reference = factor(test_set$events)), 6)
  r_truescore_tnrs <- round(specificity(r_truescore_preds, reference = factor(test_set$events)), 6)
  r_truescores <- round((2 * r_truescore_tprs * r_truescore_tnrs) / (r_truescore_tprs + r_truescore_tnrs) ,6) 
})
test_set <- mutate(test_set, r_truescores = r_truescores)
(r_max_truescore <- max(r_truescores))
(r_max_truescore_cutoff <- round(r_cutoffs[which.max(r_truescores)], 6))
qplot(x = r_cutoffs, y = r_truescores) + annotate("text", x = 0.575, y = 0.39, 
                                                  label = "Max. Truescore (0.4077) at Cutoff (0.7509)")

  # Apply a decision rule based on the cutoff that maximizes the truescore.

r_max_truescore_preds <- ifelse(r_p_hat_logit >= r_max_truescore_cutoff, "single", "field_out") %>% 
  factor(levels = levels(test_set$events))
test_set <- add_column(test_set, r_max_truescore_preds)
(r_max_truescore_tpr <- round(sensitivity(r_max_truescore_preds, reference = factor(test_set$events)), 6))
(r_max_truescore_tnr <- round(specificity(r_max_truescore_preds, reference = factor(test_set$events)), 6))
(r_max_truescore_fpr <- round(1 - r_max_truescore_tnr, 6))

  # Determine the cutoff in the decision rule by identifying the probability that 
  # minimizes the distance from the model's ROC curve to the coordinate (0, 1). Then apply
  # a decision rule using that probability (cutoff).

r_pred <- prediction(test_set$r_p_hat_logit, test_set$events, label.ordering = c("field_out", "single"))
r_perf <- performance(r_pred, measure="tpr", x.measure="fpr")
r_rocr_perf_elements <- tibble(r_cutoffs = r_perf@alpha.values[[1]], 
                               r_FPR = round(r_perf@x.values[[1]], 6), 
                               r_TPR = round(r_perf@y.values[[1]], 6))
r_rocr_perf_elements <- r_rocr_perf_elements %>% mutate(r_distance = sqrt((1 - r_TPR)^2 + (r_FPR)^2))

(r_min_distance <- round(min(r_rocr_perf_elements$r_distance), 6))
(r_min_distance_cutoff <- round(r_rocr_perf_elements$r_cutoffs[which.min(r_rocr_perf_elements$r_distance)], 6))
(r_min_distance_tpr <- round(r_rocr_perf_elements$r_TPR[which.min(r_rocr_perf_elements$r_distance)], 6))
(r_min_distance_fpr <- round(r_rocr_perf_elements$r_FPR[which.min(r_rocr_perf_elements$r_distance)], 6))
(r_min_distance_tnr <- round(1 - r_min_distance_fpr, 6))

# Create a ROC curve allowing us to visualize the optimal cutoff points identified 
# by our two methods: Max Truescore and Min Distance.

plot(r_perf, main = "ROC Curve (Refined Model):
Single-Out TPR-FPR Trade-Offs at Max Truescore Cutoff 
     and Min Distance Cutoff", 
     print.cutoffs.at = c(0.750887, 0.766355, 0.6610, 0.6994, 0.8240), 
     cutoff.label.function = function(x) { round(x, 4) }, 
     points.pch = c(15, 17, 21, 21, 21), 
     points.col = c("dark red", "blue", "black", "black", "black"), 
     points.cex = c(1.8, 1.8, 1.8, 1.8, 1.8), text.font = c(2, 2, 2, 2, 2), 
     text.pos = c(4, 2, 4, 4, 4), 
     cutoff.label.text = c("Max Truescore", "Min Distance", "ref", "ref", "ref"), 
     xaxis.tck = 1, yaxis.tck = 1, xaxis.col.tick = "light gray", 
     yaxis.col.tick = "light gray") 
legend("topleft", inset = 0.1, title = "Cutoff Method", 
       legend = c("Max Truescore", "ROC Min Distance to (0, 1)"), 
       pch = c(15, 17), col = c("dark red", "blue"))

# Compute the truescore of the optimal cutoff point identified by minimizing the distance.

(r_min_distance_truescore <- round((2 * r_min_distance_tpr * r_min_distance_tnr) / (r_min_distance_tpr + r_min_distance_tnr), 6))

# Compute the distance of the optimal point identified by maximizing the weighted truescore.

(r_max_truescore_distance <- round(sqrt((1 - r_max_truescore_tpr)^2 + (r_max_truescore_fpr)^2), 6))

# Create a table displaying all of the assessment results.

r_assessment_results <- tibble(Model = c("Baseline", "Logistic Regression", "Logistic Regression", "Logistic Regression", "Logistic Regression"), 
                               `Cutoff Method` = c("na", "Max Truescore", "ROC Min Distance to (0,1)", "Max Truescore (refined)", "ROC Min Distance to (0,1)(refined)"),
                               `Truescore` = c(bl_truescore, max_truescore, min_distance_truescore, r_max_truescore, r_min_distance_truescore), 
                               TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr, r_max_truescore_tpr, r_min_distance_tpr),
                               TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr, r_max_truescore_tnr, r_min_distance_tnr), 
                               `ROC Distance` = c("na", max_truescore_distance, min_distance, r_max_truescore_distance, r_min_distance), 
                               `TPR (repeated)` = c(bl_tpr, max_truescore_tpr, min_distance_tpr, r_max_truescore_tpr, r_min_distance_tpr), 
                               FPR = c((1 - bl_tnr), max_truescore_fpr, min_distance_fpr, r_max_truescore_fpr, r_min_distance_fpr), 
                               `Best Cutoff` = c("na", max_truescore_cutoff, min_distance_cutoff, r_max_truescore_cutoff, r_min_distance_cutoff))
knitr::kable(r_assessment_results[1:5, ], caption = "Assessment Results - Refined Logistic Regression Model")

#####################################################
# Build and Assess a K-Nearest Neighbors (knn) Model
#####################################################

# Build a knn model with k=5. Prepare the train and test sets, define the response vector and matrix of predictors, 
# and fit the model using the knn3 function.

train_set2 <- dplyr::select(train_set, -(hit_distance_sc))
test_set2 <- dplyr::select(test_set, -(hit_distance_sc), -(p_hat_logit), -(truescores), 
                           -(max_truescore_preds), -(r_p_hat_logit), -(r_truescores), -(r_max_truescore_preds))
x <- as.matrix(train_set2[, c("n_launch_angle", "n_launch_speed", "n_spray_angle_Kolp", "n_spray_angle_adj", 
                              "n_hp_to_1b", "n_if_alignment")])
y <- factor(train_set2$events)
knn_fit_k5 <- knn3(x, y, k = 5)

# Test the knn_fit_k5 model. Define the matrix of new data (test_set) and use it to predict the outcomes.

z <- as.matrix(test_set2[, c("n_launch_angle", "n_launch_speed", "n_spray_angle_Kolp", "n_spray_angle_adj", 
                             "n_hp_to_1b", "n_if_alignment")])
knn_y_hat <- predict(knn_fit_k5, z, type = "class")
knn_y_hat_prob <- predict(knn_fit_k5, z, type = "prob")

# Assess the predictions using both the Truescore method and the Minimum Distance method. 
# Compare the performance of knn_fit_k5 to that of the logistic regression models.

confusionMatrix(data = knn_y_hat, reference = test_set2$events)
(knn_tpr <- round(sensitivity(knn_y_hat, reference = factor(test_set2$events)), 6))
(knn_tnr <- round(specificity(knn_y_hat, reference = factor(test_set2$events)), 6))
(knn_fpr <- round(1 - knn_tnr, 6))
(knn_truescore <- round((2 * knn_tpr * knn_tnr) / (knn_tpr + knn_tnr) , 6))
(knn_distance <- round(sqrt((1 - knn_tpr)^2 + (knn_fpr)^2), 6))

assessment_results <- tibble(Model = c("Baseline", "Logistic Regression", "Logistic Regression", "K-Nearest Neighbors"), 
                             `Cutoff Method` = c(NA, "Truescore", "ROC Distance to (0,1)", NA),
                             `Truescore` = c(bl_truescore, max_truescore, min_distance_truescore, knn_truescore), 
                             TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr, knn_tpr),
                             TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr, knn_tnr), 
                             ROC_Distance = c(NA, max_truescore_distance, min_distance, knn_distance), 
                             `TPR (repeated)` = c(bl_tpr, max_truescore_tpr, min_distance_tpr, knn_tpr), 
                             FPR = c((1 - bl_tnr), max_truescore_fpr, min_distance_fpr, knn_fpr), 
                             `Best Cutoff` = c(NA, max_truescore_cutoff, min_distance_cutoff, NA))
knitr::kable(assessment_results[1:4, ], caption = "Assessment Results")

# Develop a knn algorithm with an optimal value for k.

  # Determine the value of k that maximizes the truescore of the model.

fitControl <- trainControl(method = "cv", number = 10, p = 0.8, returnData = TRUE, 
                           returnResamp = "all", savePredictions = "all", 
                           summaryFunction = twoClassSummary, classProbs = TRUE)
set.seed(1)
knn_train <- train(x, y, method = "knn", 
                   tuneGrid = data.frame(k = seq(1, 39, 2)), 
                   trControl = fitControl)

knn_train_sample_results <- knn_train[["resample"]]

knn_train_sample_results <- knn_train_sample_results %>% 
  mutate(knn_truescores = (2 * Sens * Spec) / (Sens + Spec))
knn_train_sample_results <- knn_train_sample_results %>% 
  mutate(knn_distances = sqrt((1 - Sens)^2 + (1 - Spec)^2))

knn_train_sample_mean_truescores <- knn_train_sample_results %>% group_by(k) %>% 
  summarize(mean_knn_truescores = mean(knn_truescores))
ggplot(knn_train_sample_mean_truescores, aes(x = k, y = mean_knn_truescores)) + 
  geom_point() + geom_line()
max(knn_train_sample_mean_truescores$mean_knn_truescores)
knn_train_sample_mean_truescores$k[which.max(knn_train_sample_mean_truescores$mean_knn_truescores)]

knn_train_sample_mean_distances <- knn_train_sample_results %>% group_by(k) %>% 
  summarize(mean_knn_distances = mean(knn_distances))
ggplot(knn_train_sample_mean_distances, aes(x = k, y = mean_knn_distances)) + 
  geom_point() + geom_line()
min(knn_train_sample_mean_distances$mean_knn_distances)
knn_train_sample_mean_distances$k[which.min(knn_train_sample_mean_distances$mean_knn_distances)]

  # Fit an optimized knn model (k = 7) to the entire predictor matrix and outcome vector.

knn_fit_k7 <- knn3(x, y, k = 7)

# Apply our final knn model (knn_fit_k7) to predict outcomes based on the predictors in test_set2.

knn_y_hat <- predict(knn_fit_k7, z, type = "class")
knn_y_hat_prob <- predict(knn_fit_k7, z, type = "prob")

# Assess the predictive ability of our final knn model (knn_fit_k7).

confusionMatrix(data = knn_y_hat, reference = test_set2$events)
(knn_tpr <- round(sensitivity(knn_y_hat, reference = factor(test_set2$events)), 6))
(knn_tnr <- round(specificity(knn_y_hat, reference = factor(test_set2$events)), 6))
(knn_fpr <- round(1 - knn_tnr, 6))
(knn_truescore <- round((2 * knn_tpr * knn_tnr) / (knn_tpr + knn_tnr) , 6))
(knn_distance <- round(sqrt((1 - knn_tpr)^2 + (knn_fpr)^2), 6))

assessment_results <- tibble(Model = c("Baseline", "Logistic Regression", "Logistic Regression", "K-Nearest Neighbors"), 
                             `Cutoff Method` = c(NA, "Truescore", "ROC Distance to (0,1)", NA),
                             `Truescore` = c(bl_truescore, max_truescore, min_distance_truescore, knn_truescore), 
                             TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr, knn_tpr),
                             TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr, knn_tnr), 
                             ROC_Distance = c(NA, max_truescore_distance, min_distance, knn_distance), 
                             `TPR (repeated)` = c(bl_tpr, max_truescore_tpr, min_distance_tpr, knn_tpr), 
                             FPR = c((1 - bl_tnr), max_truescore_fpr, min_distance_fpr, knn_fpr), 
                             `Best Cutoff` = c(NA, max_truescore_cutoff, min_distance_cutoff, NA))
knitr::kable(assessment_results[1:4, ], caption = "Assessment Results")

#################################################
# Build and Assess a Classification Tree Model
#################################################

# Develop a classification tree with an optimal value for the complexity parameter (cp).

  # Build a complete tree with cp and minsplit set to 0.

train_set3 <- dplyr::select(train_set2, events, launch_angle, launch_speed, 
                            spray_angle_Kolp, spray_angle_adj, if_fielding_alignment, hp_to_1b)

set.seed(1)
ctree_train <- rpart(events ~ ., method = "class", data = train_set3, 
                     control = rpart.control(xval = 10, minbucket = 2, cp = 0))

  # Determine the optimal value of cp.

plotcp(ctree_train)
ctree_train$cptable

    # Find the lowest estimated cross-validated error (xerror), along with 
    # its standard error (xstd). 

(cp_min_xerror_index <- which.min(ctree_train$cptable[,"xerror"]))
(cp_min_xerror <- ctree_train$cptable[cp_min_xerror_index, "xerror"])
(cp_min_xerror_xstd <- ctree_train$cptable[cp_min_xerror_index, "xstd"])

    # Identify the highest CP (the smallest tree) whose xerror is still within one 
    # standard error of the lowest xerror (Breiman's "one standard error rule").

(cp_max_range_xerror <- cp_min_xerror + cp_min_xerror_xstd)
(cp_min_range_xerror <- cp_min_xerror - cp_min_xerror_xstd)
cptable <- as.data.frame(ctree_train$cptable)
cptable1 <- cptable %>% filter(xerror <= cp_max_range_xerror & xerror >= cp_min_range_xerror)
(opt_cp <- max(cptable1[, 1]))

  # Use opt_cp to prune our classification tree.

ctree_pruned_train <- prune(ctree_train, opt_cp)

  # Review and visualize our pruned classification tree object.

print(ctree_pruned_train)
View(ctree_pruned_train)

prp(ctree_pruned_train)  # Once this plot is generated in Rstudio, the text can be made readable
                         # by exporting it and saving it as a pdf file that can be enlarged.

  # Review the Variable Importance computations from the summary() function, scaled to sum to 100.

ctree_pruned_train_var_imp <- tibble(Predictor = c("launch_angle", "launch_speed", "spray_angle_Kolp", 
                                                   "spray_angle_adj", "if_fielding_alignment", "hp_to_1b"), 
                                     `ctree Importance` = c(48, 32, 4, 13, 2, "< 1"))
knitr::kable(ctree_pruned_train_var_imp[1:6, ], caption = "Variable Importance: Pruned Classification Tree")                             
                             
# Use our pruned classification tree to predict outcomes based on our test set predictors.

test_set3 <- dplyr::select(test_set2, events, launch_angle, launch_speed, 
                           spray_angle_Kolp, spray_angle_adj, if_fielding_alignment, hp_to_1b)

ctree_pruned_predict <- predict(ctree_pruned_train, newdata = test_set3[, -1], type = "class")

# Assess the performance of the pruned classification tree.

table(predicted = ctree_pruned_predict, actual = test_set3$events)
(ctree_pruned_tpr <- round(sensitivity(ctree_pruned_predict, reference = factor(test_set3$events)), 6))
(ctree_pruned_tnr <- round(specificity(ctree_pruned_predict, reference = factor(test_set3$events)), 6))
(ctree_pruned_fpr <- round(1 - ctree_pruned_tnr, 6))
(ctree_pruned_truescore <- round((2 * ctree_pruned_tpr * ctree_pruned_tnr) / (ctree_pruned_tpr + ctree_pruned_tnr) ,6))
(ctree_pruned_distance <- round(sqrt((1 - ctree_pruned_tpr)^2 + (ctree_pruned_fpr)^2), 6))     
     
assessment_results <- tibble(Model = c("Baseline", "Logistic Regression", "Logistic Regression", 
                                       "K-Nearest Neighbors", "Classification Tree"), 
                             `Cutoff Method` = c(NA, "Truescore", "ROC Distance to (0,1)", NA, NA),
                             `Truescore` = c(bl_truescore, max_truescore, min_distance_truescore, 
                                             knn_truescore, ctree_pruned_truescore), 
                             TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr, knn_tpr, ctree_pruned_tpr),
                             TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr, knn_tnr, ctree_pruned_tnr), 
                             ROC_Distance = c(NA, max_truescore_distance, min_distance, knn_distance, 
                                              ctree_pruned_distance), 
                             `TPR (repeated)` = c(bl_tpr, max_truescore_tpr, min_distance_tpr, knn_tpr, 
                                                  ctree_pruned_tpr), 
                             FPR = c((1 - bl_tnr), max_truescore_fpr, min_distance_fpr, knn_fpr, ctree_pruned_fpr), 
                             `Best Cutoff` = c(NA, max_truescore_cutoff, min_distance_cutoff, NA, NA))
knitr::kable(assessment_results[1:5, ], caption = "Assessment Results")

#################################################
# Build and Assess a Random Forest Model
#################################################

# Fit a random forest model to our training data. Use the randomForest function in 
# the randomForest package.

library(randomForest)
set.seed(1)
(rforest_train <- randomForest(events ~ ., data = train_set3, importance = TRUE))

# Which variables did the randomForest algorithm consider to be the most important?

varImpPlot(rforest_train, sort = TRUE, type = 2, main = "Importance of Variables")
importance(rforest_train, type = 2)

# Compare variable importance as estimated by our classification tree model and our random forest model.

rforest_train_var_imp <- as_tibble(importance(rforest_train, type = 2))
rforest_train_var_imp <- mutate(rforest_train_var_imp, rforest_imp = 
         round((MeanDecreaseGini / sum(rforest_train_var_imp$MeanDecreaseGini)) * 100))

variable_imp <- tibble(Predictor = c("launch_angle", "launch_speed", "spray_angle_Kolp", 
                                     "spray_angle_adj", "if_fielding_alignment", "hp_to_1b"), 
                       `ctree Importance` = c(48, 32, 4, 13, 2, "< 1"), 
                       `rforest Importance` = rforest_train_var_imp$rforest_imp)

knitr::kable(variable_imp[1:6, ], caption = "Variable Importance by Model")  

# Use the randomForest algorithm to predict "single" or "field_out" based on our test data.

rforest_predict <- predict(rforest_train, newdata = test_set3, type = "response")
rforest_predict_prob <- predict(rforest_train, newdata = test_set3, type = "prob")

# Assess the predictive ability of the randomForest algorithm.

confusionMatrix(rforest_predict, test_set3$events)
(rforest_tpr <- round(sensitivity(rforest_predict, reference = factor(test_set3$events)), 6))
(rforest_tnr <- round(specificity(rforest_predict, reference = factor(test_set3$events)), 6))
(rforest_fpr <- round(1 - rforest_tnr, 6))
(rforest_truescore <- round((2 * rforest_tpr * rforest_tnr) / (rforest_tpr + rforest_tnr) , 6))
(rforest_distance <- round(sqrt((1 - rforest_tpr)^2 + (rforest_fpr)^2), 6))
  
assessment_results <- tibble(Model = c("Baseline", "Logistic Regression", "Logistic Regression", 
                                       "K-Nearest Neighbors", "Classification Tree", "Random Forest"), 
                             `Cutoff Method` = c(NA, "Truescore", "ROC Distance to (0,1)", NA, NA, NA),
                             `Truescore` = c(bl_truescore, max_truescore, min_distance_truescore, 
                                             knn_truescore, ctree_pruned_truescore, rforest_truescore), 
                             TPR = c(bl_tpr, max_truescore_tpr, min_distance_tpr, knn_tpr, 
                                     ctree_pruned_tpr, rforest_tpr),
                             TNR = c(bl_tnr, max_truescore_tnr, min_distance_tnr, knn_tnr, 
                                     ctree_pruned_tnr, rforest_tnr), 
                             ROC_Distance = c(NA, max_truescore_distance, min_distance, knn_distance, 
                                              ctree_pruned_distance, rforest_distance), 
                             `TPR (repeated)` = c(bl_tpr, max_truescore_tpr, min_distance_tpr, knn_tpr, 
                                                  ctree_pruned_tpr, rforest_tpr), 
                             FPR = c((1 - bl_tnr), max_truescore_fpr, min_distance_fpr, knn_fpr, 
                                     ctree_pruned_fpr, rforest_fpr), 
                             `Best Cutoff` = c(NA, max_truescore_cutoff, min_distance_cutoff, NA, NA, NA))
knitr::kable(assessment_results[1:6, ], caption = "Assessment Results")

# Fit a random forest model to our training data. Use the train function in 
# the caret package.

set.seed(200)
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5,
                           returnData = TRUE, returnResamp = "all", savePredictions = "all",
                           summaryFunction = twoClassSummary, classProbs = TRUE)
rforest_train2 <- train(train_set3[, -1], train_set3$events, method = "rf",
                        trControl = fitControl, metric = "ROC")

# Which variables did the rf method (train function) consider to be the most important?

plot(varImp(rforest_train2))

# Use the rf method (train function) to predict "single" or "field_out" based on our test data.

rforest_predict2 <- predict(rforest_train2, newdata = test_set3, type = "raw")
rforest_predict2_prob <- predict(rforest_train2, newdata = test_set3, type = "prob")

# Assess the predictive ability of the rf method (train function).

confusionMatrix(rforest_predict2, test_set3$events)
(rforest2_tpr <- round(sensitivity(rforest_predict2, reference = factor(test_set3$events)), 6))
(rforest2_tnr <- round(specificity(rforest_predict2, reference = factor(test_set3$events)), 6))
(rforest2_fpr <- round(1 - rforest2_tnr, 6))
(rforest2_truescore <- round((2 * rforest2_tpr * rforest2_tnr) / (rforest2_tpr + rforest2_tnr) , 6))
(rforest2_distance <- round(sqrt((1 - rforest2_tpr)^2 + (rforest2_fpr)^2), 6))

# Create a data frame showing all erroneous predictions by our rforest_train2 algorithm, along with 
# their potentially relevant characteristics.

 # Combine all predictions with actual outcomes.

rforest_predict2_results <- bind_cols(enframe(rforest_predict2), as_tibble(rforest_predict2_prob))
rforest_predict2_results <- rforest_predict2_results %>% 
  dplyr::mutate(obs_nmbr = name, prob_single = single, prob_out = field_out, pred = value) %>% 
  dplyr::select(obs_nmbr, prob_single, prob_out, pred)
rforest_predict2_v_actual <- bind_cols(rforest_predict2_results, as_tibble(test_set3$events))
rforest_predict2_v_actual$actual_events <- rforest_predict2_v_actual$value
rforest_predict2_v_actual <- rforest_predict2_v_actual %>% dplyr::select(-value)

 # Identify the mistaken predictions.
rforest_predict2_v_actual <- rforest_predict2_v_actual %>% 
  mutate(eval = ifelse(rforest_predict2_v_actual$pred !=  rforest_predict2_v_actual$actual_events, "mistake", "-"))

 # Quantify the magnitude of each prediction mistake.

rforest_predict2_v_actual <- rforest_predict2_v_actual %>% 
  mutate(prob_diff = abs(prob_single - prob_out))

 # Add additional information about each batted ball event.

rforest_predict2_v_actual <- bind_cols(rforest_predict2_v_actual, test_set[, 1:52])
rforest_predict2_v_actual <- rforest_predict2_v_actual %>% 
  dplyr::select(obs_nmbr, prob_single, prob_out, pred, actual_events,	eval, prob_diff, 
                game_date, batter, player_name, age, stand, position, team, events, 
                des,  bb_type, adv_bb_type, launch_angle, launch_speed, launch_speed_cat, 
                hc_x_Kolp, hc_y_Kolp, spray_angle_Kolp, spray_angle_Kolp_cat, 
                spray_angle_adj, spray_angle_adj_cat, if_fielding_alignment, num_if_alignment, 
                hp_to_1b, hp_to_1b_cat, of_fielding_alignment, hit_location, hit_distance_sc, 
                launch_speed_angle, game_pk, game_year, game_type, home_team, away_team, 
                pitcher, p_throws, description)

# Create a new data frame with just the mistaken predictions.

rforest_predict2_mistakes <- rforest_predict2_v_actual %>% dplyr::filter(eval == "mistake")

# Create a new data frame with just the correct predictions.

rforest_predict2_correct <- rforest_predict2_v_actual %>% dplyr::filter(eval != "mistake")

# Was it more common to mistakenly predict singles or outs?

mean(rforest_predict2_mistakes$pred == "single")
mean(rforest_predict2_mistakes$pred == "field_out")

# Visualize erroneous predictions by rforest_train2 model.

ggplot(rforest_predict2_mistakes, aes(hc_x_Kolp, hc_y_Kolp, color = pred)) + 
  scale_color_manual(values = c("red", "royalblue3")) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 5, height = 5), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 1) + theme_linedraw() + labs(title = "Mistaken Predictions by rforest_train2 Model")

# Visualize erroneous predictions, by type of batted ball.

pred_labels <- c("predicted single (false positives)", "predicted field_out (false negatives)")
names(pred_labels) <- c("single", "field_out")

ggplot(rforest_predict2_mistakes, aes(hc_x_Kolp, hc_y_Kolp)) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 1) + theme_linedraw() + 
  labs(title = "Mistakenly Predicted Singles and Outs, by Type of Batted Ball") + 
  facet_grid(bb_type ~ pred, labeller = labeller(pred = pred_labels))

# Visualize erroneous predictions, by adv_bb_type.

pred_labels <- c("predicted single (false positives)", "predicted field_out (false negatives)")
names(pred_labels) <- c("single", "field_out")

erroneous_predictions_by_adv_bb_type <- ggplot(rforest_predict2_mistakes, aes(hc_x_Kolp, hc_y_Kolp)) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 1) + theme_linedraw() + 
  labs(title = "Mistakenly Predicted Singles and Outs, by adv_bb_type") + 
  facet_grid(adv_bb_type ~ pred, labeller = labeller(pred = pred_labels))

# Visualize correct predictions, by adv_bb_type.

set.seed(1)
rforest_predict2_correct_sample_2533 <- sample_n(rforest_predict2_correct, 2533, replace = FALSE)

pred_labels <- c("single (true positives)", "field_out (true negatives)")
names(pred_labels) <- c("single", "field_out")

correct_predictions_by_adv_bb_type <- ggplot(rforest_predict2_correct_sample_2533, aes(hc_x_Kolp, hc_y_Kolp)) + 
  geom_segment(x = 0, y = 0, xend = 250, yend = 250, color = "dark green", size = 1.3) + 
  geom_segment(x = 0, y = 0, xend = -250, yend = 250, color = "dark green", size = 1.3) + 
  geom_tile(aes(x = 63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = -63.64, y = 63.64, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_tile(aes(x = 0, y = 127.28, width = 15, height = 15), color = "goldenrod", fill = "goldenrod") + 
  geom_point(size = 1) + theme_linedraw() + 
  labs(title = "Correctly Predicted Singles and Outs, by adv_bb_type") + 
  facet_grid(adv_bb_type ~ pred, labeller = labeller(pred = pred_labels))

grid.arrange(erroneous_predictions_by_adv_bb_type, correct_predictions_by_adv_bb_type, ncol = 4)

# Compare, by launch_angle, our population of 2,533 mistaken predictions to a random population of 2,533 
# correct predictions.

rforest_predict2_mistakes <- rforest_predict2_mistakes %>% 
  mutate(eval_type = ifelse(pred == "single", "FalseSingle", "FalseOut"))
rforest_predict2_mistakes <- rforest_predict2_mistakes %>% 
  dplyr::select(1:6, eval_type, everything())

rforest_predict2_correct_sample_2533 <- rforest_predict2_correct_sample_2533 %>% 
  mutate(eval_type = ifelse(pred == "single", "TrueSingle", "TrueOut"))
rforest_predict2_correct_sample_2533 <- rforest_predict2_correct_sample_2533 %>% 
  dplyr::select(1:6, eval_type, everything())

predictions_5066 <- bind_rows(rforest_predict2_mistakes, rforest_predict2_correct_sample_2533)
predictions_5066$eval_type <- factor(predictions_5066$eval_type, levels = c("TrueSingle", "FalseSingle", "TrueOut", "FalseOut"))

truefalse_by_launch_angle <- ggplot(predictions_5066, 
                                     aes(x = eval_type, y = launch_angle, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.8) + 
  scale_y_continuous(breaks = seq(-80, 80, by = 10), 
                     labels = c("-80", "-70", "-60", "-50", "-40", "30",  "20", "10", "0", 
                                "10", "20", "30", "40", "50", "60", "70", "80"), 
                     limits = c(-90, 90)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Predictions, by Launch Angle and Outcome") + 
  theme(legend.position = c(.2, .92))

truefalse_by_launch_angle_data <- layer_data(truefalse_by_launch_angle)

launch_angle_TrueSingle_1quartile <- truefalse_by_launch_angle_data[1, 2]
launch_angle_TrueSingle_median <- truefalse_by_launch_angle_data[1, 3]
launch_angle_TrueSingle_3quartile <- truefalse_by_launch_angle_data[1, 4]
launch_angle_FalseSingle_1quartile <- truefalse_by_launch_angle_data[2, 2]
launch_angle_FalseSingle_median <- truefalse_by_launch_angle_data[2, 3]
launch_angle_FalseSingle_3quartile <- truefalse_by_launch_angle_data[2, 4]
launch_angle_TrueOut_1quartile <- truefalse_by_launch_angle_data[3, 2]
launch_angle_TrueOut_median <- truefalse_by_launch_angle_data[3, 3]
launch_angle_TrueOut_3quartile <- truefalse_by_launch_angle_data[3, 4]
launch_angle_FalseOut_1quartile <- truefalse_by_launch_angle_data[4, 2]
launch_angle_FalseOut_median <- truefalse_by_launch_angle_data[4, 3]
launch_angle_FalseOut_3quartile <- truefalse_by_launch_angle_data[4, 4]

(truefalse_by_launch_angle <- ggplot(predictions_5066, 
                                    aes(x = eval_type, y = launch_angle, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.8) + 
  scale_y_continuous(breaks = seq(-80, 80, by = 10), 
                     labels = c("-80", "-70", "-60", "-50", "-40", "30",  "20", "10", "0", 
                                "10", "20", "30", "40", "50", "60", "70", "80"), 
                     limits = c(-90, 90)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Predictions, by Launch Angle and Outcome") + 
  theme(legend.position = c(.2, .92)) + 
  annotate("text", x = c(.58, .58, .58), size = 3, fontface = 2, hjust = 1, 
           y = c(launch_angle_TrueSingle_1quartile, launch_angle_TrueSingle_median, launch_angle_TrueSingle_3quartile), 
           label = c(launch_angle_TrueSingle_1quartile, launch_angle_TrueSingle_median, launch_angle_TrueSingle_3quartile)) + 
  annotate("text", x = c(1.58,1.58, 1.58), size = 3, fontface = 2, hjust = 1, 
           y = c(launch_angle_FalseSingle_1quartile, launch_angle_FalseSingle_median, launch_angle_FalseSingle_3quartile),
           label = c(launch_angle_FalseSingle_1quartile, launch_angle_FalseSingle_median, launch_angle_FalseSingle_3quartile)) + 
  annotate("text", x = c(2.58, 2.58, 2.58), size = 3, fontface = 2, hjust = 1, 
           y = c(launch_angle_TrueOut_1quartile, launch_angle_TrueOut_median, launch_angle_TrueOut_3quartile), 
           label = c(launch_angle_TrueOut_1quartile, launch_angle_TrueOut_median, launch_angle_TrueOut_3quartile)) + 
  annotate("text", x = c(3.58, 3.58, 3.58), size = 3, fontface = 2, hjust = 1, 
           y = c(launch_angle_FalseOut_1quartile, launch_angle_FalseOut_median, launch_angle_FalseOut_3quartile),
           label = c(launch_angle_FalseOut_1quartile, launch_angle_FalseOut_median, launch_angle_FalseOut_3quartile)))

# Analyze the worst prediction mistakes (1,106 outcomes with a predicted probability of 30% or less).

rforest_predict2_worstMistakes <- rforest_predict2_mistakes %>% dplyr::filter(prob_diff >= 0.4)

rforest_predict2_worstMistakes <- rforest_predict2_worstMistakes %>% 
  mutate(eval_type = ifelse(pred == "single", "FalseSingle", "FalseOut"))
rforest_predict2_worstMistakes <- rforest_predict2_worstMistakes %>% 
  dplyr::select(1:6, eval_type, everything())

set.seed(1)
rforest_predict2_correct_sample_1106 <- sample_n(rforest_predict2_correct, 1106, replace = FALSE)

rforest_predict2_correct_sample_1106 <- rforest_predict2_correct_sample_1106 %>% 
  mutate(eval_type = ifelse(pred == "single", "TrueSingle", "TrueOut"))
rforest_predict2_correct_sample_1106 <- rforest_predict2_correct_sample_1106 %>% 
  dplyr::select(1:6, eval_type, everything())

predictions_2212 <- bind_rows(rforest_predict2_worstMistakes, rforest_predict2_correct_sample_1106)
predictions_2212$eval_type <- factor(predictions_2212$eval_type, levels = c("TrueSingle", "FalseSingle", "TrueOut", "FalseOut"))

# Compare, by launch_angle, our population of the worst 1,106 prediction mistakes to a random population of 1,106 
# correct predictions.

truefalse_by_launch_angle <- ggplot(predictions_2212, 
                                    aes(x = eval_type, y = launch_angle, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.8) + 
  scale_y_continuous(breaks = seq(-80, 80, by = 10), 
                     labels = c("-80", "-70", "-60", "-50", "-40", "30",  "20", "10", "0", 
                                "10", "20", "30", "40", "50", "60", "70", "80"), 
                     limits = c(-90, 90)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Predictions, by Launch Angle and Outcome") + 
  theme(legend.position = c(.2, .92))

truefalse_by_launch_angle_data <- layer_data(truefalse_by_launch_angle)

launch_angle_TrueSingle_1quartile <- round(truefalse_by_launch_angle_data[1, 2], 1)
launch_angle_TrueSingle_median <- round(truefalse_by_launch_angle_data[1, 3], 1)
launch_angle_TrueSingle_3quartile <- round(truefalse_by_launch_angle_data[1, 4], 1)
launch_angle_FalseSingle_1quartile <- round(truefalse_by_launch_angle_data[2, 2], 1)
launch_angle_FalseSingle_median <- round(truefalse_by_launch_angle_data[2, 3], 1)
launch_angle_FalseSingle_3quartile <- round(truefalse_by_launch_angle_data[2, 4], 1)
launch_angle_TrueOut_1quartile <- round(truefalse_by_launch_angle_data[3, 2], 1)
launch_angle_TrueOut_median <- round(truefalse_by_launch_angle_data[3, 3], 1)
launch_angle_TrueOut_3quartile <- round(truefalse_by_launch_angle_data[3, 4], 1)
launch_angle_FalseOut_1quartile <- round(truefalse_by_launch_angle_data[4, 2], 1)
launch_angle_FalseOut_median <- round(truefalse_by_launch_angle_data[4, 3], 1)
launch_angle_FalseOut_3quartile <- round(truefalse_by_launch_angle_data[4, 4], 1)

(truefalse_by_launch_angle <- ggplot(predictions_2212, 
                                     aes(x = eval_type, y = launch_angle, color = events)) + 
    geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
    geom_point(position = "jitter", alpha = .7, size = 0.8) + 
    scale_y_continuous(breaks = seq(-80, 80, by = 10), 
                       labels = c("-80", "-70", "-60", "-50", "-40", "30",  "20", "10", "0", 
                                  "10", "20", "30", "40", "50", "60", "70", "80"), 
                       limits = c(-90, 90)) + 
    scale_color_manual(values = c("Red", "Blue")) + 
    ggtitle("True vs False Predictions, by Launch Angle and Outcome") + 
    theme(legend.position = c(.2, .92)) + 
    annotate("text", x = c(.58, .58, .58), size = 3, fontface = 2, hjust = 1, 
             y = c(launch_angle_TrueSingle_1quartile, launch_angle_TrueSingle_median, launch_angle_TrueSingle_3quartile), 
             label = c(launch_angle_TrueSingle_1quartile, launch_angle_TrueSingle_median, launch_angle_TrueSingle_3quartile)) + 
    annotate("text", x = c(1.58,1.58, 1.58), size = 3, fontface = 2, hjust = 1, 
             y = c(launch_angle_FalseSingle_1quartile, launch_angle_FalseSingle_median, launch_angle_FalseSingle_3quartile),
             label = c(launch_angle_FalseSingle_1quartile, launch_angle_FalseSingle_median, launch_angle_FalseSingle_3quartile)) + 
    annotate("text", x = c(2.58, 2.58, 2.58), size = 3, fontface = 2, hjust = 1, 
             y = c(launch_angle_TrueOut_1quartile, launch_angle_TrueOut_median, launch_angle_TrueOut_3quartile), 
             label = c(launch_angle_TrueOut_1quartile, launch_angle_TrueOut_median, launch_angle_TrueOut_3quartile)) + 
    annotate("text", x = c(3.58, 3.58, 3.58), size = 3, fontface = 2, hjust = 1, 
             y = c(launch_angle_FalseOut_1quartile, launch_angle_FalseOut_median, launch_angle_FalseOut_3quartile),
             label = c(launch_angle_FalseOut_1quartile, launch_angle_FalseOut_median, launch_angle_FalseOut_3quartile)))

# Compare the densities of launch_angle, using our population of the worst 1,106 prediction mistakes 
# and a random population of 1,106 correct predictions.

x_scale <- scale_x_continuous(breaks = seq(-80, 80, by = 10), 
                              labels = c("-80", "-70", "-60", "-50", "-40", "-30",  "-20", "-10", "0", 
                                         "10", "20", "30", "40", "50", "60", "70", "80"), 
                              limits = c(-90, 90))
y_scale <- scale_y_continuous(limits = c(0, 0.045))
(la_density <- ggplot(dplyr::filter(predictions_2212, eval_type == "TrueOut" | eval_type == "FalseOut"), 
                      aes(x = launch_angle, y = stat(density), color = eval_type)) + 
    geom_freqpoly(binwidth = 2) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(FalseOut = "red4", TrueOut = "royalblue3")) + 
    labs(title = "Density Plots of Launch Angle by TrueOut and FalseOut", 
         x = "Launch Angle (degrees)")) + 
  theme(legend.position = c(.15, .8))

# Compare, by launch_speed, our population of 2,533 mistaken predictions to a random population of 2,533 
# correct predictions. 

truefalse_by_launch_speed <- ggplot(predictions_5066, 
                                    aes(x = eval_type, y = launch_speed, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.8) + 
  scale_y_continuous(breaks = seq(25, 115, by = 10), 
                     labels = c("25", "35", "45", "55", "65", "75",  "85", "95", "105", "115"), 
                     limits = c(25, 115)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Predictions, by Launch Speed and Outcome") + 
  theme(legend.position = c(.5, .04), legend.direction = "horizontal")

truefalse_by_launch_speed_data <- layer_data(truefalse_by_launch_speed)

launch_speed_TrueSingle_1quartile <- truefalse_by_launch_speed_data[1, 2]
launch_speed_TrueSingle_median <- truefalse_by_launch_speed_data[1, 3]
launch_speed_TrueSingle_3quartile <- truefalse_by_launch_speed_data[1, 4]
launch_speed_FalseSingle_1quartile <- truefalse_by_launch_speed_data[2, 2]
launch_speed_FalseSingle_median <- truefalse_by_launch_speed_data[2, 3]
launch_speed_FalseSingle_3quartile <- truefalse_by_launch_speed_data[2, 4]
launch_speed_TrueOut_1quartile <- truefalse_by_launch_speed_data[3, 2]
launch_speed_TrueOut_median <- truefalse_by_launch_speed_data[3, 3]
launch_speed_TrueOut_3quartile <- truefalse_by_launch_speed_data[3, 4]
launch_speed_FalseOut_1quartile <- truefalse_by_launch_speed_data[4, 2]
launch_speed_FalseOut_median <- truefalse_by_launch_speed_data[4, 3]
launch_speed_FalseOut_3quartile <- truefalse_by_launch_speed_data[4, 4]

(truefalse_by_launch_speed <- ggplot(predictions_5066, 
                                     aes(x = eval_type, y = launch_speed, color = events)) + 
    geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
    geom_point(position = "jitter", alpha = .7, size = 0.8) + 
    scale_y_continuous(breaks = seq(25, 115, by = 10), 
                       labels = c("25", "35", "45", "55", "65", "75",  "85", "95", "105", "115"), 
                       limits = c(25, 115)) + 
    scale_color_manual(values = c("Red", "Blue")) + 
    ggtitle("True vs False Predictions, by Launch Speed and Outcome") + 
    theme(legend.position = c(.5, .04), legend.direction = "horizontal") + 
    annotate("text", x = c(.59, .59, .59), size = 2.5, fontface = 2, hjust = 1, 
             y = c(launch_speed_TrueSingle_1quartile, launch_speed_TrueSingle_median, launch_speed_TrueSingle_3quartile), 
             label = c(launch_speed_TrueSingle_1quartile, launch_speed_TrueSingle_median, launch_speed_TrueSingle_3quartile)) + 
    annotate("text", x = c(1.59,1.59, 1.59), size = 2.5, fontface = 2, hjust = 1, 
             y = c(launch_speed_FalseSingle_1quartile, launch_speed_FalseSingle_median, launch_speed_FalseSingle_3quartile),
             label = c(launch_speed_FalseSingle_1quartile, launch_speed_FalseSingle_median, launch_speed_FalseSingle_3quartile)) + 
    annotate("text", x = c(2.59, 2.59, 2.59), size = 2.5, fontface = 2, hjust = 1, 
             y = c(launch_speed_TrueOut_1quartile, launch_speed_TrueOut_median, launch_speed_TrueOut_3quartile), 
             label = c(launch_speed_TrueOut_1quartile, launch_speed_TrueOut_median, launch_speed_TrueOut_3quartile)) + 
    annotate("text", x = c(3.59, 3.59, 3.59), size = 2.5, fontface = 2, hjust = 1, 
             y = c(launch_speed_FalseOut_1quartile, launch_speed_FalseOut_median, launch_speed_FalseOut_3quartile),
             label = c(launch_speed_FalseOut_1quartile, launch_speed_FalseOut_median, launch_speed_FalseOut_3quartile)))

# Compare, by launch_speed, our population of the worst 1,106 predictions to a random population of 1,106 
# correct predictions. 

truefalse_by_launch_speed <- ggplot(predictions_2212, 
                                    aes(x = eval_type, y = launch_speed, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.8) + 
  scale_y_continuous(breaks = seq(25, 115, by = 10), 
                     labels = c("25", "35", "45", "55", "65", "75",  "85", "95", "105", "115"), 
                     limits = c(25, 115)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Predictions, by Launch Speed and Outcome") + 
  theme(legend.position = c(.5, .04), legend.direction = "horizontal")

truefalse_by_launch_speed_data <- layer_data(truefalse_by_launch_speed)

launch_speed_TrueSingle_1quartile <- round(truefalse_by_launch_speed_data[1, 2], 1)
launch_speed_TrueSingle_median <- round(truefalse_by_launch_speed_data[1, 3], 1)
launch_speed_TrueSingle_3quartile <- round(truefalse_by_launch_speed_data[1, 4], 1)
launch_speed_FalseSingle_1quartile <- round(truefalse_by_launch_speed_data[2, 2], 1)
launch_speed_FalseSingle_median <- round(truefalse_by_launch_speed_data[2, 3], 1)
launch_speed_FalseSingle_3quartile <- round(truefalse_by_launch_speed_data[2, 4], 1)
launch_speed_TrueOut_1quartile <- round(truefalse_by_launch_speed_data[3, 2], 1)
launch_speed_TrueOut_median <- round(truefalse_by_launch_speed_data[3, 3], 1)
launch_speed_TrueOut_3quartile <- round(truefalse_by_launch_speed_data[3, 4], 1)
launch_speed_FalseOut_1quartile <- round(truefalse_by_launch_speed_data[4, 2], 1)
launch_speed_FalseOut_median <- round(truefalse_by_launch_speed_data[4, 3], 1)
launch_speed_FalseOut_3quartile <- round(truefalse_by_launch_speed_data[4, 4], 1)

(truefalse_by_launch_speed <- ggplot(predictions_2212, 
                                     aes(x = eval_type, y = launch_speed, color = events)) + 
    geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
    geom_point(position = "jitter", alpha = .7, size = 0.8) + 
    scale_y_continuous(breaks = seq(25, 115, by = 10), 
                       labels = c("25", "35", "45", "55", "65", "75",  "85", "95", "105", "115"), 
                       limits = c(25, 115)) + 
    scale_color_manual(values = c("Red", "Blue")) + 
    ggtitle("True vs False Predictions, by Launch Speed and Outcome") + 
    theme(legend.position = c(.5, .04), legend.direction = "horizontal") + 
    annotate("text", x = c(.59, .59, .59), size = 2.5, fontface = 2, hjust = 1, 
             y = c(launch_speed_TrueSingle_1quartile, launch_speed_TrueSingle_median, launch_speed_TrueSingle_3quartile), 
             label = c(launch_speed_TrueSingle_1quartile, launch_speed_TrueSingle_median, launch_speed_TrueSingle_3quartile)) + 
    annotate("text", x = c(1.59,1.59, 1.59), size = 2.5, fontface = 2, hjust = 1, 
             y = c(launch_speed_FalseSingle_1quartile, launch_speed_FalseSingle_median, launch_speed_FalseSingle_3quartile),
             label = c(launch_speed_FalseSingle_1quartile, launch_speed_FalseSingle_median, launch_speed_FalseSingle_3quartile)) + 
    annotate("text", x = c(2.59, 2.59, 2.59), size = 2.5, fontface = 2, hjust = 1, 
             y = c(launch_speed_TrueOut_1quartile, launch_speed_TrueOut_median, launch_speed_TrueOut_3quartile), 
             label = c(launch_speed_TrueOut_1quartile, launch_speed_TrueOut_median, launch_speed_TrueOut_3quartile)) + 
    annotate("text", x = c(3.59, 3.59, 3.59), size = 2.5, fontface = 2, hjust = 1, 
             y = c(launch_speed_FalseOut_1quartile, launch_speed_FalseOut_median, launch_speed_FalseOut_3quartile),
             label = c(launch_speed_FalseOut_1quartile, launch_speed_FalseOut_median, launch_speed_FalseOut_3quartile)))

# Compare the densities of launch_speed, using our population of the worst 1,106 prediction mistakes 
# and a random population of 1,106 correct predictions.

x_scale <- scale_x_continuous(breaks = seq(25, 115, by = 10), 
                              labels = c("25", "35", "45", "55", "65", "75",  "85", "95", "105", "115"), 
                              limits = c(25, 115))
y_scale <- scale_y_continuous(limits = c(0, 0.04))
(ls_density <- ggplot(dplyr::filter(predictions_2212, eval_type == "TrueOut" | eval_type == "FalseOut"), 
                      aes(x = launch_speed, y = stat(density), color = eval_type)) + 
    geom_freqpoly(binwidth = 2.5) + 
    x_scale + 
    y_scale + 
    scale_color_manual(values = c(FalseOut = "red4", TrueOut = "royalblue3")) + 
    labs(title = "Density Plots of Exit Velocity by TrueOut and FalseOut", 
         x = "Exit Velocity (mph)")) + 
  theme(legend.position = c(.15, .8))

# Compare, by spray_angle_Kolp, our population of 2,533 mistaken predictions to a random population of 2,533 
# correct predictions.

predictions_5066_std_gb <- 
  dplyr::filter(predictions_5066, bb_type == "ground_ball" & if_fielding_alignment == "Standard")
predictions_5066_std_gb <- predictions_5066_std_gb %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball" | adv_bb_type == "high_ground_ball")

truefalse_by_spray_angle_Kolp <- ggplot(predictions_5066_std_gb, 
                                        aes(x = eval_type, y = spray_angle_Kolp, color = events)) + 
    geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
    geom_point(position = "jitter", alpha = .7, size = 0.85) + 
    scale_y_continuous(breaks = seq(-50, 50, by = 5), 
                       labels = c("-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5",  "0", 
                                  "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"), 
                       limits = c(-50, 50)) + 
    scale_color_manual(values = c("Red", "Blue")) + 
    ggtitle("True vs False Predictions, by spray_angle_Kolp and Outcome (Inf Standard)") +
    theme(legend.position = c(.5, .96), legend.direction = "horizontal")

truefalse_by_spray_angle_Kolp_data <- layer_data(truefalse_by_spray_angle_Kolp)

spray_angle_Kolp_TrueSingle_1quartile <- round(truefalse_by_spray_angle_Kolp_data[1, 2], 1)
spray_angle_Kolp_TrueSingle_median <- round(truefalse_by_spray_angle_Kolp_data[1, 3], 1)
spray_angle_Kolp_TrueSingle_3quartile <- round(truefalse_by_spray_angle_Kolp_data[1, 4], 1)
spray_angle_Kolp_FalseSingle_1quartile <- round(truefalse_by_spray_angle_Kolp_data[2, 2], 1)
spray_angle_Kolp_FalseSingle_median <- round(truefalse_by_spray_angle_Kolp_data[2, 3], 1)
spray_angle_Kolp_FalseSingle_3quartile <- round(truefalse_by_spray_angle_Kolp_data[2, 4], 1)
spray_angle_Kolp_TrueOut_1quartile <- round(truefalse_by_spray_angle_Kolp_data[3, 2], 1)
spray_angle_Kolp_TrueOut_median <- round(truefalse_by_spray_angle_Kolp_data[3, 3], 1)
spray_angle_Kolp_TrueOut_3quartile <- round(truefalse_by_spray_angle_Kolp_data[3, 4], 1)
spray_angle_Kolp_FalseOut_1quartile <- round(truefalse_by_spray_angle_Kolp_data[4, 2], 1)
spray_angle_Kolp_FalseOut_median <- round(truefalse_by_spray_angle_Kolp_data[4, 3], 1)
spray_angle_Kolp_FalseOut_3quartile <- round(truefalse_by_spray_angle_Kolp_data[4, 4], 1)

(truefalse_by_spray_angle_Kolp <- ggplot(predictions_5066_std_gb, 
                                         aes(x = eval_type, y = spray_angle_Kolp, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.85) + 
  scale_y_continuous(breaks = seq(-50, 50, by = 5), 
                     labels = c("-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5",  "0", 
                                "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Predictions, by spray_angle_Kolp and Outcome (Inf Standard)") +
  theme(legend.position = c(.2, .96), legend.direction = "horizontal") + 
  annotate("text", x = c(.60, .60, .60), size = 3, fontface = 2, hjust = 1, 
           y = c(spray_angle_Kolp_TrueSingle_1quartile, spray_angle_Kolp_TrueSingle_median, spray_angle_Kolp_TrueSingle_3quartile), 
           label = c(spray_angle_Kolp_TrueSingle_1quartile, spray_angle_Kolp_TrueSingle_median, spray_angle_Kolp_TrueSingle_3quartile)) + 
  annotate("text", x = c(1.60,1.60, 1.60), size = 3, fontface = 2, hjust = 1, 
           y = c(spray_angle_Kolp_FalseSingle_1quartile, spray_angle_Kolp_FalseSingle_median, spray_angle_Kolp_FalseSingle_3quartile),
           label = c(spray_angle_Kolp_FalseSingle_1quartile, spray_angle_Kolp_FalseSingle_median, spray_angle_Kolp_FalseSingle_3quartile)) + 
  annotate("text", x = c(2.60, 2.60, 2.60), size = 3, fontface = 2, hjust = 1, 
           y = c(spray_angle_Kolp_TrueOut_1quartile, spray_angle_Kolp_TrueOut_median, spray_angle_Kolp_TrueOut_3quartile), 
           label = c(spray_angle_Kolp_TrueOut_1quartile, spray_angle_Kolp_TrueOut_median, spray_angle_Kolp_TrueOut_3quartile)) + 
  annotate("text", x = c(3.60, 3.60, 3.60), size = 3, fontface = 2, hjust = 1, 
           y = c(spray_angle_Kolp_FalseOut_1quartile, spray_angle_Kolp_FalseOut_median, spray_angle_Kolp_FalseOut_3quartile),
           label = c(spray_angle_Kolp_FalseOut_1quartile, spray_angle_Kolp_FalseOut_median, spray_angle_Kolp_FalseOut_3quartile)))

# Compare, by spray_angle_Kolp, our population of 1,106 mistaken predictions to a random population of 1,106  
# correct predictions.

predictions_2212_std_gb <- 
  dplyr::filter(predictions_2212, bb_type == "ground_ball" & if_fielding_alignment == "Standard")
predictions_2212_std_gb <- predictions_2212_std_gb %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball" | adv_bb_type == "high_ground_ball")


truefalse_by_spray_angle_Kolp <- ggplot(predictions_2212_std_gb, 
                                        aes(x = eval_type, y = spray_angle_Kolp, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.85) + 
  scale_y_continuous(breaks = seq(-50, 50, by = 5), 
                     labels = c("-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5",  "0", 
                                "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Predictions, by spray_angle_Kolp and Outcome (Inf Standard)") +
  theme(legend.position = c(.5, .96), legend.direction = "horizontal")

truefalse_by_spray_angle_Kolp_data <- layer_data(truefalse_by_spray_angle_Kolp)

spray_angle_Kolp_TrueSingle_1quartile <- round(truefalse_by_spray_angle_Kolp_data[1, 2], 1)
spray_angle_Kolp_TrueSingle_median <- round(truefalse_by_spray_angle_Kolp_data[1, 3], 1)
spray_angle_Kolp_TrueSingle_3quartile <- round(truefalse_by_spray_angle_Kolp_data[1, 4], 1)
spray_angle_Kolp_FalseSingle_1quartile <- round(truefalse_by_spray_angle_Kolp_data[2, 2], 1)
spray_angle_Kolp_FalseSingle_median <- round(truefalse_by_spray_angle_Kolp_data[2, 3], 1)
spray_angle_Kolp_FalseSingle_3quartile <- round(truefalse_by_spray_angle_Kolp_data[2, 4], 1)
spray_angle_Kolp_TrueOut_1quartile <- round(truefalse_by_spray_angle_Kolp_data[3, 2], 1)
spray_angle_Kolp_TrueOut_median <- round(truefalse_by_spray_angle_Kolp_data[3, 3], 1)
spray_angle_Kolp_TrueOut_3quartile <- round(truefalse_by_spray_angle_Kolp_data[3, 4], 1)
spray_angle_Kolp_FalseOut_1quartile <- round(truefalse_by_spray_angle_Kolp_data[4, 2], 1)
spray_angle_Kolp_FalseOut_median <- round(truefalse_by_spray_angle_Kolp_data[4, 3], 1)
spray_angle_Kolp_FalseOut_3quartile <- round(truefalse_by_spray_angle_Kolp_data[4, 4], 1)

(truefalse_by_spray_angle_Kolp <- ggplot(predictions_2212_std_gb, 
                                         aes(x = eval_type, y = spray_angle_Kolp, color = events)) + 
    geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
    geom_point(position = "jitter", alpha = .7, size = 0.85) + 
    scale_y_continuous(breaks = seq(-50, 50, by = 5), 
                       labels = c("-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5",  "0", 
                                  "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"), 
                       limits = c(-50, 50)) + 
    scale_color_manual(values = c("Red", "Blue")) + 
    ggtitle("True vs False Predictions, by spray_angle_Kolp and Outcome (Inf Standard)") +
    theme(legend.position = c(.2, .96), legend.direction = "horizontal") + 
    annotate("text", x = c(.60, .60, .60), size = 3, fontface = 2, hjust = 1, 
             y = c(spray_angle_Kolp_TrueSingle_1quartile, spray_angle_Kolp_TrueSingle_median, spray_angle_Kolp_TrueSingle_3quartile), 
             label = c(spray_angle_Kolp_TrueSingle_1quartile, spray_angle_Kolp_TrueSingle_median, spray_angle_Kolp_TrueSingle_3quartile)) + 
    annotate("text", x = c(1.60,1.60, 1.60), size = 3, fontface = 2, hjust = 1, 
             y = c(spray_angle_Kolp_FalseSingle_1quartile, spray_angle_Kolp_FalseSingle_median, spray_angle_Kolp_FalseSingle_3quartile),
             label = c(spray_angle_Kolp_FalseSingle_1quartile, spray_angle_Kolp_FalseSingle_median, spray_angle_Kolp_FalseSingle_3quartile)) + 
    annotate("text", x = c(2.60, 2.60, 2.60), size = 3, fontface = 2, hjust = 1, 
             y = c(spray_angle_Kolp_TrueOut_1quartile, spray_angle_Kolp_TrueOut_median, spray_angle_Kolp_TrueOut_3quartile), 
             label = c(spray_angle_Kolp_TrueOut_1quartile, spray_angle_Kolp_TrueOut_median, spray_angle_Kolp_TrueOut_3quartile)) + 
    annotate("text", x = c(3.60, 3.60, 3.60), size = 3, fontface = 2, hjust = 1, 
             y = c(spray_angle_Kolp_FalseOut_1quartile, spray_angle_Kolp_FalseOut_median, spray_angle_Kolp_FalseOut_3quartile),
             label = c(spray_angle_Kolp_FalseOut_1quartile, spray_angle_Kolp_FalseOut_median, spray_angle_Kolp_FalseOut_3quartile)))

# Compare the densities of spray_angle_Kolp (Standard If Alignment), using our population of the worst 
# 1,106 prediction mistakes and a random population of 1,106 correct predictions.

predictions_2212_std_gb_TFOuts <- 
  dplyr::filter(predictions_2212, bb_type == "ground_ball" & if_fielding_alignment == "Standard")
predictions_2212_std_gb_TFOuts <- predictions_2212_std_gb_TFOuts %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball" | adv_bb_type == "high_ground_ball")
predictions_2212_std_gb_TFOuts <- predictions_2212_std_gb_TFOuts %>% 
  dplyr::filter(eval_type == "TrueOut" | eval_type == "FalseOut")

x_scale <- scale_x_continuous(breaks = seq(-50, 50, by = 5), 
                              labels = c("-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5",  "0", 
                                         "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"), 
                              limits = c(-50, 50))
y_scale <- scale_y_continuous(limits = c(0, 0.035))

(sa_Kolp_density <- ggplot(predictions_2212_std_gb_TFOuts, 
                           aes(x = spray_angle_Kolp, y = stat(density), color = eval_type)) + 
    geom_freqpoly(binwidth = 2.5) + 
    x_scale + 
    y_scale + 
    scale_color_manual(name = "eval_type", labels = c("TrueOut", "FalseOut"), values = c("royalblue3", "red4")) + 
    labs(title = "Density Plots of spray_angle_Kolp by TrueOut and FalseOut 
         (Standard If Alignment)", x = "Spray Angle Kolp (degrees)")) + 
  theme(legend.position = c(.15, .80))

# Compare, by spray_angle_adj, our population of 2,533 mistaken predictions to a random population of 2,533 
# correct predictions.

predictions_5066_shift_gb <- 
  dplyr::filter(predictions_5066, bb_type == "ground_ball" & if_fielding_alignment == "Infield shift")
predictions_5066_shift_gb <- predictions_5066_shift_gb %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball" | adv_bb_type == "high_ground_ball")

truefalse_by_spray_angle_adj <- ggplot(predictions_5066_shift_gb, 
                                       aes(x = eval_type, y = spray_angle_adj, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.85) + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Predictions, by spray_angle_adj and Outcome (Inf Shifted)") +
  theme(legend.position = c(.5, .96), legend.direction = "horizontal")

truefalse_by_spray_angle_adj_data <- layer_data(truefalse_by_spray_angle_adj)

spray_angle_adj_TrueSingle_1quartile <- round(truefalse_by_spray_angle_adj_data[1, 2], 1)
spray_angle_adj_TrueSingle_median <- round(truefalse_by_spray_angle_adj_data[1, 3], 1)
spray_angle_adj_TrueSingle_3quartile <- round(truefalse_by_spray_angle_adj_data[1, 4], 1)
spray_angle_adj_FalseSingle_1quartile <- round(truefalse_by_spray_angle_adj_data[2, 2], 1)
spray_angle_adj_FalseSingle_median <- round(truefalse_by_spray_angle_adj_data[2, 3], 1)
spray_angle_adj_FalseSingle_3quartile <- round(truefalse_by_spray_angle_adj_data[2, 4], 1)
spray_angle_adj_TrueOut_1quartile <- round(truefalse_by_spray_angle_adj_data[3, 2], 1)
spray_angle_adj_TrueOut_median <- round(truefalse_by_spray_angle_adj_data[3, 3], 1)
spray_angle_adj_TrueOut_3quartile <- round(truefalse_by_spray_angle_adj_data[3, 4], 1)
spray_angle_adj_FalseOut_1quartile <- round(truefalse_by_spray_angle_adj_data[4, 2], 1)
spray_angle_adj_FalseOut_median <- round(truefalse_by_spray_angle_adj_data[4, 3], 1)
spray_angle_adj_FalseOut_3quartile <- round(truefalse_by_spray_angle_adj_data[4, 4], 1)

(truefalse_by_spray_angle_adj <- ggplot(predictions_5066_shift_gb, 
                                        aes(x = eval_type, y = spray_angle_adj, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.85) + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Predictions, by spray_angle_adj and Outcome (Inf Shifted)") +
  theme(legend.position = c(.5, .96), legend.direction = "horizontal") + 
  annotate("text", x = c(.60, .60, .60), size = 3, fontface = 2, hjust = 1, 
           y = c(spray_angle_adj_TrueSingle_1quartile, spray_angle_adj_TrueSingle_median, spray_angle_adj_TrueSingle_3quartile), 
           label = c(spray_angle_adj_TrueSingle_1quartile, spray_angle_adj_TrueSingle_median, spray_angle_adj_TrueSingle_3quartile)) + 
  annotate("text", x = c(1.60,1.60, 1.60), size = 3, fontface = 2, hjust = 1, 
           y = c(spray_angle_adj_FalseSingle_1quartile, spray_angle_adj_FalseSingle_median, spray_angle_adj_FalseSingle_3quartile),
           label = c(spray_angle_adj_FalseSingle_1quartile, spray_angle_adj_FalseSingle_median, spray_angle_adj_FalseSingle_3quartile)) + 
  annotate("text", x = c(2.60, 2.60, 2.60), size = 3, fontface = 2, hjust = 1, 
           y = c(spray_angle_adj_TrueOut_1quartile, spray_angle_adj_TrueOut_median, spray_angle_adj_TrueOut_3quartile), 
           label = c(spray_angle_adj_TrueOut_1quartile, spray_angle_adj_TrueOut_median, spray_angle_adj_TrueOut_3quartile)) + 
  annotate("text", x = c(3.60, 3.60, 3.60), size = 3, fontface = 2, hjust = 1, 
           y = c(spray_angle_adj_FalseOut_1quartile, spray_angle_adj_FalseOut_median, spray_angle_adj_FalseOut_3quartile),
           label = c(spray_angle_adj_FalseOut_1quartile, spray_angle_adj_FalseOut_median, spray_angle_adj_FalseOut_3quartile)))

# Compare, by spray_angle_adj, our population of 1,106 mistaken predictions to a random population of 1,106 
# correct predictions.

predictions_2212_shift_gb <- 
  dplyr::filter(predictions_2212, bb_type == "ground_ball" & if_fielding_alignment == "Infield shift")
predictions_2212_shift_gb <- predictions_2212_shift_gb %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball" | adv_bb_type == "high_ground_ball")


truefalse_by_spray_angle_adj <- ggplot(predictions_2212_shift_gb, 
                                       aes(x = eval_type, y = spray_angle_adj, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.85) + 
  scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                     labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", "40",  "50"), 
                     limits = c(-50, 50)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Predictions, by spray_angle_adj and Outcome (Inf Shifted)") +
  theme(legend.position = c(.5, .96), legend.direction = "horizontal")

truefalse_by_spray_angle_adj_data <- layer_data(truefalse_by_spray_angle_adj)

spray_angle_adj_TrueSingle_1quartile <- round(truefalse_by_spray_angle_adj_data[1, 2], 1)
spray_angle_adj_TrueSingle_median <- round(truefalse_by_spray_angle_adj_data[1, 3], 1)
spray_angle_adj_TrueSingle_3quartile <- round(truefalse_by_spray_angle_adj_data[1, 4], 1)
spray_angle_adj_FalseSingle_1quartile <- round(truefalse_by_spray_angle_adj_data[2, 2], 1)
spray_angle_adj_FalseSingle_median <- round(truefalse_by_spray_angle_adj_data[2, 3], 1)
spray_angle_adj_FalseSingle_3quartile <- round(truefalse_by_spray_angle_adj_data[2, 4], 1)
spray_angle_adj_TrueOut_1quartile <- round(truefalse_by_spray_angle_adj_data[3, 2], 1)
spray_angle_adj_TrueOut_median <- round(truefalse_by_spray_angle_adj_data[3, 3], 1)
spray_angle_adj_TrueOut_3quartile <- round(truefalse_by_spray_angle_adj_data[3, 4], 1)
spray_angle_adj_FalseOut_1quartile <- round(truefalse_by_spray_angle_adj_data[4, 2], 1)
spray_angle_adj_FalseOut_median <- round(truefalse_by_spray_angle_adj_data[4, 3], 1)
spray_angle_adj_FalseOut_3quartile <- round(truefalse_by_spray_angle_adj_data[4, 4], 1)

(truefalse_by_spray_angle_adj <- ggplot(predictions_2212_shift_gb, 
                                        aes(x = eval_type, y = spray_angle_adj, color = events)) + 
    geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
    geom_point(position = "jitter", alpha = .7, size = 0.85) + 
    scale_y_continuous(breaks = seq(-50, 50, by = 10), 
                       labels = c("-50", "-40", "-30", "-20", "-10", "0", "10", "20", "30", "40",  "50"), 
                       limits = c(-50, 50)) + 
    scale_color_manual(values = c("Red", "Blue")) + 
    ggtitle("True vs False Predictions, by spray_angle_adj and Outcome (Inf Shifted)") +
    theme(legend.position = c(.5, .96), legend.direction = "horizontal") + 
    annotate("text", x = c(.60, .60, .60), size = 3, fontface = 2, hjust = 1, 
             y = c(spray_angle_adj_TrueSingle_1quartile, spray_angle_adj_TrueSingle_median, spray_angle_adj_TrueSingle_3quartile), 
             label = c(spray_angle_adj_TrueSingle_1quartile, spray_angle_adj_TrueSingle_median, spray_angle_adj_TrueSingle_3quartile)) + 
    annotate("text", x = c(1.60,1.60, 1.60), size = 3, fontface = 2, hjust = 1, 
             y = c(spray_angle_adj_FalseSingle_1quartile, spray_angle_adj_FalseSingle_median, spray_angle_adj_FalseSingle_3quartile),
             label = c(spray_angle_adj_FalseSingle_1quartile, spray_angle_adj_FalseSingle_median, spray_angle_adj_FalseSingle_3quartile)) + 
    annotate("text", x = c(2.60, 2.60, 2.60), size = 3, fontface = 2, hjust = 1, 
             y = c(spray_angle_adj_TrueOut_1quartile, spray_angle_adj_TrueOut_median, spray_angle_adj_TrueOut_3quartile), 
             label = c(spray_angle_adj_TrueOut_1quartile, spray_angle_adj_TrueOut_median, spray_angle_adj_TrueOut_3quartile)) + 
    annotate("text", x = c(3.60, 3.60, 3.60), size = 3, fontface = 2, hjust = 1, 
             y = c(spray_angle_adj_FalseOut_1quartile, spray_angle_adj_FalseOut_median, spray_angle_adj_FalseOut_3quartile),
             label = c(spray_angle_adj_FalseOut_1quartile, spray_angle_adj_FalseOut_median, spray_angle_adj_FalseOut_3quartile)))

# Compare the densities of spray_angle_adj (Shifted If Alignment), using our population of the worst 
# 1,106 prediction mistakes and a random population of 1,106 correct predictions.

predictions_2212_shift_gb_TFOuts <- 
  dplyr::filter(predictions_2212, bb_type == "ground_ball" & if_fielding_alignment == "Infield shift")
predictions_2212_shift_gb_TFOuts <- predictions_2212_shift_gb_TFOuts %>% 
  dplyr::filter(adv_bb_type == "low_ground_ball" | adv_bb_type == "high_ground_ball")
predictions_2212_shift_gb_TFOuts <- predictions_2212_shift_gb_TFOuts %>% 
  dplyr::filter(eval_type == "TrueOut" | eval_type == "FalseOut")

x_scale <- scale_x_continuous(breaks = seq(-50, 50, by = 5), 
                              labels = c("-50", "-45", "-40", "-35", "-30", "-25", "-20", "-15", "-10", "-5",  "0", 
                                         "5", "10", "15", "20", "25", "30", "35", "40", "45", "50"), 
                              limits = c(-50, 50))
#y_scale <- scale_y_continuous(limits = c(0, 0.035))

(sa_adj_density <- ggplot(predictions_2212_shift_gb_TFOuts, 
                          aes(x = spray_angle_adj, y = stat(density), color = eval_type)) + 
    geom_freqpoly(binwidth = 2.5) + 
    x_scale + 
    #    y_scale + 
    scale_color_manual(name = "eval_type", labels = c("TrueOut", "FalseOut"), values = c("royalblue3", "red4")) + 
    labs(title = "Density Plots of spray_angle_adj by TrueOut and FalseOut 
         (Shifted If Alignment)", x = "spray_angle_adj (degrees)") + 
    theme(legend.position = c(.75, .80)))

# Compare, by hp_to_1b, our population of 2,533 mistaken predictions to a random population of 2,533 
# correct predictions.

predictions_5066_gb <- 
  dplyr::filter(predictions_5066, bb_type == "ground_ball" & adv_bb_type == "low_ground_ball")

truefalse_by_hp_to_1b <- ggplot(predictions_5066_gb, 
                                aes(x = eval_type, y = hp_to_1b, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.9) + 
  scale_y_continuous(breaks = seq(3.9, 5.1, by = 0.1), 
                       labels = c("3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5", "4.6", "4.7", "4.8",  "4.9", "5.0", "5.1"), 
                       limits = c(3.9, 5.1)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Predictions, by Home to First 
  on Low Ground Balls") + 
  theme(legend.position = c(.17, .92))

truefalse_by_hp_to_1b_data <- layer_data(truefalse_by_hp_to_1b)

hp_to_1b_TrueSingle_1quartile <- round(truefalse_by_hp_to_1b_data[1, 2], 2)
hp_to_1b_TrueSingle_median <- round(truefalse_by_hp_to_1b_data[1, 3], 2)
hp_to_1b_TrueSingle_3quartile <- round(truefalse_by_hp_to_1b_data[1, 4], 2)
hp_to_1b_FalseSingle_1quartile <- round(truefalse_by_hp_to_1b_data[2, 2], 2)
hp_to_1b_FalseSingle_median <- round(truefalse_by_hp_to_1b_data[2, 3], 2)
hp_to_1b_FalseSingle_3quartile <- round(truefalse_by_hp_to_1b_data[2, 4], 2)
hp_to_1b_TrueOut_1quartile <- round(truefalse_by_hp_to_1b_data[3, 2], 2)
hp_to_1b_TrueOut_median <- round(truefalse_by_hp_to_1b_data[3, 3], 2)
hp_to_1b_TrueOut_3quartile <- round(truefalse_by_hp_to_1b_data[3, 4], 2)
hp_to_1b_FalseOut_1quartile <- round(truefalse_by_hp_to_1b_data[4, 2], 2)
hp_to_1b_FalseOut_median <- round(truefalse_by_hp_to_1b_data[4, 3], 2)
hp_to_1b_FalseOut_3quartile <- round(truefalse_by_hp_to_1b_data[4, 4], 2)

(truefalse_by_hp_to_1b <- ggplot(predictions_5066_gb, 
                                 aes(x = eval_type, y = hp_to_1b, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.9) + 
  scale_y_continuous(breaks = seq(3.9, 5.1, by = 0.1), 
                     labels = c("3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5", "4.6", "4.7", "4.8",  "4.9", "5.0", "5.1"), 
                     limits = c(3.9, 5.1)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Predictions, by Home to First 
  on Low Ground Balls") + 
  theme(legend.position = c(.17, .92)) + 
  annotate("text", x = c(.60, .60, .60), size = 3, fontface = 2, hjust = 1, 
           y = c(hp_to_1b_TrueSingle_1quartile, hp_to_1b_TrueSingle_median, hp_to_1b_TrueSingle_3quartile), 
           label = c(hp_to_1b_TrueSingle_1quartile, hp_to_1b_TrueSingle_median, hp_to_1b_TrueSingle_3quartile)) + 
  annotate("text", x = c(1.60,1.60, 1.60), size = 3, fontface = 2, hjust = 1, 
           y = c(hp_to_1b_FalseSingle_1quartile, hp_to_1b_FalseSingle_median, hp_to_1b_FalseSingle_3quartile),
           label = c(hp_to_1b_FalseSingle_1quartile, hp_to_1b_FalseSingle_median, hp_to_1b_FalseSingle_3quartile)) + 
  annotate("text", x = c(2.60, 2.60, 2.60), size = 3, fontface = 2, hjust = 1, 
           y = c(hp_to_1b_TrueOut_1quartile, hp_to_1b_TrueOut_median, hp_to_1b_TrueOut_3quartile), 
           label = c(hp_to_1b_TrueOut_1quartile, hp_to_1b_TrueOut_median, hp_to_1b_TrueOut_3quartile)) + 
  annotate("text", x = c(3.60, 3.60, 3.60), size = 3, fontface = 2, hjust = 1, 
           y = c(hp_to_1b_FalseOut_1quartile, hp_to_1b_FalseOut_median, hp_to_1b_FalseOut_3quartile),
           label = c(hp_to_1b_FalseOut_1quartile, hp_to_1b_FalseOut_median, hp_to_1b_FalseOut_3quartile)))

# Compare, by hp_to_1b, our population of 1,106 mistaken predictions to a random population of 1,106 
# correct predictions.

predictions_2212_gb <- 
  dplyr::filter(predictions_2212, bb_type == "ground_ball" & adv_bb_type == "low_ground_ball")

truefalse_by_hp_to_1b <- ggplot(predictions_2212_gb, 
                                aes(x = eval_type, y = hp_to_1b, color = events)) + 
  geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
  geom_point(position = "jitter", alpha = .7, size = 0.9) + 
  scale_y_continuous(breaks = seq(3.9, 5.1, by = 0.1), 
                     labels = c("3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5", "4.6", "4.7", "4.8",  "4.9", "5.0", "5.1"), 
                     limits = c(3.9, 5.1)) + 
  scale_color_manual(values = c("Red", "Blue")) + 
  ggtitle("True vs False Predictions, by Home to First 
  on Low Ground Balls") + 
  theme(legend.position = c(.17, .92))

truefalse_by_hp_to_1b_data <- layer_data(truefalse_by_hp_to_1b)

hp_to_1b_TrueSingle_1quartile <- round(truefalse_by_hp_to_1b_data[1, 2], 2)
hp_to_1b_TrueSingle_median <- round(truefalse_by_hp_to_1b_data[1, 3], 2)
hp_to_1b_TrueSingle_3quartile <- round(truefalse_by_hp_to_1b_data[1, 4], 2)
hp_to_1b_FalseSingle_1quartile <- round(truefalse_by_hp_to_1b_data[2, 2], 2)
hp_to_1b_FalseSingle_median <- round(truefalse_by_hp_to_1b_data[2, 3], 2)
hp_to_1b_FalseSingle_3quartile <- round(truefalse_by_hp_to_1b_data[2, 4], 2)
hp_to_1b_TrueOut_1quartile <- round(truefalse_by_hp_to_1b_data[3, 2], 2)
hp_to_1b_TrueOut_median <- round(truefalse_by_hp_to_1b_data[3, 3], 2)
hp_to_1b_TrueOut_3quartile <- round(truefalse_by_hp_to_1b_data[3, 4], 2)
hp_to_1b_FalseOut_1quartile <- round(truefalse_by_hp_to_1b_data[4, 2], 2)
hp_to_1b_FalseOut_median <- round(truefalse_by_hp_to_1b_data[4, 3], 2)
hp_to_1b_FalseOut_3quartile <- round(truefalse_by_hp_to_1b_data[4, 4], 2)

(truefalse_by_hp_to_1b <- ggplot(predictions_2212_gb, 
                                 aes(x = eval_type, y = hp_to_1b, color = events)) + 
    geom_boxplot(color = "black", notch = TRUE, size = 1.1) + 
    geom_point(position = "jitter", alpha = .7, size = 0.9) + 
    scale_y_continuous(breaks = seq(3.9, 5.1, by = 0.1), 
                       labels = c("3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5", "4.6", "4.7", "4.8",  "4.9", "5.0", "5.1"), 
                       limits = c(3.9, 5.1)) + 
    scale_color_manual(values = c("Red", "Blue")) + 
    ggtitle("True vs False Predictions, by Home to First 
  on Low Ground Balls") + 
    theme(legend.position = c(.17, .92)) + 
    annotate("text", x = c(.60, .60, .60), size = 3, fontface = 2, hjust = 1, 
             y = c(hp_to_1b_TrueSingle_1quartile, hp_to_1b_TrueSingle_median, hp_to_1b_TrueSingle_3quartile), 
             label = c(hp_to_1b_TrueSingle_1quartile, hp_to_1b_TrueSingle_median, hp_to_1b_TrueSingle_3quartile)) + 
    annotate("text", x = c(1.60,1.60, 1.60), size = 3, fontface = 2, hjust = 1, 
             y = c(hp_to_1b_FalseSingle_1quartile, hp_to_1b_FalseSingle_median, hp_to_1b_FalseSingle_3quartile),
             label = c(hp_to_1b_FalseSingle_1quartile, hp_to_1b_FalseSingle_median, hp_to_1b_FalseSingle_3quartile)) + 
    annotate("text", x = c(2.60, 2.60, 2.60), size = 3, fontface = 2, hjust = 1, 
             y = c(hp_to_1b_TrueOut_1quartile, hp_to_1b_TrueOut_median, hp_to_1b_TrueOut_3quartile), 
             label = c(hp_to_1b_TrueOut_1quartile, hp_to_1b_TrueOut_median, hp_to_1b_TrueOut_3quartile)) + 
    annotate("text", x = c(3.60, 3.60, 3.60), size = 3, fontface = 2, hjust = 1, 
             y = c(hp_to_1b_FalseOut_1quartile, hp_to_1b_FalseOut_median, hp_to_1b_FalseOut_3quartile),
             label = c(hp_to_1b_FalseOut_1quartile, hp_to_1b_FalseOut_median, hp_to_1b_FalseOut_3quartile)))

# Compare the densities of Home to First using our population of the worst 
# 1,106 prediction mistakes and a random population of 1,106 correct predictions.

predictions_2212_lgb_TFOuts <-
  dplyr::filter(predictions_2212, bb_type == "ground_ball" & adv_bb_type == "low_ground_ball")
predictions_2212_lgb_TFOuts <- predictions_2212_lgb_TFOuts %>%
  dplyr::filter(eval_type == "TrueOut" | eval_type == "FalseOut")

x_scale <- scale_x_continuous(breaks = seq(3.9, 5.1, by = 0.1),
                              labels = c("3.9", "4.0", "4.1", "4.2", "4.3", "4.4", "4.5", "4.6", "4.7", "4.8",  "4.9", "5.0", "5.1"),
                              limits = c(3.9, 5.1))
y_scale <- scale_y_continuous()

(h1b_density <- ggplot(predictions_2212_lgb_TFOuts,
                       aes(x = hp_to_1b, y = stat(density), color = eval_type)) +
    geom_freqpoly(binwidth = 0.2) +
    x_scale +
    y_scale +
    scale_color_manual(name = "eval_type", labels = c("TrueOut", "FalseOut"), values = c("royalblue3", "red4")) +
    labs(title = "Density Plots of Home to First by TrueOut and FalseOut",
         x = "Home to First (seconds)") +
    theme(legend.position = c(.80, .80)))
