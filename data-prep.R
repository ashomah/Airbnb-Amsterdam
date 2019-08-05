#####
### THIS SCRIPT PREPARES THE AIRBNB LISTINGS DATASET
####


# Loading and merging files
listings_details <- read.csv('data_input/airbnb-amsterdam/listings_details.csv', sep=',')
listings <- read.csv('data_input/airbnb-amsterdam/listings.csv', sep=',')

listings_merged <- merge(x = listings, y = listings_details, by.x = 'id', by.y = 'id')
str(listings_merged)

# Selecting interesting columns
cols <- c('id','room_type.x','property_type','accommodates','bathrooms','bedrooms',
        'beds','bed_type','latitude.x','longitude.x','neighbourhood.x','is_location_exact',
        'host_id.x','host_name.x','host_response_time','host_response_rate','host_is_superhost',
        'host_total_listings_count','host_has_profile_pic','host_identity_verified','price.x',
        'weekly_price','monthly_price','security_deposit','cleaning_fee','guests_included',
        'extra_people','minimum_nights.x','maximum_nights','calendar_updated','has_availability',
        'availability_30','availability_60','availability_90','availability_365.x','instant_bookable',
        'is_business_travel_ready','cancellation_policy','require_guest_profile_picture',
        'require_guest_phone_verification','number_of_reviews.x','reviews_per_month.x','review_scores_rating',
        'review_scores_accuracy','review_scores_cleanliness','review_scores_checkin',
        'review_scores_communication','review_scores_location','review_scores_value')

clean_listing <- listings_merged[cols]
str(clean_listing)

# Cleaning data formats and values
library(stringr)
clean_listing$host_response_rate <- str_replace_all(clean_listing$host_response_rate, '%','')
clean_listing$host_response_rate <- str_replace_all(clean_listing$host_response_rate, 'N/A','NA')
clean_listing$host_response_rate <- as.numeric(clean_listing$host_response_rate)
clean_listing$host_response_rate <- clean_listing$host_response_rate/100

clean_listing$weekly_price <- as.numeric(clean_listing$weekly_price)
clean_listing$monthly_price <- as.numeric(clean_listing$monthly_price)
clean_listing$security_deposit <- as.numeric(clean_listing$security_deposit)
clean_listing$cleaning_fee <- as.numeric(clean_listing$cleaning_fee)
clean_listing$extra_people <- as.numeric(clean_listing$extra_people)

clean_listing$host_response_time <- str_replace_all(clean_listing$host_response_time, 'N/A','NA')
clean_listing$host_is_superhost[clean_listing$host_is_superhost == ""] <- 'f'
clean_listing$host_is_superhost <- factor(clean_listing$host_is_superhost)

str(clean_listing)

# Changing column names
names(clean_listing) <- c('home_id','room_type','property_type','accommodates','bathrooms','bedrooms',
                         'beds','bed_type','latitude','longitude','neighbourhood','is_location_exact',
                         'host_id','host_name','host_response_time','host_response_rate','host_is_superhost',
                         'host_total_listings_count','host_has_profile_pic','host_identity_verified','price',
                         'weekly_price','monthly_price','security_deposit','cleaning_fee','guests_included',
                         'extra_people','minimum_nights','maximum_nights','calendar_updated','has_availability',
                         'availability_30','availability_60','availability_90','availability_365','instant_bookable',
                         'is_business_travel_ready','cancellation_policy','require_guest_profile_picture',
                         'require_guest_phone_verification','number_of_reviews','reviews_per_month','review_scores_rating',
                         'review_scores_accuracy','review_scores_cleanliness','review_scores_checkin',
                         'review_scores_communication','review_scores_location','review_scores_value')

# Adding new feature: price for 2 people 2 nights
clean_listing$price_two_nights_two_people <- clean_listing$price * 2 + clean_listing$cleaning_fee + (clean_listing$guests_included-2 < 0)*abs(clean_listing$guests_included-2)*clean_listing$extra_people
head(clean_listing[, c('price', 'cleaning_fee', 'guests_included', 'extra_people', 'price_two_nights_two_people')],10)

# Reorganizing columns
col_order <- c('home_id', 'property_type', 'room_type', 'accommodates', 'bathrooms', 'bedrooms', 'beds', 'bed_type',
               'longitude', 'latitude', 'neighbourhood', 'is_location_exact',
               'host_id', 'host_name', 'host_is_superhost', 'host_response_time', 'host_response_rate',
               'host_total_listings_count','host_has_profile_pic','host_identity_verified',
               'price', 'weekly_price','monthly_price','security_deposit','cleaning_fee','guests_included', 'extra_people', 'price_two_nights_two_people',
               'minimum_nights','maximum_nights','calendar_updated','has_availability', 'availability_30','availability_60','availability_90','availability_365',
               'instant_bookable', 'is_business_travel_ready','cancellation_policy','require_guest_profile_picture', 'require_guest_phone_verification',
               'number_of_reviews','reviews_per_month','review_scores_rating', 'review_scores_accuracy','review_scores_cleanliness','review_scores_checkin', 'review_scores_communication','review_scores_location','review_scores_value')

listings_clean <- clean_listing[col_order]

# Saving CSV file
write.csv(listings_clean, 'data_output/listings_clean.csv')

# Imputing missing ratings
summary(listings_clean$review_scores_rating)
listings_clean$RATING <- round(listings_clean$review_scores_rating,0)
summary(listings_clean$RATING)
listings_clean[is.na(listings_clean$RATING),'RATING'] <- 20

# Saving CSV file
write.csv(listings_clean, 'data_output/listings_clean_ratings.csv')

