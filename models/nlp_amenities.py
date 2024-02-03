import nltk
from nltk.tokenize import word_tokenize
from nltk.stem import WordNetLemmatizer
from nltk.corpus import stopwords

# Download necessary NLTK data (do this once)
nltk.download('punkt')
nltk.download('wordnet')
nltk.download('stopwords')

class Preprocessor:
    def __init__(self,amenities_list):
        self.lemmatizer = WordNetLemmatizer()
        self.amenities_list = amenities_list
        self.categories = {
            'Leisure': ['pool', 'grill', 'courtyard', 'clubhouse', 'lounge', 'spa'],
            'Technology': ['wi-fi', 'high speed internet access', 'electric car charging','wifi'],
            'Convenience': ['laundry facilities', 'dishwasher', 'maintenance', 'package service', 'valet trash removal'],
            'Fitness & Wellness': ['fitness center', 'gym', 'yoga', 'health club'],
            'Safety & Security': ['gated', 'controlled access', 'security', 'surveillance'],
            'Pet-Friendly': ['pet spa', 'pet play area', 'dog park']
        }


    def preprocess_and_deduplicate(self, items):
        unique = set()
        for item in items:
            tokens = word_tokenize(item.lower())
            lemmatized_tokens = [self.lemmatizer.lemmatize(token) for token in tokens if token not in stopwords.words('english')]
            unique.add(' '.join(lemmatized_tokens))
        return list(unique)

    def categorize_amenities(self, amenities):
        categorized = {category: [] for category in self.categories}  # Initialize categories
        for amenity in amenities:
            categorized_flag = False
            for category, keywords in self.categories.items():
                if any(keyword in amenity for keyword in keywords):
                    categorized[category].append(amenity)
                    categorized_flag = True
                    break  # Stop checking other categories if a match is found
            if not categorized_flag:
                # If the amenity doesn't fit any category, you might want to review it later
                categorized.setdefault('Uncategorized', []).append(amenity)
        return categorized
    
    def main(self):
        processed = self.preprocess_and_deduplicate(self.amenities_list)
        categorized = self.categorize_amenities(processed)
        print(categorized[''])


if __name__ == "__main__":

    test_list = [ 'double vanities ‘', 'entertaining clubhouse w/ kitchen', 'guest apartment', 'stackable washer & dryer rental', 'on-site storage', 'on site laundry', 'gourmet island kitchen', 'waterfront', 'built-in desk/shelving', 'pet-friendly apartments', 'hardwood styled flooring in select apartments', 'linen closets with built-in shelving', 'luxury vinyl plank flooring in sea coast', 'custom tile', 'spin room', 'wood style plank flooring', 'weekly happy hours', 'microwave in select units', 'nightpatrol', 'picturesque hill country views', 'full size pantries', 'european washer and dryers', 'garden bathtub in select homes', 'landscaped garden', 'wood-style plank flooring in living areas', 'on-site park', '60% amfi unit', 'granite counters in kitchen and bath', 'w/d included', 'the coffee shop', 'porch', 'health-focused air-filtration', 'cabanas', 'wood-style plank flooring throughout (select 1st f', 'handicap accessible', 'upgraded lighting fixtures', 'garden windows in selected units', 'maintenance on-site', 'shower / tub combo', 'custom-framed mirrors with specialty lighting', 'pet wash', '6 inch baseboards', 'day care center on-site', 'oversized rooms & closets', 'large windows with natural lighting', 'full-size top load washer and dryer', 'custom color accent walls available', 'skyline terrace with downtown views', 'dry cleaning services on-site', 'detached garages available', 'ceiling fan', '24 hour mw dry cleaning service', 'ut austin campus nearby', 'ground level', 'ceiling fans in living rooms & bedrooms', 'private screening room with stadium seating', 'no breed or weight restrictions', 'subway tile backsplash in kitchen', 'pet friendly with bark park', 'floor 13', 'indoor mail center', 'island kitchens with granite countertops', '2-inch faux wood window blinds', 'two designer finish packages with accent wall in living room', 'decorative subway tile backsplash', 'wood-style flooring and oversized windows offering sweeping views of austin', 'coworking spaces', 'cozy fireplaces', 'poolside grills & picnic area', 'community room', 'svc cable', 'brushed nickel light fixtures & hardware', 'monthly resident events', 'beautiful landscaped grounds with picnic areas', 'package room', 'package service', 'exclusive 5th street resident lounge', 'convenient location', 'open air fitness courtyard', 'tile floors', 'subway tile backsplash', 'built-in desks & bookcases', 'yards attached to select homes', 'first floor', 'sport court', 'rainfall shower heads', 'outdoor poolside kitchen', 'energy star front-load washer & dryer', '24-hour resident clubhouse access', 'poolside lounge with hdtvs and a fireplace', 'uniquely designed', 'pet wash station', 'access controlled entries and elevators', 'hardwood-style flooring', 'coworking space', 'ev charging stations', 'immediate access to downtown austin’s unique dining, trendy shopping and lively entertainment option', 'door-to-door trash pick-up & trash chutes', 'oversized chess board and social games', 'gas range', 'central location with freeway access', 'espresso cabinetry with stainless-steel pulls', 'abundant natural light throughout', 'open concept residences', 'covered poolside veranda', 'wine fridge in unit', 'austin energy green building', 'refrigerators with ice-makers', 'pet friendly', 'alarm ready', 'bbq grill/food prep area', 'xl patio/balcony', 'easy access to mass transit', 'garden tubs', 'balcony in select units', 'separate dining area', 'private patio or balcony', 'touchless water refill stations', 'large heated plunge spa', '7-level private parking garage', 'clubhouse lounge', 'recycling pick-up at your front door', 'minutes to downtown austin & austin nightlife', 'oval garden tubs', 'quartz countertops (deluxe homes)', 'upgraded 2- floorplans', 'bar & lounge', 'standard patio (38-54sq ft)', 'intrusion alarm systems', 'brushed nickel lighting', 'study rooms', 'double basin undermount sink with sprayer faucet', '4 electric car charging stations', 'secured parking garage', '3.0 miles to lady bird lake', 'resident lounge featuring seating and flat screens with attached kitchen', 'energy star appliances with french door refrigerators', '18-20 foot ceilings in select units', 'easy access to freeways, dining and shopping', 'spectrum wifi & cable', 'pet friendly, amazing staff', '24-hour to-go marketplace for food, beverage and convenience items', 'gorgeous lush landscaping', 'energy star® stainless steel appliance packages', 'single basin sink w/ gooseneck pull down faucet', 'gated bark park', 'dual-access closet', 'single basin kitchen sink', 'built in bookshelves ', 'grilling stations', 'resident portal- pay rent/request maint.', 'full size front loader washer and dryer', 'bike storage room', 'relaxing resort style pool', 'close to shopping', 'washer and dryer', 'one & two bedroom apartments & carriage homes', 'leed gold certified building', '2 resort-style pools and sundeck', 'smart unit', 'ten foot exposed concrete ceilings', 'entertainment room', 'no neighbors up or downstairs.', 'balcony, deck or patio', 'pool deck seating', '24 hour dry cleaning service', 'smart-home integration', 'easy access to freeways and shopping', 'three refreshing swimming pools', 'custom cabinetry with brushed nickel hardware', 'spanish speaking staff', 'private dining at cork', 'roof top', 'complimentary bikes available', '4th floor', 'energy-efficient stainless steel appliances', '42" hardwood kitchen cabinets', 'fenced backyards', 'walk-in showers with built-in bench', 'spa', 'custom tile backsplashes', 'farmhouse-style kitchen sinks', 'washer/dryer hookup', 'designer pendant lighting', 'octagon functional training', 'double vanity', 'dry bar, terrace, computer desk', 'walk in closets', 'washer/dryer in residence', 'faux hardwood or polished concrete flooring', 'bocce lawn', 'double vanity sinks ', 'high ceilings', 'energy star certified building', 'new/renovated interior', 'limited access gates', 'double vanity ', 'onsite storage available', 'washer/dryer hookups in every unit and stackable washer/dryer in select units', 'close proximity to local shops', 'shower - secondary bath', 'gold', 'convenient location to apple; dell; hp and samsung', 'national relocation program', 'painted accent walls', 'wood-style flooring (carpet in bedroom)']
    nlp = Preprocessor(test_list)
    nlp.main()