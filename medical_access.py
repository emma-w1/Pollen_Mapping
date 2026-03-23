import requests
import pandas as pd
import time

url = "https://npiregistry.cms.hhs.gov/api"

zip_codes = [i for i in range(10451,10476)] # all bronx zip codes
taxinomies = ["Family Medicine", "Pediatric", "Allergy","Immunology", "Urgent Care", "Primary Care"]
physician_taxonomy_codes = {
    "207K00000X", # allergy & immunology physician
    "207KA0200X", # allergy physician
    "207KI0005X", # clinical and laboratory immunology physician,
    "207Q00000X", # family medicine physician
    "207QA0000X", # family medicine adolescent physician
    "207QA0505X", # family medicine adult physician
    "208D00000X", # general practice physician
    "207R00000X", # internal medicine physician
    "207RA0000X", # adolescent internal medicine physician
    "207RA0201X", # allergy and immunology internal medicine physician
    "207RI0001X", # clinical and laboratory immunology internal medicine physician
    "208000000X", # pediatrics physician
    "2080A0000X", # adolescent pediatrics physician
    "2080I0007X", # pediatric clinical and laboratory immunology physician
    "2080P0201X", # pediatric allergy & immunology physician
    "2080P0204X", # pediatric emergency medicine physician                
}

# from https://taxonomy.nucc.org/
clinic_taxonomy_codes = {
    "261QP2300X", # primary care clinic
    "261QU0200X", # urgent care clinic
    "282N00000X", # general acute care hospital
    "282NC2000X", # children's hospital

    # below are more accessible: often funded by federal/local government so costs are lower
    "261QC1500X", # community health center 
    "261QE0002X", # emergency care center
    "261QF0400X", # federally qualified health center 
    "261QH0100X", # health service clinic
    "261QP0904X", # federal public health center
    "261QP0905X", # state/local public health center
}


def fetch_providers(zip, taxinomy_codes):
    matched_records = []
    skip = 0
    limit = 200

    while True:
        params = {
            "version": "2.1",
            "postal_code": zip,
            "enumeration_type": "NPI-1",
            "country_code": "US",
            "limit": 200,
            "skip": skip
        }

        try:
            response = requests.get(url, params=params, timeout=10)  
            response.raise_for_status
            data = response.json()

        except Exception as e:
            print(f"Error for zip code {zip}, skip {skip}: e")
            break
        
        results = data.get("results",[])

        page_matches = []
        for r in results:
            taxinomies = r.get("taxonomies",[])

            codes = set()
            for t in taxinomies:
                codes.add(t.get("code",""))
            
            if codes & taxinomy_codes: # checks for overlap 
                page_matches.append(r)
        
        matched_records.extend(page_matches)
        print(f"request for zip code {zip} & skip {skip}: got {len(results)}, matched {len(page_matches)}, total matched: {len(matched_records)}")

        if len(results) < limit:
            break

        skip += limit
        time.sleep(0.2)
    
    return matched_records




# final_df = pd.concat(all_results, ignore_index=True)
# final_df.drop_duplicates(subset=['npi'], inplace=True)
# final_df.to_csv("/Users/wenggeiwong/pollen_mapping_data/bronx_healthcare_providers.csv")
