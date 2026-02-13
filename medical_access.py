import requests
import pandas as pd
import time

url = "https://npiregistry.cms.hhs.gov/api"

zip_codes = []
for i in range(10451,10476):
    zip_codes.append(str(i))

print(type(zip_codes[0]))

all_results = []

for z in zip_codes:
    params = {
        "version": "2.1",
        "postal_code": z,
        "enumeration_type": "NPI-1",
        "country_code": "US",
        "limit": 200,
        "taxonomy"
    }
    
    response = requests.get(url, params=params)  
    data = response.json()
    if 'results' in data:
        df = pd.json_normalize(data['results'])
        all_results.append(df)

    time.sleep(0.2)

final_df = pd.concat(all_results, ignore_index=True)
final_df.drop_duplicates(subset=['npi'], inplace=True)
final_df.to_csv("/Users/wenggeiwong/pollen_mapping_data/bronx_healthcare_providers.csv")
