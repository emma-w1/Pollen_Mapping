import requests
import pandas as pd
import time
import string

url = "https://npiregistry.cms.hhs.gov/api"

bronx_zip_codes = [str(i) for i in range(10451,10476)] # all bronx zip codes
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


def fetch_providers_by_name(taxonomy_codes, last_name_prefix):
    matched_records = []
    skip = 0
    limit = 200

    while True:
        params = {
            "version": "2.1",
            # "postal_code": zip,
            "city": "Bronx",
            "state": "NY",
            "enumeration_type": "NPI-1",
            "country_code": "US",
            "last_name": last_name_prefix + "*",
            "limit": 200,
            "skip": skip
        }


        try:
            response = requests.get(url, params=params, timeout=10)  
            print(f"DEBUG full url: {response.url}")
            response.raise_for_status()
            data = response.json()

        except Exception as e:
            print(f"Error for prefix {last_name_prefix}, skip {skip}: {e}")
            break
        

        results = data.get("results",[])
        if not results:
            break

        if skip == 0 and last_name_prefix == "A":
            import json
            print("DEBUG sample result:", json.dumps(results[0], indent=2))  # DEBUG

        page_matches = []
        for r in results:
            taxinomies = r.get("taxonomies",[])

            codes = set()
            for t in taxinomies:
                codes.add(t.get("code",""))
            
            if codes & taxonomy_codes: # checks for overlap 
                page_matches.append(r)
        
        matched_records.extend(page_matches)
        print(f"request for prefix {last_name_prefix}, skip {skip}: got {len(results)}, matched {len(page_matches)}, total matched: {len(matched_records)}")


        if len(results) < limit :
            break

        if(skip >=1000):
            print(f"**WARNING**: prefix={last_name_prefix} hit 1000+ skip, may be capped")
            break

        skip += limit
        time.sleep(0.2)
    
    return matched_records

def fetch_providers(zip_codes, taxonomy_codes):
    all = []
    seen = set()

    prefixes = [a + b for a in string.ascii_uppercase for b in string.ascii_uppercase]

    for prefix in prefixes:
        records = fetch_providers_by_name(taxonomy_codes, prefix)
        print(f"DEBUG prefix={prefix}: fetch_providers_by_name returned {len(records)} records")  # DEBUG
        for r in records:
            provider_zip = r.get("addresses", [{}])[0].get("postal_code", "")[:5]
            print(f"DEBUG provider_zip='{provider_zip}', in list: {provider_zip in zip_codes}")  # DEBUG
            if provider_zip not in zip_codes:
                continue
            npi = r.get("number")
            if npi not in seen:
                seen.add(npi)
                all.append(r)
        time.sleep(0.2)
    return all


# def fetch_all(zip_code, taxonomy_codes):
#     all = []

#     for i,z in enumerate(zip_code,1):

#         print(f"{i}/{len(zip_code)}: {z}")
#         records = fetch_providers(z, taxonomy_codes)
#         all.extend(records)
#         time.sleep(.2)
    
#     return all

def remove_duplicates(providers):
    seen = set()
    filtered = []

    for provider in providers:
        npi = provider.get("number")

        if npi in seen:
            continue
        else:
            seen.add(npi)
            filtered.append(provider)
    print(f"before filtering: {len(providers)}, after filtering: {len(filtered)}")
    return filtered

def save_results(filepath, filtered_providers):
    final_df = pd.json_normalize(filtered_providers)
    final_df.to_csv(filepath, index=False)

if __name__ == "__main__":
    physician_filepath = "/Users/wenggeiwong/Pollen_Mapping/physicians.csv"
    clinic_filepath = "/Users/wenggeiwong/Pollen_Mapping/clinics.csv"

    # all_physicians = fetch_providers(bronx_zip_codes,physician_taxonomy_codes)
    # filtered_physicians = remove_duplicates(all_physicians)

    all_clinics = fetch_providers(bronx_zip_codes, clinic_taxonomy_codes)
    filtered_clinics = remove_duplicates(all_clinics)

    # save_results(physician_filepath, filtered_physicians)
    save_results(clinic_filepath,filtered_clinics)


