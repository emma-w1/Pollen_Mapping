from sodapy import Socrata
import pandas as pd
import os


domain = "health.data.ny.gov"
token = os.getenv("OPENDATA_APP_TOKEN")
dataset_id = "vn5v-hh5r"
client = Socrata(domain,token)
brx_filepath = "bronx_facilities.csv"
manh_filepath = "manhattan_facilities.csv"

def dataset_nys(county):
    criteria = f"""
        county = '{county}'
        AND (
            fac_desc_short NOT IN (
                'NH',
                'ADHCP',
                'AH',
                'CHHA',
                'EHP',
                'LHCSA',
                'LTHHCP'
            )
            OR description = 'Mobile Diagnostic and Treatment Center Extension Clinic'
        )
    """
    results = client.get(dataset_id, where=criteria)
    df=pd.DataFrame.from_records(results)
    df=df[['fac_id','facility_name','county','description','address1','fac_zip','main_site_name','latitude','longitude','fac_phone','web_site']]
    return df

# def dataset_nppes(county):
    


def append_data(county):
    data1 = dataset_nys(county)
    data2 = dataset_nppes(county)
    df = pd.concat([data1,data2])
    return df

if __name__ == "__main__":
    bronx_data = append_data("Bronx")
    manh_data = append_data("New York")
    print("done!")