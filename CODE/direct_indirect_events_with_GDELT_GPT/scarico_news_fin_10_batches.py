import pandas as pd
from datetime import timedelta
import time
import re
from gdeltdoc import GdeltDoc, Filters, near, repeat, multi_repeat 
import numpy as np
import matplotlib

root = 'C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/direct_indirect_events_with_GDELT_GPT/'

# Function to split company names into multiple keywords
def split_company_keywords(company_name):
    # Use regex to separate the part before and within parentheses
    main_name = re.match(r"^[^(]+", company_name).group(0).strip()
    
    # Extract aliases within parentheses, if any
    aliases = re.findall(r"\(([^)]+)\)", company_name)
    
    if aliases:
        alias_list = [alias.strip() for alias in aliases[0].split(';')]
    else:
        alias_list = []

    # Combine main name with aliases into a single list of keywords
    keywords = [main_name] + alias_list
    return keywords

# Load the data from FIN.xlsx
fin = pd.read_excel(root + 'DATI/dset/FIN_EVENTS_no_rep_2.xlsx', sheet_name='FINANCIAL_SECTOR_EVENTS', dtype='object')

# Load the environment data
mapin = pd.read_excel(root + 'DATI/dset/environment.xlsx', sheet_name='dsetmap', dtype='object')

# Initialize success counter and other objects
success = 0
gd = GdeltDoc()
chunk_count = 0  # To keep track of chunk number

# Loop over each row in FIN.xlsx
for j in range(len(fin)):
    # Extract the company name and date from the current row
    company = fin['company'].values[j]
    date = pd.to_datetime(fin['date'].values[j])

    # Split the company name into keywords
    keywords = split_company_keywords(company)

    # Add words from fin['variables'] to keywords
    if 'variables' in fin.columns:
        # Extract and clean up the words from fin['variables']
        variables = fin['variables'].values[j]
        if pd.notna(variables):  # Ensure there are variables to add
            # Split the variables by commas and replace underscores with spaces
            var_keywords = [var.strip().replace('_', ' ') for var in variables.split(',')]
            # Add these keywords to the existing keywords list
            keywords.extend(var_keywords)

    # Define start_date and end_date based on the current date from FIN.xlsx
    start_date = (date - timedelta(days=1)).strftime('%Y-%m-%d')
    end_date = (date + timedelta(days=1)).strftime('%Y-%m-%d')

    # Loop over Identifiers in mapin
    for i, gkg in enumerate(mapin['Identifiers'].values):
        # Extract the theme for the current identifier
        themes = mapin['Themes'].values[i]

        # Set up the filters using the current company keywords and date range
        f = Filters(
            start_date=start_date,
            end_date=end_date,
            keyword=keywords,  # Using the split keywords from the company name and variables
            theme=gkg  # Using GKG codes from mapin
        )

        try:
            # Search for articles matching the filters
            news = gd.article_search(f)

        except KeyError:
            print(f'Fail: no GKG for {gkg}')

        else:
            success += 1
            print(f'{gkg} OK!')

            # Add identifier and theme to the news data
            news['ID'] = gkg
            news['THEME'] = themes
            news['j/95'] = j
            news['date'] = date
            news['company'] = company

            # Append news to the DataFrame
            if success == 1:
                DFnews = news
            else:
                DFnews = pd.concat([DFnews, news], axis=0)

        # Pause to respect API limits
        time.sleep(5)

    # Save the DataFrame every 10 iterations of 'j'
    if (j + 1) % 10 == 0:
        chunk_count += 1
        DFnews.to_excel(root + f'OUTPUT/batches/my_news_chunk_{chunk_count}.xlsx', index=False)
        print(f'Saved chunk {chunk_count} at iteration {j + 1}')

# Save the final dataset after the loop ends
DFnews.to_excel(root + 'OUTPUT/my_news_final.xlsx', index=False)
