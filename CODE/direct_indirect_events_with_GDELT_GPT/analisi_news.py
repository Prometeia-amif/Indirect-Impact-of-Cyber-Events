# -*- coding: utf-8 -*-
"""
Created on Tue May 31 13:06:55 2022

@author: dimercuriom
"""
#OBIETTIVO: scaricare le news ogni 31 minuti e testare i diversi filtri per lo scarico
from gdeltdoc import GdeltDoc, Filters, near, repeat, multi_repeat 
import pandas as pd
import numpy as np
import time
import matplotlib

root = 'C:/Users/ivan.delorenzo/OneDrive - PROMETEIA SPA/Desktop/Rep Risk/direct_indirect_events_with_GDELT_GPT/'
mapin = pd.read_excel( root + 'DATI/dset/environment.xlsx', sheet_name='dsetmap', dtype='object')
#testate = ["cnn.com","washingtonpost.com","nytimes.com"] # una sola per volta

 #selezionare 6 identifiers
# init objects
success = 0
gd = GdeltDoc()


for j in range(1):
    
 for i, gkg in enumerate(mapin['Identifiers'].values): 
    
   #gkg = gkg
    
    # estrai il tema relativo allo i-esimo 
    themes = mapin['Themes'].values[i]
   
 
    f = Filters(
        start_date = "2021-01-18",
        end_date = "2021-01-25",
        #timespan='31min',
        # domain = testate,
        # num_records = 250,
        #country = "IT",
        keyword = ['Banco de Sabadell', 'Banco Sabadell'], 
        theme  =  gkg, # GKG codes
        #repeat = repeat(5, "italy")
    )

    
    try:
        # Search for articles matching the filters
        gd.article_search(f)
       
    except KeyError:
        print('fail no gkg')
        
    else:
        
        success += 1
        print( gkg + " OK!")
        
        news = gd.article_search(f)
        news['ID'] = gkg
        
        
        news['THEME'] = themes
        news['j/95']= j
    
        
        if success == 1:
            DFnews= news
            
         
        else:
            DFnews   = pd.concat( [DFnews, news], axis=0 )
            
    
        # rest API
        time.sleep(20)
          
 #time.sleep(1860)
 
 
 
 #export
 pd.DataFrame(DFnews).to_excel( root + 'OUTPUT/my_news.xlsx', index=False)
 
 #output_news= DFnews 
 #output_news= pd.concat([output_news, DFnews], axis=0)   
 
 
 #output_news= DFnews 
 #output_news= pd.concat([output_news, DFnews], axis=0)                


#concatenare in base a j

#output
