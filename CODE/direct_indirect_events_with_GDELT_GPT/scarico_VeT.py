# -*- coding: utf-8 -*-
"""
Created on Fri May 27 15:21:20 2022

@author: dimercuriom
"""
#SCARICARE MISURE DI VOLUME E TONO DEGLI IDENTIFIERS CATEGORIZZATI PER TEMI
from gdeltdoc import GdeltDoc, Filters, near, repeat, multi_repeat 
import pandas as pd
import numpy as np
import time
import matplotlib
import pickle

root = 'G:/CLIMATE RISK & ESG/PROGETTI/NLP/GDELT/'
mapin = pd.read_excel( root + 'DATI/dset/inputmap.xlsx', sheet_name='dsetmap', dtype='object')
#testate = ["cnn.com","washingtonpost.com","nytimes.com"] # una sola per volta
# init objects

success = 0
gd = GdeltDoc()

#for j da 0 a 95   
for i, gkg in enumerate(mapin['Identifiers'].values): 
    
    # gkg = gkg   
    
    # estrai il tema relativo allo i-esimo 
    
    themes = mapin['Themes'].values[i]
    #Theme = pd.DataFrame(theme)
 
    f = Filters(
        start_date = "2019-01-01",
        end_date = "2022-05-30",
        #timespan = "3d",
        # domain = testate,
        # num_records = 250,
        #country = "IT",
        keyword = ['china','chinese'], 
        theme  =  gkg, # GKG codes
        #repeat = multi_repeat([(3, "africa"),(2,'african')],'OR'),
        
    )

    
    try:
        gd.timeline_search("timelinevol", f)
    except KeyError:
        print('fail no gkg')
        
    else:
        
        success += 1
        print( gkg + " OK!")
        
        # Get a timeline of the number of articles matching the filters
        vol = gd.timeline_search("timelinevolraw", f)  
        # Get a sentiment of the number of articles matching the filters
        sentix = gd.timeline_search("timelinetone", f)
  
        vol['ID'] = gkg
        sentix['ID'] = gkg 
        
        vol['THEME'] = themes
        sentix ['THEME'] = themes
        
        if success == 1:
            DFvol    = vol
            DFsentix = sentix
         
        else:
            DFvol    = pd.concat( [DFvol, vol], axis=0 )
            DFsentix = pd.concat( [DFsentix, sentix], axis=0 )
        
    
        # rest API
        time.sleep(15)
#output.news
#time.sleep(15minuti)  
#concatenare in base a j
#output

      
#unisco in una sola matrice misure di volume e di tono     
#TOT= pd.concat([DFvol,DFsentix['Average Tone']], axis=1)  
# export
DFvol['datetime'] = DFvol['datetime'].dt.tz_localize(None)
DFsentix['datetime'] = DFsentix['datetime'].dt.tz_localize(None)

pd.DataFrame(DFvol).to_excel( root + 'OUTPUT/vol_KEY_chinese.xlsx', index=False)
pd.DataFrame(DFsentix).to_excel( root + 'OUTPUT/sentix_KEY_chinese.xlsx',index=False)

#salvare dati grezzi di volume e tono

pd.DataFrame(DFvol).to_pickle(root + 'OUTPUT/vol_KEY_chinese.pkl')
pd.DataFrame(DFsentix).to_pickle(root + 'OUTPUT/sentix_KEY_chinese.pkl')


'''
#provo a fare i grafici
DF_INPUT = pd.merge(DFsentix, DFvol, how='left', left_on=['datetime','ID','THEME'], right_on=['datetime','ID','THEME'])
DF_INPUT['CWT_ID'] = DF_INPUT['Average Tone'] * DF_INPUT['Article Count']
OUT_CWT_avg = DF_INPUT.groupby(['datetime','THEME']).mean('CWT_ID')
CWT_GEO_AVG = OUT_CWT_avg.groupby('datetime').mean('CWT_ID')

CWT_GEO_AVG_W = CWT_GEO_AVG['CWT_ID'].resample('W').mean()
CWTgraf_w = CWT_GEO_AVG_W.plot()
CWTgraf_g = CWT_GEO_AVG['CWT_ID'].plot()

# tono medio per ogni j-esimo tema
OUT_TON_AVG = DF_INPUT.groupby(['datetime','THEME']).mean('Average Tone')
# volume medio per ogni j-esimo tema
OUT_VOL_AVG = DF_INPUT.groupby(['datetime','THEME']).mean('Article Count')

TON_GEO_AVG = OUT_TON_AVG.groupby('datetime').mean('Average Tone')
VOL_GEO_SUM = OUT_VOL_AVG.groupby('datetime').sum('Article Count')
VOL_GEO_SUM_W = VOL_GEO_SUM['Article Count'].resample('W').sum()
TON_GEO_AVG_W = TON_GEO_AVG['Average Tone'].resample('W').mean()
VOLgraf_w= VOL_GEO_SUM_W.plot()
TONgraf_w= TON_GEO_AVG_W.plot()
'''