# -*- coding: utf-8 -*-
"""
Created on Thu Jun 30 12:02:18 2022

@author: dimercuriom
"""
#OBIETTIVO: CALCOLARE IL CWT CORRETTO, ELIMINANDO TEMI ED IDENTIFIERS FALSI AMICI
from gdeltdoc import GdeltDoc, Filters, near, repeat 
import pandas as pd
import time
import matplotlib.pyplot as plt

from scipy.stats import pearsonr


root = 'G:/CLIMATE RISK & ESG/PROGETTI/NLP/GDELT/'

# import data
#map_in = pd.read_excel( root + 'dset/inputmap.xlsx', sheet_name='dsetmap', dtype='object')
dset_ton = pd.read_pickle( root + 'OUTPUT/PAESI/sentix_OR_3_2_saudi_arabia.pkl')
dset_vol = pd.read_pickle( root + 'OUTPUT/PAESI/vol_OR_3_2_saudi_arabia.pkl')


# object
THEMES = sorted( list(set( dset_vol['THEME'].values ) ) )

# merge based on multple columns
DF_INPUT = pd.merge(dset_ton, dset_vol, how='left', left_on=['datetime','ID','THEME'], right_on=['datetime','ID','THEME'])

# calc CWT per identifier
DF_INPUT['CWT_ID'] = DF_INPUT['Average Tone'] * DF_INPUT['Article Count']

#ISOLARE DATI DI INTERESSE
CWT= DF_INPUT.iloc[:,[0,2,3,6]]

#ELIMINARE TEMA AIDGROUPS
Aid= CWT[CWT['THEME']== 'AIDGROUPS'].index
CWT.drop(Aid , inplace=True)

#ELIMINARE IDENTIFIERS
CRI_T03= CWT[CWT['ID']== 'CRISISLEX_T03_DEAD'].index
CWT.drop(CRI_T03,inplace=True)

CRI_T02= CWT[CWT['ID']== 'CRISISLEX_T02_INJURED'].index
CWT.drop(CRI_T02,inplace=True)

CRI_T09= CWT[CWT['ID']== 'CRISISLEX_T09_DISPLACEDRELOCATEDEVACUATED'].index
CWT.drop(CRI_T09,inplace=True)

CRI_REC= CWT[CWT['ID']== 'CRISISLEX_CRISISLEXREC'].index
CWT.drop(CRI_REC,inplace=True)

#CRI_C07= CWT[CWT['ID']== 'CRISISLEX_C07_SAFETY'].index
#CWT.drop(CRI_C07,inplace=True)


KILL= CWT[CWT['ID']== 'KILL'].index
CWT.drop(KILL,inplace=True)


#controllo su excel se ho eliminato bene le righe
#CWT['datetime'] = CWT['datetime'].dt.tz_localize(None)
#pd.DataFrame(CWT).to_excel( root + 'CWT_ID_BRAZIL_corr.xlsx', index=False)
 
# cwt medio per ogni j-esimo tema
OUT_CWT_avg = CWT.groupby(['datetime','THEME']).mean()
# narrative CWT calcolata come media dei CWT medi relativi ad ogni tema
CWT_GEO_AVG = CWT.groupby('datetime').mean('CWT_ID')

#indice medio mensile
CWT_GEO_AVG_M = CWT_GEO_AVG['CWT_ID'].resample('M', convention= 'start').mean()
#CWT_AVG_graf_m = CWT_GEO_AVG_M.plot()

Index=pd.DataFrame(CWT_GEO_AVG_M)
#Index['datetime'] = Index['datetime'].dt.tz_localize(None)
pd.DataFrame(Index).to_excel( root + 'SAUDI_ARABIA_corr_index.xlsx', index=False)
