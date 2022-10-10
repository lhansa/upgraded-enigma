import requests
import pandas as pd
import seaborn as sns

url = 'https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/3994'
r = requests.get(url)

datos = r.json()

[d['Nombre'] for d in datos]

df_datos = pd.DataFrame()
for d in datos:
   df = pd.DataFrame.from_dict(d['Data'])
   df['nombre'] = d['Nombre']
   df_datos = df_datos.append(df)

df_datos.head()
titulo = 'Distribución porcentual de la población por sector económico. '
df_datos['nombre'] = df_datos['nombre'].str.replace(titulo, '')

df_datos[['region', 'sexo', 'sector', 'tipo', 'tipo2', 'cosa']] = df_datos['nombre'].str.split('.', expand=True)
df_datos = df_datos.drop('nombre', axis = 1)

# df_datos['region'] = df_datos['nombre'].str.extract('([A-Za-z ]+)\\.')
# df_datos['nombre'] = df_datos['nombre'].str.replace('^([A-Za-z ]+\\.\s+)', '')

# df_datos['sexo'] = df_datos['nombre'].str.extract('([A-Za-z ]+)\\.')



df_datos.sexo.drop_duplicates()


str(df_datos['Anyo'].values) + str(df_datos['FK_Periodo'].values)
df_datos['periodo'] = df_datos['Anyo'].astype(str) + df_datos['FK_Periodo'].astype(str)
df_datos = df_datos.drop(['Anyo', 'FK_Periodo'], axis = 1)


df_datos.head()

# sns.relplot(x='periodo', y='Valor', data=df_datos, col='sector', row='region')

df_datos['sector'].drop_duplicates()
sns.relplot(x='periodo', y='Valor', kind='line', 
            data=df_datos.query('sector == " Construcción"'), 
            col='region', col_wrap=2)