# Análisis exploratorio de Bitcoin y variables de mercado

Proyecto académico de introducción a Ciencia de Datos que analiza el comportamiento de Bitcoin en relación con activos financieros e indicadores macroeconómicos.

## Pregunta de análisis

¿Cómo se comporta Bitcoin frente a otros activos y variables de contexto económico, y qué relaciones exploratorias pueden observarse en los datos históricos?

## Flujo de trabajo

1. **Adquisición de datos con Python:** descarga de series históricas mediante yfinance.
2. **Preparación en R:** limpieza, homogeneización de fechas y unión de las distintas fuentes.
3. **Análisis exploratorio:** cálculo de retornos, variaciones y medidas de volatilidad.
4. **Comparación:** revisión de correlaciones entre Bitcoin, oro, petróleo, NASDAQ y S&P 500.
5. **Contexto macroeconómico:** incorporación de series de tasas, actividad y empleo disponibles en el proyecto.
6. **Visualización:** gráficos para comunicar tendencias, cambios y relaciones observadas.

## Tecnologías

- **Python:** Pandas y yfinance.
- **R:** tidyverse, modelr, reshape2 y scales.
- **Entorno:** Jupyter / Google Colab y R Markdown.
- **Fuentes:** datos financieros de Yahoo Finance y series macroeconómicas incluidas en el repositorio.

## Activos analizados

- Bitcoin
- Oro
- Petróleo WTI y Brent
- NASDAQ
- S&P 500

## Qué demuestra el proyecto

- Obtención de datos desde fuentes externas.
- Integración de series con frecuencias y fechas diferentes.
- Limpieza y transformación con Python y R.
- Análisis de retornos, volatilidad y correlaciones.
- Visualización y comunicación de hallazgos exploratorios.

## Reproducción

1. Revisar las rutas de entrada y salida de los notebooks para adaptarlas al entorno local.
2. Ejecutar primero la adquisición de datos en Python.
3. Instalar los paquetes de R enumerados en la sección Tecnologías.
4. Ejecutar la preparación, unión y análisis exploratorio en el orden documentado por los notebooks.

## Interpretación responsable

Este trabajo es exploratorio. Las correlaciones observadas no implican causalidad y los resultados no constituyen asesoramiento financiero. El valor del proyecto está en el proceso de adquisición, integración, análisis y comunicación de datos.
