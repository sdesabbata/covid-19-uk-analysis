##########
# Get data
# Author: Stefano De Sabbata
# Date: 05 May 2020
# Licensed under the GNU General Public License v3.0 https://www.gnu.org/licenses/gpl-3.0.html
##########

download.file(
  "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv",
  "Data/Gov/coronavirus-cases_latest.csv"
)

download.file(
  "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-deaths_latest.csv",
  "Data/Gov/coronavirus-deaths_latest.csv"
)