REPO_DIR=${PWD}
echo "Run container"
docker run -d --rm -p 28787:8787 --name covid19ukanalysis -v $REPO_DIR:/home/rstudio/covid-19-uk-analysis sdesabbata/covid19ukanalysis