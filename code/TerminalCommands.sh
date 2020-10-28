# build image
docker build -t jaredlander/plumber .
# run container
docker run --rm --name plumbing -p 8000:8000 jaredlander/plumber:latest
# use docker-compose
docker-compose up -d
# check contaniers
docker ps
# call API with CURL
curl -X GET --data @../one_row.json http://localhost:8000/predict2 -H "content-type: application/json"
