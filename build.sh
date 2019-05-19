VERSION=v1.0.11

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
