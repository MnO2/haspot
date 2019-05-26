VERSION=v1.0.23

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
