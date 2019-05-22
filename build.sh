VERSION=v1.0.21

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
