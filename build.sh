VERSION=v1.0.4

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
