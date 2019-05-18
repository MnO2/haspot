VERSION=v1.0.7

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
