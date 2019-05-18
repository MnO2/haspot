VERSION=v1.0.5

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
