VERSION=v1.0.17

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
