VERSION=v1.0.13

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
