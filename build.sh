VERSION=v1.0.15

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
