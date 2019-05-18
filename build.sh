VERSION=v1.0.8

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
