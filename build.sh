VERSION=v1.0.3

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
