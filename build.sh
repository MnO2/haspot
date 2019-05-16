VERSION=v1.0.2

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
